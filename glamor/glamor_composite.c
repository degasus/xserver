/*
 * Copyright © 2014 Markus Wick
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 * Authors:
 *    Markus Wick <markus@selfnet.de>
 *
 */

#include "glamor_priv.h"

#ifdef RENDER
#include "mipict.h"
#include "fbpict.h"
#include <sys/stat.h>
#endif

static Bool
glamor_composite_verify_picture(PicturePtr pic,
                                glamor_composite_state_image* state,
                                Bool upload, Bool readonly)
{
    Bool ret = TRUE;
    state->pixmap = NULL;
    state->priv = NULL;
    state->has_alpha = TRUE;
    state->mask_rgba = FALSE;
    state->upload = GLAMOR_NONE;
    state->repeat = RepeatNone;

    if(!pic) { // input not available
        state->state = STATE_NONE;
        ret = readonly;

    } else if(!pic->pDrawable) { // filled
        switch(pic->pSourcePict->type) {
            case SourcePictTypeSolidFill:
                state->state = STATE_FILL_CONST;
                glamor_get_rgba_from_pixel
                    (pic->pSourcePict->solidFill.color,
                     &state->color[0], &state->color[1],
                     &state->color[2], &state->color[3], PICT_a8r8g8b8);
                ret = readonly;
                break;
            default:
                ErrorF("TODO: XRender fallback because of not implemented fill style %d\n", pic->pSourcePict->type);
                ret = FALSE;
                break;
        }

    } else { // common texture
        state->state = STATE_TEX;
        state->pixmap = glamor_get_drawable_pixmap(pic->pDrawable);
        state->priv = glamor_get_pixmap_private(state->pixmap);
        state->has_alpha = !!PICT_FORMAT_A(pic->format);
        state->mask_rgba = !!pic->componentAlpha;
        state->repeat = pic->repeatType;

        if (state->priv->base.gl_fbo == GLAMOR_FBO_UNATTACHED && upload) {
            state->upload = GLAMOR_UPLOAD_PENDING;
        } else if (!GLAMOR_PIXMAP_PRIV_HAS_FBO((state->priv))) {
            ErrorF("XRender fallback because of not reachable pixmap of type %d\n", state->priv->base.gl_fbo);
            return FALSE;
        }

        if (pic->alphaMap) {
            ErrorF("TODO: XRender fallback because of alpha map\n");
            return FALSE;
        }
        if (pic->transform) {
            ErrorF("TODO: XRender fallback because of image transform, keep care about filtering\n");
            return FALSE;
        }
    }
    return ret;
}

static Bool
glamor_composite_bind_program(ScreenPtr screen)
{
    glamor_screen_private *glamor_priv = glamor_get_screen_private(screen);
    GLuint fs, vs, prog;

    if (glamor_priv->composite.program) {
        glUseProgram(glamor_priv->composite.program);
        return TRUE;
    }

    vs = glamor_compile_glsl_prog(GL_VERTEX_SHADER,
        "#version 130\n"

        "in vec2 position;\n"

        "out vec2 source_position;\n"
        "out vec2 mask_position;\n"

        "uniform vec4 dest_transform;\n"

        "uniform vec4 source_transform;\n"

        "uniform vec4 mask_transform;\n"

        "#define TRANS(x, t) (((x) - t.xy) * t.zw)\n"

        "void main() {\n"
        "   gl_Position = vec4(TRANS(position, dest_transform) * 2.0 - 1.0, 0.0, 1.0);\n"
        "   source_position = TRANS(position, source_transform);\n"
        "   mask_position = TRANS(position, mask_transform);\n"
        "}\n"
    );
    fs = glamor_compile_glsl_prog(GL_FRAGMENT_SHADER,
        "#version 130\n"

        "in vec2 source_position;\n"
        "in vec2 mask_position;\n"

        "out vec4 out_color;\n"
        "out vec4 out_alpha;\n"

        "uniform sampler2D source_sampler;\n"
        "uniform int source_state;\n"
        "uniform vec4 source_color;\n"
        "uniform int source_has_alpha;\n"
        "uniform int source_repeat;\n"
        "uniform vec4 source_textransform;\n"

        "uniform sampler2D mask_sampler;\n"
        "uniform int mask_state;\n"
        "uniform vec4 mask_color;\n"
        "uniform int mask_has_alpha;\n"
        "uniform int mask_repeat;\n"
        "uniform vec4 mask_textransform;\n"

        "uniform int rgba_mask;\n"

        "bool clamped(vec2 v) {\n"
        "   return v.x >= 0.0 && v.x <= 1.0 && v.y >= 0.0 && v.y <= 1.0;\n"
        "}\n"

        "vec4 mysampler(int state, sampler2D sampler, vec2 position, vec4 color, int has_alpha, int repeat, vec4 textransform) {\n"
        "   vec4 c;\n"
        "   switch(state) {\n"
        "       case 0: return vec4(1.0, 1.0, 1.0, 1.0);\n"
        "       case 1:\n"
        "           switch(repeat) {\n"
        "               case 0: if(!clamped(position)) return vec4(0.0);\n"
        "               case 1: position = fract(position); break;\n"
        "               case 2: position = clamp(position, 0.0, 1.0); break;\n"
        "               case 3: position = 1.0 - abs(fract(position * 0.5) * 2.0 - 1.0); break;\n"
        "           }\n"
        "           position = position * textransform.xy + textransform.zw;\n"
        "           if(!clamped(position)) discard;\n"
        "           c = texture2D(sampler, position);\n"
        "           return vec4(c.rgb, has_alpha == 1 ? c.a : 1.0);\n"
        "       case 2: return color;\n"
        "   }\n"
        "   return vec4(1.0, 1.0, 1.0, 1.0);\n"
        "}\n"

        "void main() {\n"
        "   vec4 source = mysampler(source_state, source_sampler, source_position, source_color, source_has_alpha, source_repeat, source_textransform);\n"
        "   vec4 mask = mysampler(mask_state, mask_sampler, mask_position, mask_color, mask_has_alpha, mask_repeat, mask_textransform);\n"

        "   out_color = source * ( rgba_mask == 1 ? mask : vec4(mask.a) );\n"
        "   out_alpha = source.a * mask;\n"
        "}\n"
    );
    prog = glCreateProgram();
    glAttachShader(prog, vs);
    glAttachShader(prog, fs);
    glBindAttribLocation(prog, GLAMOR_VERTEX_POS, "position");
    glBindFragDataLocationIndexed(prog, 0, 0, "out_color");
    glBindFragDataLocationIndexed(prog, 0, 1, "out_alpha");
    glamor_link_glsl_prog(screen, prog, "composite");



#define GET_POS(a) glamor_priv->composite.source.a ## _pos = glGetUniformLocation(prog, "source_" #a); \
                   glamor_priv->composite.mask.a   ## _pos = glGetUniformLocation(prog, "mask_"   #a)
    GET_POS(sampler);
    GET_POS(state);
    GET_POS(color);
    GET_POS(transform);
    GET_POS(has_alpha);
    GET_POS(repeat);
    GET_POS(textransform);
#undef GET_POS

#define GET_POS(a) glamor_priv->composite.a ## _pos = glGetUniformLocation(prog, #a)
    GET_POS(dest_transform);
    GET_POS(rgba_mask);
#undef GET_POS

    glUseProgram(prog);
    glUniform1i(glamor_priv->composite.source.sampler_pos, 0);
    glUniform1i(glamor_priv->composite.mask.sampler_pos, 1);

    glamor_priv->composite.program = prog;
    return TRUE;
}

static void
glamor_composite_set_textures(ScreenPtr screen,
                              glamor_composite_state_image* state,
                              glamor_composite_private_image* priv,
                              int binding_point,
                              int box_x, int box_y,
                              int offset_x, int offset_y,
                              PicturePtr pic
                             )
{
    BoxPtr box;
    glamor_pixmap_fbo* tex;
    glUniform1i(priv->state_pos, state->state);

    switch(state->state) {
        case STATE_TEX:
            // We do have a pixmap, so bind it
            tex = glamor_pixmap_fbo_at(state->priv, box_x, box_y);
            glActiveTexture(GL_TEXTURE0 + binding_point);
            glBindTexture(GL_TEXTURE_2D, tex->tex);
            box = glamor_pixmap_box_at(state->priv, box_x, box_y);
            glUniform4f(priv->transform_pos,
                -offset_x, -offset_y, 1.0f / pic->pDrawable->width, 1.0f / pic->pDrawable->height);
            glUniform4f(priv->textransform_pos,
                1.0f * pic->pDrawable->width / (box->x2 - box->x1),
                1.0f * pic->pDrawable->height / (box->y2 - box->y1),
                -1.0f * (box->x1 - pic->pDrawable->x) / (box->x2 - box->x1),
                -1.0f * (box->y1 - pic->pDrawable->y) / (box->y2 - box->y1)
            );
            glUniform1i(priv->has_alpha_pos, state->has_alpha);
            glUniform1i(priv->repeat_pos, state->repeat);
            break;

        case STATE_FILL_CONST:
            glUniform4fv(priv->color_pos, 1, state->color);
            break;

        case STATE_NONE: { break; }
    }
}

static void
glamor_composite_bind_fbo(ScreenPtr screen,
                          glamor_composite_state_image* state,
                          int box_x, int box_y,
                          PicturePtr pic)
{
    glamor_screen_private *glamor_priv = glamor_get_screen_private(screen);
    BoxPtr box = glamor_pixmap_box_at(state->priv, box_x, box_y);

    glamor_set_destination_pixmap_fbo(
        glamor_pixmap_fbo_at(state->priv, box_x, box_y),
        0, 0, box->x2 - box->x1, box->y2 - box->y1);

    glUniform4f(glamor_priv->composite.dest_transform_pos,
                box->x1 - pic->pDrawable->x, box->y1 - pic->pDrawable->y,
                1.0f / (box->x2 - box->x1), 1.0f / (box->y2 - box->y1));
}

static Bool
glamor_composite_set_blend_state(ScreenPtr screen,
                                 CARD8 op,
                                 glamor_composite_state* state)
{
    GLenum src = GL_ONE;
    GLenum dst = GL_ZERO;
    Bool ret = TRUE;

    switch(op) {
        case PictOpClear:
            src = GL_ZERO;
            dst = GL_ZERO;
            break;
        case PictOpSrc:
            src = GL_ONE;
            dst = GL_ZERO;
            break;
        case PictOpDst:
            src = GL_ZERO;
            dst = GL_ONE;
            break;
        case PictOpOver:
            src = GL_ONE;
            if (state->mask.mask_rgba)
                dst = GL_ONE_MINUS_SRC1_COLOR;
            else
                dst = GL_ONE_MINUS_SRC_ALPHA;
            break;
        default:
            src = GL_ONE;
            dst = GL_ZERO;
            ret = FALSE;
            ErrorF("TODO: XRender fallback because of not implemented blend op %d\n", op);
            break;
    }

    glBlendFunc(src, dst);
    glEnable(GL_BLEND);

    return ret;
}

static Bool
glamor_composite_gl(CARD8 op,
                    PicturePtr source,
                    PicturePtr mask,
                    PicturePtr dest,
                    INT16 x_source, INT16 y_source,
                    INT16 x_mask, INT16 y_mask,
                    INT16 x_dest, INT16 y_dest,
                    CARD16 width, CARD16 height)
{
    ScreenPtr screen;
    glamor_screen_private *glamor_priv;
    glamor_composite_state state;
    GLshort* vertices;
    char* vbo_offset;
    int src_box_x, src_box_y;
    int mask_box_x, mask_box_y;
    int dst_box_x, dst_box_y;
    RegionRec region;
    BoxPtr box;
    int nbox, i;
    Bool ret = FALSE;

    if (!glamor_composite_verify_picture(source, &state.source, TRUE,  TRUE ) ||
        !glamor_composite_verify_picture(mask,   &state.mask,   TRUE,  TRUE ) ||
        !glamor_composite_verify_picture(dest,   &state.dest,   FALSE, FALSE))
    {
        // dest pixmap is unavailable for the gpu, so we have to fallback
        return FALSE;
    }

    if (state.dest.pixmap == state.source.pixmap || state.dest.pixmap == state.mask.pixmap) {
        ErrorF("TODO: XRender fallback because of maybe overlapping textures, make a copy\n");
        return FALSE;
    }

    if (!miComputeCompositeRegion(&region,
                                  source, mask, dest,
                                  x_source, y_source,
                                  x_mask, y_mask,
                                  x_dest, y_dest,
                                  width, height)) {
        // source + mask + dest doesn't overlap, so nothing to do
        return TRUE;
    }
    nbox = RegionNumRects(&region);
    box = RegionRects(&region);

    screen = dest->pDrawable->pScreen;
    glamor_priv = glamor_get_screen_private(screen);

    /* reset the state after here */

    glamor_make_current(glamor_priv);

    // upload pixmaps which are in memory
    if (state.source.upload == GLAMOR_UPLOAD_PENDING)
        if((state.source.upload = glamor_upload_picture_to_texture(source)) != GLAMOR_UPLOAD_DONE)
            goto bail;
    if (state.mask.upload == GLAMOR_UPLOAD_PENDING)
        if((state.mask.upload = glamor_upload_picture_to_texture(mask)) != GLAMOR_UPLOAD_DONE)
            goto bail;

    if (!glamor_composite_set_blend_state(screen, op, &state)) {
        // unsupported blending op.
        goto bail;
    }

    if (!glamor_composite_bind_program(screen)) {
        ErrorF("XRender fallback because of broken shader.\n");
        goto bail;
    }

    vertices = glamor_get_vbo_space(screen, nbox * 8 * sizeof(GLshort), &vbo_offset);
    glEnableVertexAttribArray(GLAMOR_VERTEX_POS);
    glVertexAttribPointer(GLAMOR_VERTEX_POS, 2, GL_SHORT, GL_FALSE, 0, vbo_offset);
    for (i = 0; i < nbox; i++) {
        vertices[6] = vertices[0] = box[i].x1;
        vertices[3] = vertices[1] = box[i].y1;
        vertices[4] = vertices[2] = box[i].x2;
        vertices[7] = vertices[5] = box[i].y2;
        vertices += 8;
    }
    glamor_put_vbo_space(screen);

    glUniform1i(glamor_priv->composite.rgba_mask_pos, state.mask.mask_rgba);

    // This function will also iterate over nullptr, so we don't have to check if this images exist
    glamor_pixmap_loop(state.dest.priv, dst_box_x, dst_box_y) {

        glamor_composite_bind_fbo(screen, &state.dest, dst_box_x, dst_box_y, dest);

        glamor_pixmap_loop(state.source.priv, src_box_x, src_box_y) {

            glamor_composite_set_textures(screen, &state.source, &glamor_priv->composite.source, 0,
                                          src_box_x, src_box_y,
                                          x_source - x_dest, y_source - y_dest, source
                                         );

            glamor_pixmap_loop(state.mask.priv, mask_box_x, mask_box_y) {

                glamor_composite_set_textures(screen, &state.mask, &glamor_priv->composite.mask, 1,
                                              mask_box_x, mask_box_y,
                                              x_mask - x_dest, y_mask - y_dest, mask
                                             );

                glDrawArrays(GL_QUADS, 0, 4 * nbox);
            }
        }
    }
    ret = TRUE;
bail:
    glDisable(GL_BLEND);
    glDisableVertexAttribArray(GLAMOR_VERTEX_POS);
    return ret;
}

static void
glamor_composite_bail(CARD8 op,
                 PicturePtr source,
                 PicturePtr mask,
                 PicturePtr dest,
                 INT16 x_source, INT16 y_source,
                 INT16 x_mask, INT16 y_mask,
                 INT16 x_dest, INT16 y_dest,
                 CARD16 width, CARD16 height)
{
    miCompositeSourceValidate(source);
    if (mask)
        miCompositeSourceValidate(mask);

    if (glamor_prepare_access_picture(dest, GLAMOR_ACCESS_RW) &&
        glamor_prepare_access_picture(source, GLAMOR_ACCESS_RO) &&
        glamor_prepare_access_picture(mask, GLAMOR_ACCESS_RO))
    {
        fbComposite(op,
                    source, mask, dest,
                    x_source, y_source,
                    x_mask, y_mask, x_dest, y_dest, width, height);
    }
    glamor_finish_access_picture(mask);
    glamor_finish_access_picture(source);
    glamor_finish_access_picture(dest);
}

void
glamor_composite(CARD8 op,
                 PicturePtr source,
                 PicturePtr mask,
                 PicturePtr dest,
                 INT16 x_source, INT16 y_source,
                 INT16 x_mask, INT16 y_mask,
                 INT16 x_dest, INT16 y_dest,
                 CARD16 width, CARD16 height)
{
    static int gl = 0;
    static int sw = 0;
    if (glamor_composite_gl(op, source, mask, dest,
                            x_source, y_source,
                            x_mask, y_mask,
                            x_dest, y_dest,
                            width, height))
    {
        gl++;
        return;
    }
    sw++;
    ErrorF("sw: composite %d op, %d source, %d mask, %d dest, sw %d, gl %d\n", op, !!source, !!mask, !!dest, sw, gl);

    // fallback to fb
    glamor_composite_bail(op, source, mask, dest,
                          x_source, y_source,
                          x_mask, y_mask,
                          x_dest, y_dest,
                          width, height);
}
