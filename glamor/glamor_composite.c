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

static const int SAMPLER_SOURCE = 0;
static const int SAMPLER_MASK = 1;

static Bool
glamor_composite_verify_picture(PicturePtr pic,
                                glamor_composite_state_image* state,
                                Bool destination)
{
    state->pixmap = NULL;
    state->priv = NULL;
    state->mask_rgba = FALSE;
    state->upload = GLAMOR_NONE;
    state->repeat = RepeatNone;
    state->transform = FALSE;
    state->fake_alpha = FALSE;
    state->pixmap_offset_x = 0;
    state->pixmap_offset_y = 0;

    // input not available
    if(!pic) {
        state->state = STATE_NONE;
        return !destination;

    // filled
    } else if(!pic->pDrawable) {
        if (destination)
            // constant pictures aren't supported as dest
            return FALSE;

        switch(pic->pSourcePict->type) {
            case SourcePictTypeSolidFill:
                state->state = STATE_FILL_CONST;
                glamor_get_rgba_from_pixel
                    (pic->pSourcePict->solidFill.color,
                     &state->color[0], &state->color[1],
                     &state->color[2], &state->color[3], PICT_a8r8g8b8);
                break;
            case SourcePictTypeLinear:
                if (pic->pSourcePict->linear.nstops > 4) {
                    ErrorF("TODO: XRender fallback: linear gradient only supports 4 stop points but %d requested.\n", pic->pSourcePict->linear.nstops);
                    return FALSE;
                }
                state->state = STATE_FILL_LINEAR;
                break;
            default:
                ErrorF("TODO: XRender fallback because of not implemented fill style %d\n", pic->pSourcePict->type);
                return FALSE;
        }

    // common texture
    } else {
        state->state = STATE_TEX;
        state->pixmap = glamor_get_drawable_pixmap(pic->pDrawable);
        state->priv = glamor_get_pixmap_private(state->pixmap);
        state->mask_rgba = !!pic->componentAlpha;
        state->repeat = pic->repeatType;
        state->transform = !!pic->transform;
        state->fake_alpha = !PICT_FORMAT_A(pic->format);
        state->pixmap_offset_x = pic->pDrawable->x;
        state->pixmap_offset_y = pic->pDrawable->y;



        if (pic->format != PICT_a8r8g8b8 && pic->format != PICT_x8r8g8b8 &&
            pic->format != PICT_a8)
        {
            ErrorF("TODO: XRender fallback because of incompatible type %x, use texture_view\n", pic->format);
            return FALSE;
        }

        if (pic->alphaMap) {
            ErrorF("TODO: XRender fallback because of alpha map\n");
            return FALSE;
        }

        if (!GLAMOR_PIXMAP_PRIV_HAS_FBO(state->priv)) {
            if (destination)
                return FALSE;
            if (glamor_upload_picture_to_texture(pic) != GLAMOR_UPLOAD_DONE) {
                ErrorF("XRender fallback because of not reachable pixmap of type %d\n", state->priv->type);
                return FALSE;
            }
        }
    }
    return TRUE;
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

        "out vec3 source_position;\n"
        "out vec3 mask_position;\n"

        "uniform vec4 dest_transform;\n"

        "uniform mat3 source_matrix;\n"
        "uniform vec2 source_offset;\n"

        "uniform mat3 mask_matrix;\n"
        "uniform vec2 mask_offset;\n"

        "vec2 TRANS(vec2 x, vec4 t) { return x * t.xy + t.zw; }\n"

        "void main() {\n"
        "   gl_Position = vec4(TRANS(position, dest_transform) * 2.0 - 1.0, 0.0, 1.0);\n"

            // xrender transform, mostly identity
        "   source_position = source_matrix * vec3(position + source_offset, 1.0);\n"
        "   mask_position = mask_matrix * vec3(position + mask_offset, 1.0);\n"
        "}\n"
    );
    fs = glamor_compile_glsl_prog(GL_FRAGMENT_SHADER,
        "#version 130\n"

        "in vec3 source_position;\n"
        "in vec3 mask_position;\n"

        "out vec4 out_color;\n"
        "out vec4 out_alpha;\n"

        "uniform sampler2D source_sampler;\n"
        "uniform int source_state;\n"
        "uniform vec4 source_color;\n"
        "uniform int source_repeat;\n"
        "uniform vec4 source_textransform;\n"
        "uniform vec4 source_transform;\n"
        "uniform int source_fake_alpha;\n"
        "uniform vec4 source_gradient_stops;\n"
        "uniform vec4 source_gradient_colors[4];\n"

        "uniform sampler2D mask_sampler;\n"
        "uniform int mask_state;\n"
        "uniform vec4 mask_color;\n"
        "uniform int mask_repeat;\n"
        "uniform vec4 mask_textransform;\n"
        "uniform vec4 mask_transform;\n"
        "uniform int mask_fake_alpha;\n"
        "uniform vec4 mask_gradient_stops;\n"
        "uniform vec4 mask_gradient_colors[4];\n"

        "uniform int rgba_mask;\n"

        // return true if 0 < v < 1
        "bool clamped(vec2 v) {\n"
        "   return v.x >= 0.0 && v.x < 1.0 && v.y >= 0.0 && v.y < 1.0;\n"
        "}\n"

        "vec2 TRANS(vec2 x, vec4 t) { return x * t.xy + t.zw; }\n"

        "vec4 mysampler(int state, sampler2D sampler, vec3 position_in, vec4 color, int repeat, vec4 textransform, vec4 transform, int fake_alpha, \n"
        "               vec4 gradient_stops, vec4 gradient_colors[4]) {\n"
        "   vec4 c;\n"
        "   float pos;\n"

            // needed for xrender transformation
            // must be done in the pixel shader as the division isn't linear
            // TODO: detect if the matrix scale at all / scale with a const factor
        "   vec2 position = position_in.xy / position_in.z;\n"

        "   switch(state) {\n"

                // not available
        "       case 0: return vec4(1.0, 1.0, 1.0, 1.0);\n"

                // common texture
        "       case 1:\n"

                    // transform to drawable coords
        "           position = TRANS(position, transform);\n"
        "           switch(repeat) {\n"

                        // repeat none: transparent if out of image
        "               case 0: if(!clamped(position)) return vec4(0.0);\n"

                        // repeat normal
        "               case 1: position = fract(position); break;\n"

                        // repeat pad: use the border color
        "               case 2: position = clamp(position, 0.0, 1.0); break;\n"

                        // repeat reflect
        "               case 3: position = 1.0 - abs(fract(position * 0.5) * 2.0 - 1.0); break;\n"
        "           }\n"

                    // transform to texture coords
        "           position = TRANS(position, textransform);\n"

                    // discard if our of texture (only happens with large textures)
        "           if(!clamped(position)) discard;\n"
        "           c = texture2D(sampler, position);\n"
        "           if (fake_alpha == 1) c.a = 1.0;\n"
        "           return c;\n"

                // const color
        "       case 2: return color;\n"

                // linear gradient_colors
        "       case 3:\n"
                    // transform into linear gradient space
        "           position = (position - transform.xy) * transform.zw;\n"
        "           pos = (position.x + position.y) / (transform.z * transform.z + transform.w * transform.w);\n"

        "           if(pos <= gradient_stops.x)\n"
        "               return gradient_colors[0];"
        "           if(pos <= gradient_stops.y)\n"
        "               return mix(gradient_colors[0], gradient_colors[1],\n"
        "                          (pos - gradient_stops.x) / (gradient_stops.y - gradient_stops.x));"
        "           if(pos <= gradient_stops.z)\n"
        "               return mix(gradient_colors[1], gradient_colors[2],\n"
        "                          (pos - gradient_stops.y) / (gradient_stops.z - gradient_stops.y));"
        "           if(pos <= gradient_stops.w)\n"
        "               return mix(gradient_colors[2], gradient_colors[3],\n"
        "                          (pos - gradient_stops.z) / (gradient_stops.w - gradient_stops.z));"
        "           return gradient_colors[3];"
        "   }\n"
        "   return vec4(1.0, 1.0, 1.0, 1.0);\n"
        "}\n"

        "void main() {\n"
        "   vec4 source = mysampler(source_state, source_sampler, source_position, source_color, source_repeat, source_textransform, source_transform, source_fake_alpha,\n"
        "                           source_gradient_stops, source_gradient_colors);\n"
        "   vec4 mask   = mysampler(mask_state, mask_sampler, mask_position, mask_color, mask_repeat, mask_textransform, mask_transform, mask_fake_alpha,\n"
        "                           mask_gradient_stops, mask_gradient_colors);\n"

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


    // fetch uniform positions
#define GET_POS(a) glamor_priv->composite.source.a ## _pos = glGetUniformLocation(prog, "source_" #a); \
                   glamor_priv->composite.mask.a   ## _pos = glGetUniformLocation(prog, "mask_"   #a)
    GET_POS(sampler);
    GET_POS(state);
    GET_POS(color);
    GET_POS(transform);
    GET_POS(repeat);
    GET_POS(textransform);
    GET_POS(matrix);
    GET_POS(offset);
    GET_POS(fake_alpha);
    GET_POS(gradient_stops);
    GET_POS(gradient_colors);
#undef GET_POS
#define GET_POS(a) glamor_priv->composite.a ## _pos = glGetUniformLocation(prog, #a)
    GET_POS(dest_transform);
    GET_POS(rgba_mask);
#undef GET_POS

    // bind sampler
    glUseProgram(prog);
    glUniform1i(glamor_priv->composite.source.sampler_pos, SAMPLER_SOURCE);
    glUniform1i(glamor_priv->composite.mask.sampler_pos, SAMPLER_MASK);

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
                              PicturePtr pic)
{
    BoxPtr box;
    glamor_pixmap_fbo* tex;
    GLenum filter;
    PictLinearGradient* linear;
    int i;
    float matrix[3][3] = {{1,0,0},{0,1,0},{0,0,1}};
    float gradient_colors[4][4] = {{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0}};
    float gradient_stops[4] = {0,0,0,0};

    glUniform1i(priv->state_pos, state->state);
    glUniform2f(priv->offset_pos, offset_x, offset_y);

    // Transformation matrix
    if (state->transform) {
        int x,y;
        for(x=0; x<3; x++)
            for(y=0; y<3; y++)
                matrix[x][y] = pic->transform->matrix[x][y] / 65536.0f;

        switch (pic->filter) {
            default:
            case PictFilterFast:
            case PictFilterNearest: filter = GL_NEAREST; break;
            case PictFilterGood:
            case PictFilterBest:
            case PictFilterBilinear: filter = GL_LINEAR; break;
        }
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, filter);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, filter);
    }
    glUniformMatrix3fv(priv->matrix_pos, 1, TRUE, matrix);

    switch(state->state) {
        case STATE_TEX:
            // We do have a pixmap, so bind it
            tex = glamor_pixmap_fbo_at(state->priv, box_x, box_y);
            glActiveTexture(GL_TEXTURE0 + binding_point);
            glBindTexture(GL_TEXTURE_2D, tex->tex);
            box = glamor_pixmap_box_at(state->priv, box_x, box_y);

            // transformation from pixels to drawable coords
            glUniform4f(priv->transform_pos,
                1.0f / pic->pDrawable->width,
                1.0f / pic->pDrawable->height,
                0, 0);

            // transformation from drawable coords to texture coords
            glUniform4f(priv->textransform_pos,
                (float)pic->pDrawable->width / (box->x2 - box->x1),
                (float)pic->pDrawable->height / (box->y2 - box->y1),
                -(float)(box->x1 - state->pixmap_offset_x) / (box->x2 - box->x1),
                -(float)(box->y1 - state->pixmap_offset_y) / (box->y2 - box->y1)
            );
            glUniform1i(priv->repeat_pos, state->repeat);
            glUniform1i(priv->fake_alpha_pos, state->fake_alpha);
            break;

        case STATE_FILL_CONST:
            glUniform4fv(priv->color_pos, 1, state->color);
            break;

        case STATE_FILL_LINEAR:
            linear = &pic->pSourcePict->linear;
            // Transform from pixel into linear gradient space
            glUniform4f(priv->transform_pos,
                        linear->p1.x / 65536.0f,
                        linear->p1.y / 65536.0f,
                        (linear->p2.x - linear->p1.x) / 65536.0f,
                        (linear->p2.y - linear->p1.y) / 65536.0f);

            for (i=0; i<4; i++) {
                if (linear->nstops > i) {
                    gradient_stops[i] = linear->stops[i].x / 65536.0f;
                    gradient_colors[i][0] = linear->stops[i].color.red / 65535.0f;
                    gradient_colors[i][1] = linear->stops[i].color.green / 65535.0f;
                    gradient_colors[i][2] = linear->stops[i].color.blue / 65535.0f;
                    gradient_colors[i][3] = linear->stops[i].color.alpha / 65535.0f;
                } else {
                    gradient_stops[i] = i>0 ? gradient_stops[i-1] : 0.0f;
                    gradient_colors[i][0] = i>0 ? gradient_colors[i-1][0] : 0.0f;
                    gradient_colors[i][1] = i>0 ? gradient_colors[i-1][1] : 0.0f;
                    gradient_colors[i][2] = i>0 ? gradient_colors[i-1][2] : 0.0f;
                    gradient_colors[i][3] = i>0 ? gradient_colors[i-1][3] : 0.0f;
                }
            }
            glUniform4fv(priv->gradient_stops_pos, 1, gradient_stops);
            glUniform4fv(priv->gradient_colors_pos, 4, gradient_colors);
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

    int off_x, off_y;
    glamor_get_drawable_deltas(pic->pDrawable, state->pixmap, &off_x, &off_y);

    glamor_set_destination_pixmap_fbo(
        glamor_pixmap_fbo_at(state->priv, box_x, box_y),
        0, 0, box->x2 - box->x1, box->y2 - box->y1);

    // transformation from pixels to texture coords
    // drawable coords aren't needed for the dest
    glUniform4f(glamor_priv->composite.dest_transform_pos,
                1.0f / (box->x2 - box->x1),
                1.0f / (box->y2 - box->y1),
                (float)(state->pixmap_offset_x + off_x - box->x1) / (box->x2 - box->x1),
                (float)(state->pixmap_offset_y + off_y - box->y1) / (box->y2 - box->y1));
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
        case PictOpDisjointClear:
        case PictOpConjointClear:
            src = GL_ZERO;
            dst = GL_ZERO;
            break;
        case PictOpSrc:
        case PictOpDisjointSrc:
        case PictOpConjointSrc:
            src = GL_ONE;
            dst = GL_ZERO;
            break;
        case PictOpDst:
        case PictOpDisjointDst:
        case PictOpConjointDst:
            src = GL_ZERO;
            dst = GL_ONE;
            break;
        case PictOpOver:
            src = GL_ONE;
            dst = GL_ONE_MINUS_SRC_ALPHA;
            break;
        case PictOpOverReverse:
            src = GL_ONE_MINUS_DST_ALPHA;
            dst = GL_ONE;
            break;
        case PictOpIn:
            src = GL_DST_ALPHA;
            dst = GL_ZERO;
            break;
        case PictOpInReverse:
            src = GL_ZERO;
            dst = GL_SRC_ALPHA;
            break;
        case PictOpOut:
            src = GL_ONE_MINUS_DST_ALPHA;
            dst = GL_ZERO;
            break;
        case PictOpOutReverse:
            src = GL_ZERO;
            dst = GL_ONE_MINUS_SRC_ALPHA;
            break;
        case PictOpAtop:
            src = GL_DST_ALPHA;
            dst = GL_ONE_MINUS_SRC_ALPHA;
            break;
        case PictOpAtopReverse:
            src = GL_ONE_MINUS_DST_ALPHA;
            dst = GL_SRC_ALPHA;
            break;
        case PictOpXor:
            src = GL_ONE_MINUS_DST_ALPHA;
            dst = GL_ONE_MINUS_SRC_ALPHA;
            break;
        case PictOpAdd:
            src = GL_ONE;
            dst = GL_ONE;
            break;

        default:
            src = GL_ONE;
            dst = GL_ZERO;
            ret = FALSE;
            ErrorF("TODO: XRender fallback because of not implemented blend op %d\n", op);
            break;
    }

    // We use full argb textures for xrgb and rgb pictures.
    // Not having alpha is always 1.0, so just fake it here:
    if (state->dest.fake_alpha && src == GL_DST_ALPHA) src = GL_ONE;
    if (state->dest.fake_alpha && src == GL_ONE_MINUS_DST_ALPHA) src = GL_ZERO;

    // With rgba masking, we have to use source.a * mask.rgba for blending.
    // As we want to write source.rgba * mask.rgba, we have to use an additional color output.
    if (state->mask.mask_rgba && dst == GL_SRC_ALPHA) dst = GL_SRC1_COLOR;
    if (state->mask.mask_rgba && dst == GL_ONE_MINUS_SRC_ALPHA) dst = GL_ONE_MINUS_SRC1_COLOR;

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

    // validates the pictures and parse the useful bits
    if (!glamor_composite_verify_picture(source, &state.source, FALSE) ||
        !glamor_composite_verify_picture(mask,   &state.mask,   FALSE) ||
        !glamor_composite_verify_picture(dest,   &state.dest,   TRUE ))
    {
        // dest pixmap is unavailable for the gpu, so we have to fallback
        return FALSE;
    }

    // Fallbacks which could be avoided
    if (state.dest.pixmap == state.source.pixmap || state.dest.pixmap == state.mask.pixmap) {
        ErrorF("TODO: XRender fallback because of maybe overlapping textures, make a copy\n");
        return FALSE;
    }

    // apply clipping and generate a Region for the not-clipped-area
    // WARNING: Through clipping vars are stored in the picture, they are in pixmap coords.
    //          So we have to apply the pixmap offset as clipping offset.
    //          Of course, the resulting region is also in dest pixmap coords.
    if (!miComputeCompositeRegion(&region,
                                  source, mask, dest,
                                  x_source + state.source.pixmap_offset_x,
                                  y_source + state.source.pixmap_offset_y,
                                  x_mask + state.mask.pixmap_offset_x,
                                  y_mask + state.mask.pixmap_offset_y,
                                  x_dest + state.dest.pixmap_offset_x,
                                  y_dest + state.dest.pixmap_offset_y,
                                  width, height)) {
        // source + mask + dest doesn't overlap, so nothing to do
        return TRUE;
    }
    nbox = RegionNumRects(&region);
    box = RegionRects(&region);

    /* reset the state after here */

    screen = dest->pDrawable->pScreen;
    glamor_priv = glamor_get_screen_private(screen);
    glamor_make_current(glamor_priv);

    // apply blending state
    if (!glamor_composite_set_blend_state(screen, op, &state)) {
        // unsupported blending op.
        goto bail;
    }

    // compile + bind program. TODO: split up the program in smaller ones
    if (!glamor_composite_bind_program(screen)) {
        ErrorF("XRender fallback because of broken shader.\n");
        goto bail;
    }

    // Upload dest rect as vertices
    vertices = glamor_get_vbo_space(screen, nbox * 8 * sizeof(GLshort), &vbo_offset);
    glEnableVertexAttribArray(GLAMOR_VERTEX_POS);
    glVertexAttribPointer(GLAMOR_VERTEX_POS, 2, GL_SHORT, GL_FALSE, 0, vbo_offset);
    for (i = 0; i < nbox; i++) {
        vertices[6] = vertices[0] = box[i].x1 - state.dest.pixmap_offset_x;
        vertices[3] = vertices[1] = box[i].y1 - state.dest.pixmap_offset_y;
        vertices[4] = vertices[2] = box[i].x2 - state.dest.pixmap_offset_x;
        vertices[7] = vertices[5] = box[i].y2 - state.dest.pixmap_offset_y;
        vertices += 8;
    }
    glamor_put_vbo_space(screen);

    glUniform1i(glamor_priv->composite.rgba_mask_pos, state.mask.mask_rgba);

    // Iterate over _all_ combinations of source, mask and dest textures.
    // This will be slow as hell for huge large_textures
    // TODO: skip textures which can't overlap. Keep care about transformation and repeating
    // Note: This function will also iterate over nullptr, so we don't have to check if this images exist
    glamor_pixmap_loop(state.dest.priv, dst_box_x, dst_box_y) {

        glamor_composite_bind_fbo(screen, &state.dest, dst_box_x, dst_box_y, dest);

        glamor_pixmap_loop(state.source.priv, src_box_x, src_box_y) {

            glamor_composite_set_textures(screen, &state.source, &glamor_priv->composite.source,
                                          SAMPLER_SOURCE, src_box_x, src_box_y,
                                          x_source - x_dest, y_source - y_dest, source);

            glamor_pixmap_loop(state.mask.priv, mask_box_x, mask_box_y) {

                glamor_composite_set_textures(screen, &state.mask, &glamor_priv->composite.mask,
                                              SAMPLER_MASK, mask_box_x, mask_box_y,
                                              x_mask - x_dest, y_mask - y_dest, mask);

                glDrawArrays(GL_QUADS, 0, 4 * nbox);
            }
        }
    }
    ret = TRUE;
bail:
    glDisable(GL_BLEND);
    glDisableVertexAttribArray(GLAMOR_VERTEX_POS);
    RegionUninit(&region);
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
