/*
 * Copyright © 2014 Keith Packard
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that copyright
 * notice and this permission notice appear in supporting documentation, and
 * that the name of the copyright holders not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  The copyright holders make no representations
 * about the suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 * OF THIS SOFTWARE.
 */

#ifndef _GLAMOR_RENDER_FALLBACK_H_
#define _GLAMOR_RENDER_FALLBACK_H_

#include "mipict.h"
#include "fbpict.h"

void
glamor_trapezoids_fallback(CARD8 op,
                           PicturePtr pSrc,
                           PicturePtr pDst,
                           PictFormatPtr maskFormat,
                           INT16 xSrc, INT16 ySrc,
                           int ntrap, xTrapezoid * traps);

#define glamor_trapezoids glamor_trapezoids_fallback

void
glamor_triangles_fallback(CARD8 op,
                          PicturePtr src,
                          PicturePtr dst,
                          PictFormatPtr mask_format,
                          INT16 x_src, INT16 y_src,
                          int ntri, xTriangle * tris);

#define glamor_triangles glamor_triangles_fallback

void
glamor_add_traps_fallback(PicturePtr picture,
                          INT16 x_off, INT16 y_off,
                          int ntrap, xTrap * traps);

#define glamor_add_traps glamor_add_traps_fallback

#define glamor_composite_rectangles miCompositeRects

void
glamor_glyphs_fallback(CARD8 op,
                       PicturePtr src,
                       PicturePtr dst,
                       PictFormatPtr mask_format,
                       INT16 x_src, INT16 y_src,
                       int nlist, GlyphListPtr list,
                       GlyphPtr * glyphs);


#define glamor_glyphs glamor_glyphs_fallback

void
glamor_glyph_unrealize_fallback(ScreenPtr screen,
                                GlyphPtr glyph);

#define glamor_glyph_unrealize glamor_glyph_unrealize_fallback

#define glamor_create_picture miCreatePicture

#define glamor_destroy_picture miDestroyPicture

#endif /* _GLAMOR_RENDER_FALLBACK_H_ */
