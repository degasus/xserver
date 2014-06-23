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

#include "glamor_priv.h"
#include "glamor_render_fallback.h"

void
glamor_trapezoids_fallback(CARD8 op,
                           PicturePtr src,
                           PicturePtr dst,
                           PictFormatPtr mask_format,
                           INT16 x_src, INT16 y_src,
                           int ntrap, xTrapezoid * traps)
{
    if (glamor_prepare_access_picture(dst, GLAMOR_ACCESS_RW) &&
        glamor_prepare_access_picture(src, GLAMOR_ACCESS_RO))
    {
        fbTrapezoids(op,
                     src, dst, mask_format,
                     x_src, y_src, ntrap, traps);
    }
    glamor_finish_access_picture(src);
    glamor_finish_access_picture(dst);
}

void
glamor_triangles_fallback(CARD8 op,
                          PicturePtr src,
                          PicturePtr dst,
                          PictFormatPtr mask_format,
                          INT16 x_src, INT16 y_src,
                          int ntri, xTriangle * tris)
{
    if (glamor_prepare_access_picture(dst, GLAMOR_ACCESS_RW) &&
        glamor_prepare_access_picture(src, GLAMOR_ACCESS_RO))
    {
        fbTriangles(op,
                    src, dst, mask_format,
                    x_src, y_src, ntri, tris);
    }
    glamor_finish_access_picture(src);
    glamor_finish_access_picture(dst);
}

void
glamor_add_traps_fallback(PicturePtr picture,
                          INT16 x_off, INT16 y_off,
                          int ntrap, xTrap * traps)
{
    if (glamor_prepare_access_picture(picture, GLAMOR_ACCESS_RW))
        fbAddTraps(picture, x_off, y_off, ntrap, traps);
    glamor_finish_access_picture(picture);
}
