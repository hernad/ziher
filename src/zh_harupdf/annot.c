/*
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Ziher Project gives permission for
 * additional uses of the text contained in its release of Ziher.
 *
 * The exception is that, if you link the Ziher libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Ziher library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Ziher
 * Project under the name Ziher.  If you copy code from other
 * Ziher Project or Free Software Foundation releases into a copy of
 * Ziher, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Ziher, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "zh_harupdf.h"

/* HPDF_LinkAnnot_SetHighlightMode( hAnnot, nHilightMode ) --> hStatus
       nHilightMode ==
   HPDF_ANNOT_NO_HIGHTLIGHT       1     No highlighting.
   HPDF_ANNOT_INVERT_BOX          2     Invert the contents of the area of annotation.
   HPDF_ANNOT_INVERT_BORDER       3     Invert the annotation's border.
   HPDF_ANNOT_DOWN_APPEARANCE     4     Dent the annotation.
 */
ZH_FUNC( HPDF_LINKANNOT_SETHIGHLIGHTMODE )
{
   zh_retnl( ( long ) HPDF_LinkAnnot_SetHighlightMode( ( HPDF_Annotation ) zh_parptr( 1 ), ( HPDF_AnnotHighlightMode ) zh_parni( 2 ) ) );
}

/* HPDF_LinkAnnot_SetBorderStyle( hAnnot, nWidth, nDashOn, nDashOff ) --> hStatus */
ZH_FUNC( HPDF_LINKANNOT_SETBORDERSTYLE )
{
   zh_retnl( ( long ) HPDF_LinkAnnot_SetBorderStyle( ( HPDF_Annotation ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_UINT16 ) zh_parni( 3 ), ( HPDF_UINT16 ) zh_parni( 4 ) ) );
}

/* HPDF_TextAnnot_SetIcon( hAnnot, nIconID ) --> hStatus
       nIconID
   HPDF_ANNOT_ICON_COMMENT
   HPDF_ANNOT_ICON_KEY
   HPDF_ANNOT_ICON_NOTE
   HPDF_ANNOT_ICON_HELP
   HPDF_ANNOT_ICON_NEW_PARAGRAPH
   HPDF_ANNOT_ICON_PARAGRAPH
   HPDF_ANNOT_ICON_INSERT
 */
ZH_FUNC( HPDF_TEXTANNOT_SETICON )
{
   zh_retnl( ( long ) HPDF_TextAnnot_SetIcon( ( HPDF_Annotation ) zh_parptr( 1 ), ( HPDF_AnnotIcon ) zh_parni( 2 ) ) );
}

/* HPDF_TextAnnot_SetOpened( hAnnot, lOpened ) --> hStatus */
ZH_FUNC( HPDF_TEXTANNOT_SETOPENED )
{
   zh_retnl( ( long ) HPDF_TextAnnot_SetOpened( ( HPDF_Annotation ) zh_parptr( 1 ), zh_parl( 2 ) ? HPDF_TRUE : HPDF_FALSE ) );
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateFreeTextAnnot  (HPDF_Page       page,
                        HPDF_Rect       rect,
                        const char     *text,
                        HPDF_Encoder    encoder);
 */
ZH_FUNC( HPDF_PAGE_CREATEFREETEXTANNOT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

   zh_retptr( HPDF_Page_CreateFreeTextAnnot( ( HPDF_Page ) zh_parptr( 1 ), rc, zh_parc( 3 ), ( HPDF_Encoder ) zh_parptr( 4 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateLineAnnot  (HPDF_Page       page,
                     const char     *text,
                     HPDF_Encoder    encoder);
 */
ZH_FUNC( HPDF_PAGE_CREATELINEANNOT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   zh_retptr( HPDF_Page_CreateLineAnnot( ( HPDF_Page ) zh_parptr( 1 ), zh_parc( 2 ), ( HPDF_Encoder ) zh_parptr( 3 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateHighlightAnnot  (HPDF_Page   page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
 */
ZH_FUNC( HPDF_PAGE_CREATEHIGHLIGHTANNOT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

   zh_retptr( HPDF_Page_CreateHighlightAnnot( ( HPDF_Page ) zh_parptr( 1 ), rc, zh_parc( 3 ), ( HPDF_Encoder ) zh_parptr( 4 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateUnderlineAnnot (HPDF_Page    page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
 */
ZH_FUNC( HPDF_PAGE_CREATEUNDERLINEANNOT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

   zh_retptr( HPDF_Page_CreateUnderlineAnnot( ( HPDF_Page ) zh_parptr( 1 ), rc, zh_parc( 3 ), ( HPDF_Encoder ) zh_parptr( 4 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateSquigglyAnnot  (HPDF_Page    page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
 */
ZH_FUNC( HPDF_PAGE_CREATESQUIGGLYANNOT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

   zh_retptr( HPDF_Page_CreateSquigglyAnnot( ( HPDF_Page ) zh_parptr( 1 ), rc, zh_parc( 3 ), ( HPDF_Encoder ) zh_parptr( 4 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateStrikeOutAnnot  (HPDF_Page   page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
 */
ZH_FUNC( HPDF_PAGE_CREATESTRIKEOUTANNOT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

   zh_retptr( HPDF_Page_CreateStrikeOutAnnot( ( HPDF_Page ) zh_parptr( 1 ), rc, zh_parc( 3 ), ( HPDF_Encoder ) zh_parptr( 4 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreatePopupAnnot  ( HPDF_Page    page,
                        HPDF_Rect          rect,
                        HPDF_Annotation      parent);
 */
ZH_FUNC( HPDF_PAGE_CREATEPOPUPANNOT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

   zh_retptr( HPDF_Page_CreatePopupAnnot( ( HPDF_Page ) zh_parptr( 1 ), rc, ( HPDF_Annotation ) zh_parptr( 3 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateStampAnnot  (   HPDF_Page           page,
                        HPDF_Rect           rect,
                        HPDF_StampAnnotName name,
                        const char*         text,
                        HPDF_Encoder      encoder);
 */
ZH_FUNC( HPDF_PAGE_CREATESTAMPANNOT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

   zh_retptr( HPDF_Page_CreateStampAnnot( ( HPDF_Page ) zh_parptr( 1 ), rc, ( HPDF_StampAnnotName ) zh_parni( 3 ), zh_parc( 4 ), ( HPDF_Encoder ) zh_parptr( 5 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateSquareAnnot (HPDF_Page          page,
                      HPDF_Rect          rect,
                      const char         *text,
                      HPDF_Encoder       encoder);
 */
ZH_FUNC( HPDF_PAGE_CREATESQUAREANNOT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

   zh_retptr( HPDF_Page_CreateSquareAnnot( ( HPDF_Page ) zh_parptr( 1 ), rc, zh_parc( 3 ), ( HPDF_Encoder ) zh_parptr( 4 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateCircleAnnot (HPDF_Page          page,
                      HPDF_Rect          rect,
                      const char         *text,
                      HPDF_Encoder       encoder);
 */
ZH_FUNC( HPDF_PAGE_CREATECIRCLEANNOT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

   zh_retptr( HPDF_Page_CreateCircleAnnot( ( HPDF_Page ) zh_parptr( 1 ), rc, zh_parc( 3 ), ( HPDF_Encoder ) zh_parptr( 4 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_SetRGBColor (HPDF_Annotation annot, HPDF_RGBColor color);
 */
ZH_FUNC( HPDF_ANNOT_SETRGBCOLOR )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
   {
      HPDF_RGBColor rgb;

      rgb.r = ( HPDF_REAL ) zh_parvnd( 2, 1 );
      rgb.g = ( HPDF_REAL ) zh_parvnd( 2, 2 );
      rgb.b = ( HPDF_REAL ) zh_parvnd( 2, 3 );

      zh_retnl( ( long ) HPDF_Annot_SetRGBColor( annot, rgb ) );
   }
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_SetCMYKColor (HPDF_Annotation annot, HPDF_CMYKColor color);
 */
ZH_FUNC( HPDF_ANNOT_SETCMYKCOLOR )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
   {
      HPDF_CMYKColor cmyk;

      cmyk.c = ( HPDF_REAL ) zh_parvnd( 2, 1 );
      cmyk.m = ( HPDF_REAL ) zh_parvnd( 2, 2 );
      cmyk.y = ( HPDF_REAL ) zh_parvnd( 2, 3 );
      cmyk.k = ( HPDF_REAL ) zh_parvnd( 2, 4 );

      zh_retnl( ( long ) HPDF_Annot_SetCMYKColor( annot, cmyk ) );
   }
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_SetGrayColor (HPDF_Annotation annot, HPDF_REAL color);
 */
ZH_FUNC( HPDF_ANNOT_SETGRAYCOLOR )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_Annot_SetGrayColor( annot, ( HPDF_REAL ) zh_parnd( 2 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_SetNoColor (HPDF_Annotation annot);
 */
ZH_FUNC( HPDF_ANNOT_SETNOCOLOR )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_Annot_SetNoColor( annot ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetTitle (HPDF_Annotation annot, const char* name);
 */
ZH_FUNC( HPDF_MARKUPANNOT_SETTITLE )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_MarkupAnnot_SetTitle( annot, zh_parc( 2 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetSubject (HPDF_Annotation annot, const char* name);
 */
ZH_FUNC( HPDF_MARKUPANNOT_SETSUBJECT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_MarkupAnnot_SetSubject( annot, zh_parc( 2 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetCreationDate (HPDF_Annotation annot, HPDF_Date value);
 */
ZH_FUNC( HPDF_MARKUPANNOT_SETCREATIONDATE )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
   {
      HPDF_Date date;

      memset( &date, 0, sizeof( date ) );

      date.year    = zh_parvni( 2, 1 );
      date.month   = zh_parvni( 2, 2 );
      date.day     = zh_parvni( 2, 3 );
      date.hour    = zh_parvni( 2, 4 );
      date.minutes = zh_parvni( 2, 5 );
      date.seconds = zh_parvni( 2, 6 );
      date.ind     = ' ';

      zh_retnl( ( long ) HPDF_MarkupAnnot_SetCreationDate( annot, date ) );
   }
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetTransparency (HPDF_Annotation annot, HPDF_REAL value);
 */
ZH_FUNC( HPDF_MARKUPANNOT_SETTRANSPARENCY )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_MarkupAnnot_SetTransparency( annot, ( HPDF_REAL ) zh_parnd( 2 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetIntent (HPDF_Annotation  annot, HPDF_AnnotIntent  intent);
 */
ZH_FUNC( HPDF_MARKUPANNOT_SETINTENT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_MarkupAnnot_SetIntent( annot, ( HPDF_AnnotIntent ) zh_parni( 2 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetPopup (HPDF_Annotation annot, HPDF_Annotation popup);
 */
ZH_FUNC( HPDF_MARKUPANNOT_SETPOPUP )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_MarkupAnnot_SetPopup( annot, ( HPDF_Annotation ) zh_parptr( 2 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetRectDiff (HPDF_Annotation  annot, HPDF_Rect  rect);
 */
ZH_FUNC( HPDF_MARKUPANNOT_SETRECTDIFF )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
   {
      HPDF_Rect rc;

      rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
      rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
      rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
      rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

      zh_retnl( ( long ) HPDF_MarkupAnnot_SetRectDiff( annot, rc ) );
   }
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetCloudEffect (HPDF_Annotation  annot, HPDF_INT cloudIntensity);
 */
ZH_FUNC( HPDF_MARKUPANNOT_SETCLOUDEFFECT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_MarkupAnnot_SetCloudEffect( annot, ( HPDF_INT ) zh_parni( 2 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetInteriorRGBColor (HPDF_Annotation  annot, HPDF_RGBColor color);
 */
ZH_FUNC( HPDF_MARKUPANNOT_SETINTERIORRGBCOLOR )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
   {
      HPDF_RGBColor rgb;

      rgb.r = ( HPDF_REAL ) zh_parvnd( 2, 1 );
      rgb.g = ( HPDF_REAL ) zh_parvnd( 2, 2 );
      rgb.b = ( HPDF_REAL ) zh_parvnd( 2, 3 );

      zh_retnl( ( long ) HPDF_MarkupAnnot_SetInteriorRGBColor( annot, rgb ) );
   }
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetInteriorCMYKColor (HPDF_Annotation  annot, HPDF_CMYKColor color);
 */
ZH_FUNC( HPDF_MARKUPANNOT_SETINTERIORCMYKCOLOR )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
   {
      HPDF_CMYKColor cmyk;

      cmyk.c = ( HPDF_REAL ) zh_parvnd( 2, 1 );
      cmyk.m = ( HPDF_REAL ) zh_parvnd( 2, 2 );
      cmyk.y = ( HPDF_REAL ) zh_parvnd( 2, 3 );
      cmyk.k = ( HPDF_REAL ) zh_parvnd( 2, 4 );

      zh_retnl( ( long ) HPDF_MarkupAnnot_SetInteriorCMYKColor( annot, cmyk ) );
   }
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetInteriorGrayColor (HPDF_Annotation  annot, HPDF_REAL color);
 */
ZH_FUNC( HPDF_MARKUPANNOT_SETINTERIORGRAYCOLOR )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_MarkupAnnot_SetInteriorGrayColor( annot, ( HPDF_REAL ) zh_parnd( 2 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetInteriorTransparent (HPDF_Annotation  annot);
 */
ZH_FUNC( HPDF_MARKUPANNOT_SETINTERIORTRANSPARENT )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_MarkupAnnot_SetInteriorTransparent( annot ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_TextMarkupAnnot_SetQuadPoints ( HPDF_Annotation annot, HPDF_Point lb, HPDF_Point rb, HPDF_Point rt, HPDF_Point lt);
 */
ZH_FUNC( HPDF_TEXTMARKUPANNOT_SETQUADPOINTS )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
   {
      HPDF_Point p1;
      HPDF_Point p2;
      HPDF_Point p3;
      HPDF_Point p4;

      p1.x = ( HPDF_REAL ) zh_parvnd( 2, 1 );
      p1.y = ( HPDF_REAL ) zh_parvnd( 2, 2 );

      p2.x = ( HPDF_REAL ) zh_parvnd( 3, 1 );
      p2.y = ( HPDF_REAL ) zh_parvnd( 3, 2 );

      p3.x = ( HPDF_REAL ) zh_parvnd( 4, 1 );
      p3.y = ( HPDF_REAL ) zh_parvnd( 4, 2 );

      p4.x = ( HPDF_REAL ) zh_parvnd( 5, 1 );
      p4.y = ( HPDF_REAL ) zh_parvnd( 5, 2 );

      zh_retnl( ( long ) HPDF_TextMarkupAnnot_SetQuadPoints( annot, p1, p2, p3, p4 ) );
   }
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_Set3DView  ( HPDF_MMgr mmgr,
                   HPDF_Annotation   annot,
                   HPDF_Annotation   annot3d,
                   HPDF_Dict         view);
 */
ZH_FUNC( HPDF_ANNOT_SET3DVIEW )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_MMgr mmgr = ( HPDF_MMgr ) zh_parptr( 1 );

   if( mmgr )
      zh_retnl( ( long ) HPDF_Annot_Set3DView( mmgr, ( HPDF_Annotation ) zh_parptr( 2 ), ( HPDF_Annotation ) zh_parptr( 3 ), ( HPDF_Dict ) zh_parptr( 4 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_PopupAnnot_SetOpened  (HPDF_Annotation   annot,
                            HPDF_BOOL         opened);
 */
ZH_FUNC( HPDF_POPUPANNOT_SETOPENED )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   zh_retnl( ( long ) HPDF_PopupAnnot_SetOpened( ( HPDF_Annotation ) zh_parptr( 1 ), ( HPDF_BOOL ) zh_parl( 2 ) ) );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_FreeTextAnnot_SetLineEndingStyle (HPDF_Annotation annot, HPDF_LineAnnotEndingStyle startStyle, HPDF_LineAnnotEndingStyle endStyle);
 */
ZH_FUNC( HPDF_FREETEXTANNOT_SETLINEENDINGSTYLE )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_FreeTextAnnot_SetLineEndingStyle( annot, ( HPDF_LineAnnotEndingStyle ) zh_parni( 2 ), ( HPDF_LineAnnotEndingStyle ) zh_parni( 3 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_FreeTextAnnot_Set3PointCalloutLine (HPDF_Annotation annot, HPDF_Point startPoint, HPDF_Point kneePoint, HPDF_Point endPoint);
 */
ZH_FUNC( HPDF_FREETEXTANNOT_SET3POINTCALLOUTLINE )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
   {
      HPDF_Point p1;
      HPDF_Point p2;
      HPDF_Point p3;

      p1.x = ( HPDF_REAL ) zh_parvnd( 2, 1 );
      p1.y = ( HPDF_REAL ) zh_parvnd( 2, 2 );

      p2.x = ( HPDF_REAL ) zh_parvnd( 3, 1 );
      p2.y = ( HPDF_REAL ) zh_parvnd( 3, 2 );

      p3.x = ( HPDF_REAL ) zh_parvnd( 4, 1 );
      p3.y = ( HPDF_REAL ) zh_parvnd( 4, 2 );

      zh_retnl( ( long ) HPDF_FreeTextAnnot_Set3PointCalloutLine( annot, p1, p2, p3 ) );
   }
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_FreeTextAnnot_Set2PointCalloutLine (HPDF_Annotation annot, HPDF_Point startPoint, HPDF_Point endPoint);
 */
ZH_FUNC( HPDF_FREETEXTANNOT_SET2POINTCALLOUTLINE )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
   {
      HPDF_Point p1;
      HPDF_Point p2;

      p1.x = ( HPDF_REAL ) zh_parvnd( 2, 1 );
      p1.y = ( HPDF_REAL ) zh_parvnd( 2, 2 );

      p2.x = ( HPDF_REAL ) zh_parvnd( 3, 1 );
      p2.y = ( HPDF_REAL ) zh_parvnd( 3, 2 );

      zh_retnl( ( long ) HPDF_FreeTextAnnot_Set2PointCalloutLine( annot, p1, p2 ) );
   }
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_FreeTextAnnot_SetDefaultStyle (HPDF_Annotation  annot, const char* style);
 */
ZH_FUNC( HPDF_FREETEXTANNOT_SETDEFAULTSTYLE )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_FreeTextAnnot_SetDefaultStyle( annot, zh_parc( 2 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_LineAnnot_SetPosition (HPDF_Annotation annot,
                     HPDF_Point startPoint, HPDF_LineAnnotEndingStyle startStyle,
                     HPDF_Point endPoint, HPDF_LineAnnotEndingStyle endStyle);
 */
ZH_FUNC( HPDF_LINEANNOT_SETPOSITION )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
   {
      HPDF_Point p1;
      HPDF_Point p2;

      p1.x = ( HPDF_REAL ) zh_parvnd( 2, 1 );
      p1.y = ( HPDF_REAL ) zh_parvnd( 2, 2 );

      p2.x = ( HPDF_REAL ) zh_parvnd( 4, 1 );
      p2.y = ( HPDF_REAL ) zh_parvnd( 4, 2 );

      zh_retnl( ( long ) HPDF_LineAnnot_SetPosition( annot, p1, ( HPDF_LineAnnotEndingStyle ) zh_parni( 3 ), p2, ( HPDF_LineAnnotEndingStyle ) zh_parni( 5 ) ) );
   }
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_LineAnnot_SetLeader (HPDF_Annotation annot, HPDF_INT leaderLen, HPDF_INT leaderExtLen, HPDF_INT leaderOffsetLen);
 */
ZH_FUNC( HPDF_LINEANNOT_SETLEADER )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_LineAnnot_SetLeader( annot, zh_parni( 2 ), zh_parni( 3 ), zh_parni( 4 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_LineAnnot_SetCaption (HPDF_Annotation annot, HPDF_BOOL showCaption, HPDF_LineAnnotCapPosition position, HPDF_INT horzOffset, HPDF_INT vertOffset);
 */
ZH_FUNC( HPDF_LINEANNOT_SETCAPTION )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_LineAnnot_SetCaption( annot, zh_parl( 2 ), ( HPDF_LineAnnotCapPosition ) zh_parni( 3 ), zh_parni( 4 ), zh_parni( 5 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annotation_SetBorderStyle  (HPDF_Annotation  annot,
                                 HPDF_BSSubtype   subtype,
                                 HPDF_REAL        width,
                                 HPDF_UINT16      dash_on,
                                 HPDF_UINT16      dash_off,
                                 HPDF_UINT16      dash_phase);
 */
ZH_FUNC( HPDF_ANNOTATION_SETBORDERSTYLE )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Annotation annot = ( HPDF_Annotation ) zh_parptr( 1 );

   if( annot )
      zh_retnl( ( long ) HPDF_Annotation_SetBorderStyle( annot, ( HPDF_BSSubtype ) zh_parni( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_UINT16 ) zh_parni( 4 ), ( HPDF_UINT16 ) zh_parni( 5 ), ( HPDF_UINT16 ) zh_parni( 6 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}
