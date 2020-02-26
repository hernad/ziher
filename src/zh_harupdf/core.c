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

static ZH_GARBAGE_FUNC( zh_HPDF_Doc_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      HPDF_Free( ( HPDF_Doc ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

static const ZH_GC_FUNCS s_gcHPDF_DocFuncs =
{
   zh_HPDF_Doc_release,
   zh_gcDummyMark
};

HPDF_Doc zh_HPDF_Doc_par( int iParam )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcHPDF_DocFuncs, iParam );

   return ph ? ( HPDF_Doc ) * ph : NULL;
}

/* Most of the functions return hStatus == HPDF_OK or ERROR Code */

/* HPDF_New() --> hDoc */
ZH_FUNC( HPDF_NEW )
{
   void ** ph = ( void ** ) zh_gcAllocate( sizeof( HPDF_Doc ), &s_gcHPDF_DocFuncs );

   *ph = ( void * ) HPDF_New( NULL, NULL );

   zh_retptrGC( ph );
}

/* HPDF_Free( hDoc ) --> NIL */
ZH_FUNC( HPDF_FREE )
{
   void ** ph = ( void ** ) zh_parptrGC( &s_gcHPDF_DocFuncs, 1 );

   if( ph && *ph )
   {
      /* Destroy the object */
      HPDF_Free( ( HPDF_Doc ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

/* HPDF_NewDoc( hDoc ) --> hStatus */
ZH_FUNC( HPDF_NEWDOC )
{
   zh_retnl( ( long ) HPDF_NewDoc( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_FreeDoc( hNewDoc ) --> NIL */
ZH_FUNC( HPDF_FREEDOC )
{
   HPDF_FreeDoc( zh_HPDF_Doc_par( 1 ) );
}

/* HPDF_FreeDocAll() --> NIL */
ZH_FUNC( HPDF_FREEDOCALL )
{
   HPDF_FreeDocAll( zh_HPDF_Doc_par( 1 ) );
}

/* HPDF_SaveToFile( hDoc, cFileToSave ) --> hStatus */
ZH_FUNC( HPDF_SAVETOFILE )
{
   char *       pszFree;
   const char * pszFileName = zh_fsNameConv( zh_parcx( 2 ), &pszFree );

   zh_retnl( ( long ) HPDF_SaveToFile( zh_HPDF_Doc_par( 1 ), pszFileName ) );

   if( pszFree )
      zh_xfree( pszFree );
}

/* HPDF_SaveToStream( hDoc ) --> hStatus */
ZH_FUNC( HPDF_SAVETOSTREAM )
{
   zh_retnl( ( long ) HPDF_SaveToStream( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_GetStreamSize( hDoc ) --> nSize */
ZH_FUNC( HPDF_GETSTREAMSIZE )
{
   zh_retns( ( ZH_SIZE ) HPDF_GetStreamSize( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_ReadFromStream( hDoc, @cBuffer ) --> nBytesRead
   HPDF_ReadFromStream( hDoc, [nBytesToRead] ) --> cBuffer */
ZH_FUNC( HPDF_READFROMSTREAM )
{
   if( ZH_ISBYREF( 2 ) )
   {
      HPDF_UINT32 size = ( HPDF_UINT32 ) zh_parclen( 2 );
      HPDF_BYTE * buffer;

      if( size == 0 )
         size = HPDF_GetStreamSize( zh_HPDF_Doc_par( 1 ) );
      else if( size < 1024 )
         size = 1024;

      buffer = ( HPDF_BYTE * ) zh_xgrab( size + 1 );

      zh_retns( ( ZH_SIZE ) HPDF_ReadFromStream( zh_HPDF_Doc_par( 1 ), buffer, &size ) );

      if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
         zh_xfree( buffer );
   }
   else
   {
      HPDF_UINT32 size = ZH_IS_PARAM_NUM( 2 ) ? ( HPDF_UINT32 ) zh_parns( 2 ) : HPDF_GetStreamSize( zh_HPDF_Doc_par( 1 ) );
      HPDF_BYTE * buffer;

      buffer = ( HPDF_BYTE * ) zh_xgrab( size + 1 );

      HPDF_ReadFromStream( zh_HPDF_Doc_par( 1 ), buffer, &size );

      zh_retclen_buffer( ( char * ) buffer, size );
   }
}

/* HPDF_ResetStream( hDoc ) --> hStatus */
ZH_FUNC( HPDF_RESETSTREAM )
{
   zh_retnl( ( long ) HPDF_ResetStream( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_HasDoc( hDoc ) --> lHasDoc */
ZH_FUNC( HPDF_HASDOC )
{
   zh_retl( HPDF_HasDoc( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_SetErrorHandler( hDoc, procErrHandler ) --> hStatus */
ZH_FUNC( HPDF_SETERRORHANDLER )
{
   /* FIXME: This should be extended to pass a wrapper which calls a
             user defined codeblock. */

   zh_retnl( ( long ) HPDF_SetErrorHandler( zh_HPDF_Doc_par( 1 ), ( HPDF_Error_Handler ) zh_parptr( 2 ) ) );
}

/* HPDF_GetError( hDoc ) --> nErrorCode */
ZH_FUNC( HPDF_GETERROR )
{
   zh_retnl( ( long ) HPDF_GetError( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_GetErrorDetail( hDoc ) --> nErrorCode */
ZH_FUNC( HPDF_GETERRORDETAIL )
{
   zh_retnl( ( long ) HPDF_GetErrorDetail( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_ResetError( hDoc ) --> NIL */
ZH_FUNC( HPDF_RESETERROR )
{
   HPDF_ResetError( zh_HPDF_Doc_par( 1 ) );
}

/* HPDF_SetPagesConfiguration( hDoc, nPagePerPages ) --> hStatus */
ZH_FUNC( HPDF_SETPAGESCONFIGURATION )
{
   zh_retnl( ( long ) HPDF_SetPagesConfiguration( zh_HPDF_Doc_par( 1 ), zh_parni( 2 ) ) );
}

/* HPDF_SetPageLayout( hDoc, nLayout ) --> hStatus
        nLayout ==
   HPDF_PAGE_LAYOUT_SINGLE             0
   HPDF_PAGE_LAYOUT_ONE_COLUMN         1
   HPDF_PAGE_LAYOUT_TWO_COLUMN_LEFT    2
   HPDF_PAGE_LAYOUT_TWO_COLUMN_RIGHT   3
   HPDF_PAGE_LAYOUT_EOF                4
 */
ZH_FUNC( HPDF_SETPAGELAYOUT )
{
   zh_retnl( ( long ) HPDF_SetPageLayout( zh_HPDF_Doc_par( 1 ), ( HPDF_PageLayout ) zh_parni( 2 ) ) );
}

/* HPDF_GetPageLayout( hDoc ) --> nLayout */
ZH_FUNC( HPDF_GETPAGELAYOUT )
{
   zh_retni( ( int ) HPDF_GetPageLayout( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_SetPageMode( hDoc, nPageMode ) --> hStatus
       nPageMode ==
   HPDF_PAGE_MODE_USE_NONE     0  Display the document with neither outline nor thumbnail.
   HPDF_PAGE_MODE_USE_OUTLINE  1  Display the document with outline pane.
   HPDF_PAGE_MODE_USE_THUMBS   2  Display the document with thumbnail pane.
   HPDF_PAGE_MODE_FULL_SCREEN  3  Display the document with full screen mode.
   HPDF_PAGE_MODE_EOF          4
 */
ZH_FUNC( HPDF_SETPAGEMODE )
{
   zh_retnl( ( long ) HPDF_SetPageMode( zh_HPDF_Doc_par( 1 ), ( HPDF_PageMode ) zh_parni( 2 ) ) );
}

/* HPDF_GetPageMode( hDoc ) --> nPageMode */
ZH_FUNC( HPDF_GETPAGEMODE )
{
   zh_retni( ( int ) HPDF_GetPageMode( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_SetOpenAction( hDoc, hDestn ) --> hStatus */
ZH_FUNC( HPDF_SETOPENACTION )
{
   zh_retnl( ( long ) HPDF_SetOpenAction( zh_HPDF_Doc_par( 1 ), ( HPDF_Destination ) zh_parptr( 2 ) ) );
}

/* HPDF_GetCurrentPage( hDoc ) --> hPage */
ZH_FUNC( HPDF_GETCURRENTPAGE )
{
   zh_retptr( ( void * ) HPDF_GetCurrentPage( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_AddPage( hDoc ) --> hPage */
ZH_FUNC( HPDF_ADDPAGE )
{
   zh_retptr( ( void  * ) HPDF_AddPage( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_InsertPage( hDoc, hPage ) --> hPageInserted  : Just before hPage */
ZH_FUNC( HPDF_INSERTPAGE )
{
   zh_retptr( ( void * ) HPDF_InsertPage( zh_HPDF_Doc_par( 1 ), ( HPDF_Page ) zh_parptr( 2 ) ) );
}

/* HPDF_GetFont( hDoc, cFontName, cEncoding ) --> hFont */
ZH_FUNC( HPDF_GETFONT )
{
   zh_retptr( ( void * ) HPDF_GetFont( zh_HPDF_Doc_par( 1 ), zh_parc( 2 ), zh_parc( 3 ) ) );
}

/* HPDF_AddPageLabel( hDoc, nPageNum, nPgNoStyle, nFirstPageInRange, cPrefixToLabel ) --> hStatus
       nPgNoStyle
   HPDF_PAGE_NUM_STYLE_DECIMAL         1   Page label is displayed by Arabic numerals.
   HPDF_PAGE_NUM_STYLE_UPPER_ROMAN     2   Page label is displayed by Uppercase roman numerals.
   HPDF_PAGE_NUM_STYLE_LOWER_ROMAN     3   Page label is displayed by Lowercase roman numerals.
   HPDF_PAGE_NUM_STYLE_UPPER_LETTERS   4   Page label is displayed by Uppercase letters (using A to Z).
   HPDF_PAGE_NUM_STYLE_LOWER_LETTERS   5   Page label is displayed by Lowercase letters (using a to z).
 */
ZH_FUNC( HPDF_ADDPAGELABEL )
{
   zh_retnl( ( long ) HPDF_AddPageLabel( zh_HPDF_Doc_par( 1 ), zh_parni( 2 ), ( HPDF_PageNumStyle ) zh_parni( 3 ), zh_parni( 4 ), zh_parc( 5 ) ) );
}

/* HPDF_CreateExtGState( hDoc ) --> hExtGState */
ZH_FUNC( HPDF_CREATEEXTGSTATE )
{
   zh_retptr( ( void * ) HPDF_CreateExtGState( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_CreateOutline( hDoc, hParentOutline, cTitle, hEncoder ) --> hOutline */
ZH_FUNC( HPDF_CREATEOUTLINE )
{
   zh_retptr( ( void * ) HPDF_CreateOutline( zh_HPDF_Doc_par( 1 ), ( HPDF_Outline ) zh_parptr( 2 ), zh_parc( 3 ), ( HPDF_Encoder ) zh_parptr( 4 ) ) );
}

/* HPDF_GetEncoder( hDoc, cEncoding ) --> hEncoder */
ZH_FUNC( HPDF_GETENCODER )
{
   zh_retptr( ( void * ) HPDF_GetEncoder( zh_HPDF_Doc_par( 1 ), zh_parc( 2 ) ) );
}

/* HPDF_GetCurrentEncoder( hDoc ) --> hEncoder */
ZH_FUNC( HPDF_GETCURRENTENCODER )
{
   zh_retptr( ( void * ) HPDF_GetCurrentEncoder( zh_HPDF_Doc_par( 1 ) ) );
}

/* HPDF_SetCurrentEncoder( hDoc, hEncoder ) --> hStatus */
ZH_FUNC( HPDF_SETCURRENTENCODER )
{
   zh_retnl( ( long ) HPDF_SetCurrentEncoder( zh_HPDF_Doc_par( 1 ), zh_parc( 2 ) ) );
}

/* HPDF_SetInfoAttr( hDoc, nInfoType, cInfo ) --> hStatus
       nInfoType ==
   HPDF_INFO_AUTHOR
   HPDF_INFO_CREATOR
   HPDF_INFO_TITLE
   HPDF_INFO_SUBJECT
   HPDF_INFO_KEYWORDS
 */
ZH_FUNC( HPDF_SETINFOATTR )
{
   HPDF_Doc doc = zh_HPDF_Doc_par( 1 );

   if( doc )
      zh_retnl( ( long ) HPDF_SetInfoAttr( doc, ( HPDF_InfoType ) zh_parni( 2 ), zh_parc( 3 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
}

/* HPDF_GetInfoAttr( hDoc, nInfoType ) --> cInfo */
ZH_FUNC( HPDF_GETINFOATTR )
{
   HPDF_Doc doc = zh_HPDF_Doc_par( 1 );

   if( doc )
      zh_retc( HPDF_GetInfoAttr( doc, ( HPDF_InfoType ) zh_parni( 2 ) ) );
   else
      zh_retc_null();
}

/* HPDF_SetInfoDateAttr( hDoc, nInfoType, aDateValues ) --> hStatus
       nInfoType ==
   HPDF_INFO_CREATION_DATE
   HPDF_INFO_MOD_DATE
 */
ZH_FUNC( HPDF_SETINFODATEATTR )
{
   HPDF_Doc doc = zh_HPDF_Doc_par( 1 );

   if( doc )
   {
      HPDF_Date date;

      memset( &date, 0, sizeof( date ) );

      date.year    = zh_parvni( 3, 1 );
      date.month   = zh_parvni( 3, 2 );
      date.day     = zh_parvni( 3, 3 );
      date.hour    = zh_parvni( 3, 4 );
      date.minutes = zh_parvni( 3, 5 );
      date.seconds = zh_parvni( 3, 6 );
      date.ind     = ' ';

      zh_retnl( ( long ) HPDF_SetInfoDateAttr( doc, ( HPDF_InfoType ) zh_parni( 2 ), date ) );
   }
   else
      zh_retnl( ZH_HPDF_BADPARAM );
}

/* HPDF_SetPassword( hDoc, cOwnerPassword = NO NIL, cUserPassword = CANBE NIL ) --> hStatus */
ZH_FUNC( HPDF_SETPASSWORD )
{
   zh_retnl( ( long ) HPDF_SetPassword( zh_HPDF_Doc_par( 1 ), zh_parc( 2 ), zh_parc( 3 ) ) );
}

/* HPDF_SetPermission( hDoc, nPermission ) --> hStatus
       nPermission ==
   HPDF_ENABLE_READ      1   user can read the document.
   HPDF_ENABLE_PRINT     2   user can print the document.
   HPDF_ENABLE_EDIT_ALL  3   user can edit the contents of the document other than annotations, form fields.
   HPDF_ENABLE_COPY      4   user can copy the text and the graphics of the document.
   HPDF_ENABLE_EDIT      5   user can add or modify the annotations and form fields of the document.
 */
ZH_FUNC( HPDF_SETPERMISSION )
{
   zh_retnl( ( long ) HPDF_SetPermission( zh_HPDF_Doc_par( 1 ), zh_parni( 2 ) ) );
}

/* HPDF_SetEncryptionMode( hDoc, nEncMode, nKeyLen ) --> hStatus
       nEncMode ==
   HPDF_ENCRYPT_R2    1   Use "Revision 2" algorithm.
                             The length of key is automatically set to 5(40bit).
   HPDF_ENCRYPT_R3    2   Use "Revision 3" algorithm.
                             Between 5(40bit) and 16(128bit) can be specified for length of the key
 */
ZH_FUNC( HPDF_SETENCRYPTIONMODE )
{
   zh_retnl( ( long ) HPDF_SetEncryptionMode( zh_HPDF_Doc_par( 1 ), ( HPDF_EncryptMode ) zh_parni( 2 ), zh_parni( 3 ) ) );
}

/* HPDF_SetCompressionMode( hDoc, nCompMode ) --> hStatus
       nCompMode ==
   HPDF_COMP_NONE         1    All contents are not compressed.
   HPDF_COMP_TEXT         2    Compress the contents stream of the page.
   HPDF_COMP_IMAGE        3    Compress the streams of the image objects.
   HPDF_COMP_METADATA     4    Other stream datas (fonts, cmaps and so on) are compressed.
   HPDF_COMP_ALL          5    All stream datas are compressed. (The same as "HPDF_COMP_TEXT | HPDF_COMP_IMAGE | HPDF_COMP_METADATA")
 */
ZH_FUNC( HPDF_SETCOMPRESSIONMODE )
{
   zh_retnl( ( long ) HPDF_SetCompressionMode( zh_HPDF_Doc_par( 1 ), zh_parni( 2 ) ) );
}

/* --- Page Handling --- */

/* HPDF_Page_SetWidth( hPage, nWidth ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETWIDTH )
{
   HPDF_Page page = ( HPDF_Page ) zh_parptr( 1 );

   if( page )
      zh_retnl( ( long ) HPDF_Page_SetWidth( page, ( HPDF_REAL ) zh_parnd( 2 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
}

/* HPDF_Page_SetHeight( hPage, nHeight ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETHEIGHT )
{
   HPDF_Page page = ( HPDF_Page ) zh_parptr( 1 );

   if( page )
      zh_retnl( ( long ) HPDF_Page_SetHeight( page, ( HPDF_REAL ) zh_parnd( 2 ) ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
}

/* HPDF_Page_SetSize( hPage, nSize, nOrientation = 1 Portrait, 2 Landscape ) --> hStatus
       nSize ==
   HPDF_PAGE_SIZE_LETTER         1      8.5 x 11 (Inches) 612 x 792
   HPDF_PAGE_SIZE_LEGAL          2      8.5 x 14 (Inches) 612 x 1008
   HPDF_PAGE_SIZE_A3             3      297 x 420 (mm) 841.89 x 1199.551
   HPDF_PAGE_SIZE_A4             4      210 x 297 (mm)  595.276 x 841.89
   HPDF_PAGE_SIZE_A5             5      148 x 210 (mm) 419.528 x 595.276
   HPDF_PAGE_SIZE_B4             6      250 x 353 (mm)  708.661 x 1000.63
   HPDF_PAGE_SIZE_B5             7      176 x 250 (mm) 498.898 x 708.661
   HPDF_PAGE_SIZE_EXECUTIVE      8      7.5 x 10.5 (Inches) 522 x 756
   HPDF_PAGE_SIZE_US4x6          9      4 x 6 (Inches) 288 x 432
   HPDF_PAGE_SIZE_US4x8         10      4 x 8 (Inches) 288 x 576
   HPDF_PAGE_SIZE_US5x7         11      5 x 7 (Inches) 360 x 504
   HPDF_PAGE_SIZE_COMM10        12      4.125 x 9.5 (Inches) 297x 684
 */
ZH_FUNC( HPDF_PAGE_SETSIZE )
{
   zh_retnl( ( long ) HPDF_Page_SetSize( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_PageSizes ) zh_parni( 2 ), ( HPDF_PageDirection ) zh_parni( 3 ) ) );
}

/* HPDF_Page_SetRotate( hPage, nAngle = 0-360 ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETROTATE )
{
   zh_retnl( ( long ) HPDF_Page_SetRotate( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_UINT16 ) zh_parni( 2 ) ) );
}

/* HPDF_Page_GetWidth( hPage ) --> nWidth */
ZH_FUNC( HPDF_PAGE_GETWIDTH )
{
   zh_retnd( ( double ) HPDF_Page_GetWidth( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetHeight( hPage ) --> nHeight */
ZH_FUNC( HPDF_PAGE_GETHEIGHT )
{
   zh_retnd( ( double ) HPDF_Page_GetHeight( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_CreateDestination( hPage ) --> hDestn */
ZH_FUNC( HPDF_PAGE_CREATEDESTINATION )
{
   zh_retptr( ( void * ) HPDF_Page_CreateDestination( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_CreateAnnot( hPage, aRect[nLeft,nTop,nRight,nBottom], cText, cEncoder ) --> nHandle */
ZH_FUNC( HPDF_PAGE_CREATETEXTANNOT )
{
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

   zh_retptr( HPDF_Page_CreateTextAnnot( ( HPDF_Page ) zh_parptr( 1 ), rc, zh_parc( 3 ), ( HPDF_Encoder ) zh_parptr( 4 ) ) );
}

/* HPDF_Page_CreateLinkAnnot( hPage, aRect, hDestn ) --> nHandle */
ZH_FUNC( HPDF_PAGE_CREATELINKANNOT )
{
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

   zh_retptr( HPDF_Page_CreateLinkAnnot( ( HPDF_Page ) zh_parptr( 1 ), rc, ( HPDF_Destination ) zh_parptr( 3 ) ) );
}

/* HPDF_Page_CreateURILinkAnnot( hPage, aRect, cURI ) --> nHandle */
ZH_FUNC( HPDF_PAGE_CREATEURILINKANNOT )
{
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) zh_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) zh_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) zh_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) zh_parvnd( 2, 4 );

   zh_retptr( HPDF_Page_CreateURILinkAnnot( ( HPDF_Page ) zh_parptr( 1 ), rc, zh_parc( 3 ) ) );
}

/* HPDF_Page_TextWidth( hPage, cText ) --> nTextWidth */
ZH_FUNC( HPDF_PAGE_TEXTWIDTH )
{
   zh_retnd( ( double ) HPDF_Page_TextWidth( ( HPDF_Page ) zh_parptr( 1 ), zh_parc( 2 ) ) );
}

/* HPDF_Page_MeasureText( hPage, cText, nWidth, lWordWrap ) --> nByteLenOfTextToFitWidth */
ZH_FUNC( HPDF_PAGE_MEASURETEXT )
{
   zh_retnl( ( long ) HPDF_Page_MeasureText( ( HPDF_Page ) zh_parptr( 1 ), zh_parc( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), zh_parl( 4 ) ? HPDF_TRUE : HPDF_FALSE, NULL ) );
}

/* HPDF_Page_GetMode( hPage ) --> nGraphicMode */
ZH_FUNC( HPDF_PAGE_GETGMODE )
{
   zh_retnl( ( long ) HPDF_Page_GetGMode( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetCurrentPos( hPage ) --> aCurPos[] { nX, nY } */
ZH_FUNC( HPDF_PAGE_GETCURRENTPOS )
{
   HPDF_Point pt;
   PZH_ITEM   info = zh_itemArrayNew( 2 );

   HPDF_Page_GetCurrentPos2( ( HPDF_Page ) zh_parptr( 1 ), &pt );

   zh_arraySetND( info, 1, ( double ) pt.x );
   zh_arraySetND( info, 2, ( double ) pt.y );

   zh_itemReturnRelease( info );
}

/* HPDF_Page_GetCurrentTextPos( hPage ) --> aCurTextPos[] { nX, nY } */
ZH_FUNC( HPDF_PAGE_GETCURRENTTEXTPOS )
{
   HPDF_Point pt;
   PZH_ITEM   info = zh_itemArrayNew( 2 );

   HPDF_Page_GetCurrentTextPos2( ( HPDF_Page ) zh_parptr( 1 ), &pt );

   zh_arraySetND( info, 1, ( double ) pt.x );
   zh_arraySetND( info, 2, ( double ) pt.y );

   zh_itemReturnRelease( info );
}

/* HPDF_Page_GetCurrentFont( hPage ) --> hFont */
ZH_FUNC( HPDF_PAGE_GETCURRENTFONT )
{
   zh_retptr( ( void * ) HPDF_Page_GetCurrentFont( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetCurrentFontSize( hPage ) --> nFontSize */
ZH_FUNC( HPDF_PAGE_GETCURRENTFONTSIZE )
{
   zh_retnd( ( double ) HPDF_Page_GetCurrentFontSize( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetTransMatrix( hPage ) --> aMatrix[] */
ZH_FUNC( HPDF_PAGE_GETTRANSMATRIX )
{
   HPDF_TransMatrix matrix;
   PZH_ITEM         info = zh_itemArrayNew( 6 );

   matrix = HPDF_Page_GetTransMatrix( ( HPDF_Page ) zh_parptr( 1 ) );

   zh_arraySetND( info, 1, ( double ) matrix.a );
   zh_arraySetND( info, 2, ( double ) matrix.b );
   zh_arraySetND( info, 3, ( double ) matrix.c );
   zh_arraySetND( info, 4, ( double ) matrix.d );
   zh_arraySetND( info, 5, ( double ) matrix.x );
   zh_arraySetND( info, 6, ( double ) matrix.y );

   zh_itemReturnRelease( info );
}

/* HPDF_Page_GetLineWidth( hPage ) --> nLineWidth */
ZH_FUNC( HPDF_PAGE_GETLINEWIDTH )
{
   zh_retnd( ( double ) HPDF_Page_GetLineWidth( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetLineCap( hPage ) --> nLineCapStyle */
ZH_FUNC( HPDF_PAGE_GETLINECAP )
{
   zh_retnl( ( long ) HPDF_Page_GetLineCap( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetLineJoin( hPage ) --> nLineJoinStyle */
ZH_FUNC( HPDF_PAGE_GETLINEJOIN )
{
   zh_retnl( ( long ) HPDF_Page_GetLineJoin( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetMiterLimit( hPage ) --> nMiterLimit */
ZH_FUNC( HPDF_PAGE_GETMITERLIMIT )
{
   zh_retnd( ( double ) HPDF_Page_GetMiterLimit( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetDash( hPage ) --> aDash */
ZH_FUNC( HPDF_PAGE_GETDASH )
{
   HPDF_DashMode dash;
   PZH_ITEM      info = zh_itemArrayNew( 10 );

   dash = HPDF_Page_GetDash( ( HPDF_Page ) zh_parptr( 1 ) );

   zh_arraySetNI( info, 1, dash.ptn[ 0 ] );
   zh_arraySetNI( info, 2, dash.ptn[ 1 ] );
   zh_arraySetNI( info, 3, dash.ptn[ 2 ] );
   zh_arraySetNI( info, 4, dash.ptn[ 3 ] );
   zh_arraySetNI( info, 5, dash.ptn[ 4 ] );
   zh_arraySetNI( info, 6, dash.ptn[ 5 ] );
   zh_arraySetNI( info, 7, dash.ptn[ 6 ] );
   zh_arraySetNI( info, 8, dash.ptn[ 7 ] );
   zh_arraySetND( info, 9, dash.num_ptn );
   zh_arraySetND( info, 10, dash.phase   );

   zh_itemReturnRelease( info );
}

/* HPDF_Page_GetFlat( hPage ) --> nCurFlatness */
ZH_FUNC( HPDF_PAGE_GETFLAT )
{
   zh_retnd( ( double ) HPDF_Page_GetFlat( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetCharSpace( hPage ) --> nCurCharSpace */
ZH_FUNC( HPDF_PAGE_GETCHARSPACE )
{
   zh_retnd( ( double ) HPDF_Page_GetCharSpace( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetWordSpace( hPage ) --> nCurWordSpace */
ZH_FUNC( HPDF_PAGE_GETWORDSPACE )
{
   zh_retnd( ( double ) HPDF_Page_GetWordSpace( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetHorizontalScalling( hPage ) --> nHorzScaling */
ZH_FUNC( HPDF_PAGE_GETHORIZONTALSCALLING )
{
   zh_retnd( ( double ) HPDF_Page_GetHorizontalScalling( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetTextLeading( hPage ) --> nTextLeading */
ZH_FUNC( HPDF_PAGE_GETTEXTLEADING )
{
   zh_retnd( ( double ) HPDF_Page_GetTextLeading( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetTextRenderingMode( hPage ) --> nTextRenderingMode */
ZH_FUNC( HPDF_PAGE_GETTEXTRENDERINGMODE )
{
   zh_retnd( ( double ) HPDF_Page_GetTextRenderingMode( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetTextRise( hPage ) --> nTextRise */
ZH_FUNC( HPDF_PAGE_GETTEXTRISE )
{
   zh_retnd( ( double ) HPDF_Page_GetTextRise( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetRGBFill( hPage ) --> aRGBFill[] { nRed, nGreen, nBlue } */
ZH_FUNC( HPDF_PAGE_GETRGBFILL )
{
   HPDF_RGBColor rgb;
   PZH_ITEM      info = zh_itemArrayNew( 3 );

   rgb = HPDF_Page_GetRGBFill( ( HPDF_Page ) zh_parptr( 1 ) );

   zh_arraySetND( info, 1, ( double ) rgb.r );
   zh_arraySetND( info, 2, ( double ) rgb.g );
   zh_arraySetND( info, 3, ( double ) rgb.b );

   zh_itemReturnRelease( info );
}

/* HPDF_Page_GetRGBStroke( hPage ) --> aRGBStroke[] { nRed, nGreen, nBlue } */
ZH_FUNC( HPDF_PAGE_GETRGBSTROKE )
{
   HPDF_RGBColor rgb;
   PZH_ITEM      info = zh_itemArrayNew( 3 );

   rgb = HPDF_Page_GetRGBStroke( ( HPDF_Page ) zh_parptr( 1 ) );

   zh_arraySetND( info, 1, ( double ) rgb.r );
   zh_arraySetND( info, 2, ( double ) rgb.g );
   zh_arraySetND( info, 3, ( double ) rgb.b );

   zh_itemReturnRelease( info );
}

/* HPDF_Page_GetCMYKFill( hPage ) --> aCMYKFill[] { nC, nM, nY, nK } */
ZH_FUNC( HPDF_PAGE_GETCMYKFILL )
{
   HPDF_CMYKColor cmyk;
   PZH_ITEM       info = zh_itemArrayNew( 4 );

   cmyk = HPDF_Page_GetCMYKFill( ( HPDF_Page ) zh_parptr( 1 ) );

   zh_arraySetND( info, 1, ( double ) cmyk.c );
   zh_arraySetND( info, 2, ( double ) cmyk.m );
   zh_arraySetND( info, 3, ( double ) cmyk.y );
   zh_arraySetND( info, 4, ( double ) cmyk.k );

   zh_itemReturnRelease( info );
}

/* HPDF_Page_GetCMYKStroke( hPage ) --> aCMYKStroke[] { nC, nM, nY, nK } */
ZH_FUNC( HPDF_PAGE_GETCMYKSTROKE )
{
   HPDF_CMYKColor cmyk;
   PZH_ITEM       info = zh_itemArrayNew( 4 );

   cmyk = HPDF_Page_GetCMYKStroke( ( HPDF_Page ) zh_parptr( 1 ) );

   zh_arraySetND( info, 1, ( double ) cmyk.c );
   zh_arraySetND( info, 2, ( double ) cmyk.m );
   zh_arraySetND( info, 3, ( double ) cmyk.y );
   zh_arraySetND( info, 4, ( double ) cmyk.k );

   zh_itemReturnRelease( info );
}

/* HPDF_Page_GetGrayFill( hPage ) --> nGrayFillValue */
ZH_FUNC( HPDF_PAGE_GETGRAYFILL )
{
   zh_retnd( ( double ) HPDF_Page_GetGrayFill( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetGrayStroke( hPage ) --> nGrayStrokeValue */
ZH_FUNC( HPDF_PAGE_GETGRAYSTROKE )
{
   zh_retnd( ( double ) HPDF_Page_GetGrayStroke( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetStrokingColorSpace( hPage ) --> nStrokingSpace */
ZH_FUNC( HPDF_PAGE_GETSTROKINGCOLORSPACE )
{
   zh_retni( ( int ) HPDF_Page_GetStrokingColorSpace( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetFillingColorSpace( hPage ) --> nFillingColorSpace */
ZH_FUNC( HPDF_PAGE_GETFILLINGCOLORSPACE )
{
   zh_retni( ( int ) HPDF_Page_GetFillingColorSpace( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GetTextMatrix( hPage ) --> aMatrix[] */
ZH_FUNC( HPDF_PAGE_GETTEXTMATRIX )
{
   HPDF_TransMatrix matrix;
   PZH_ITEM         info = zh_itemArrayNew( 6 );

   matrix = HPDF_Page_GetTextMatrix( ( HPDF_Page ) zh_parptr( 1 ) );

   zh_arraySetND( info, 1, ( double ) matrix.a );
   zh_arraySetND( info, 2, ( double ) matrix.b );
   zh_arraySetND( info, 3, ( double ) matrix.c );
   zh_arraySetND( info, 4, ( double ) matrix.d );
   zh_arraySetND( info, 5, ( double ) matrix.x );
   zh_arraySetND( info, 6, ( double ) matrix.y );

   zh_itemReturnRelease( info );
}

/* HPDF_Page_GetGStateDepth( hPage ) --> nGStateDepth */
ZH_FUNC( HPDF_PAGE_GETGSTATEDEPTH )
{
   zh_retni( ( int ) HPDF_Page_GetGStateDepth( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_SetSlideShow( hPage, nType, nDurationPerFrame, nTranstnTime = 1 Second ) --> hStatus
       nType ==
   HPDF_TS_WIPE_RIGHT
   HPDF_TS_WIPE_UP
   HPDF_TS_WIPE_LEFT
   HPDF_TS_WIPE_DOWN
   HPDF_TS_BARN_DOORS_HORIZONTAL_OUT
   HPDF_TS_BARN_DOORS_HORIZONTAL_IN
   HPDF_TS_BARN_DOORS_VERTICAL_OUT
   HPDF_TS_BARN_DOORS_VERTICAL_IN
   HPDF_TS_BOX_OUT
   HPDF_TS_BOX_IN
   HPDF_TS_BLINDS_HORIZONTAL
   HPDF_TS_BLINDS_VERTICAL
   HPDF_TS_DISSOLVE
   HPDF_TS_GLITTER_RIGHT
   HPDF_TS_GLITTER_DOWN
   HPDF_TS_GLITTER_TOP_LEFT_TO_BOTTOM_RIGHT
   HPDF_TS_REPLACE
 */
ZH_FUNC( HPDF_PAGE_SETSLIDESHOW )
{
   zh_retnl( ( long ) HPDF_Page_SetSlideShow( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_TransitionStyle ) zh_parni( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ) ) );
}

/* --- GRAPHICS --- */

/* HPDF_Page_SetLineWidth( hPage, nLineWidth ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETLINEWIDTH )
{
   zh_retnl( ( long ) HPDF_Page_SetLineWidth( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_Page_SetLineCap( hPage, nLineCap ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETLINECAP )
{
   zh_retnl( ( long ) HPDF_Page_SetLineCap( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_LineCap ) zh_parni( 2 ) ) );
}

/* HPDF_Page_SetLineJoin( hPage, nLineJoin ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETLINEJOIN )
{
   zh_retnl( ( long ) HPDF_Page_SetLineJoin( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_LineJoin ) zh_parni( 2 ) ) );
}

/* HPDF_Page_SetMiterLimit( hPage, nMiterLimit ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETMITERLIMIT )
{
   zh_retnl( ( long ) HPDF_Page_SetMiterLimit( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_Page_SetDash( hPage, aDash, nNumPoints, nStartFrom ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETDASH )
{
   HPDF_DashMode dash;
   int nPtns = zh_parni( 3 );
   int i;

   for( i = 0; i < nPtns; i++ )
      dash.ptn[ i ] = ( HPDF_UINT16 ) zh_parvni( 2, i + 1 );

   zh_retnl( ( long ) HPDF_Page_SetDash( ( HPDF_Page ) zh_parptr( 1 ), dash.ptn, nPtns, zh_parni( 4 ) ) );
}

/* HPDF_Page_SetExtGState( hPage, hGState ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETEXTGSTATE )
{
   zh_retnl( ( long ) HPDF_Page_SetExtGState( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_ExtGState ) zh_parptr( 2 ) ) );
}

/* HPDF_Page_GSave( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_GSAVE )
{
   zh_retnl( ( long ) HPDF_Page_GSave( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_GRestore( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_GRESTORE )
{
   zh_retnl( ( long ) HPDF_Page_GRestore( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_Concat( hPage, nA, nB, nC, nD, nX, nY ) --> hStatus */
ZH_FUNC( HPDF_PAGE_CONCAT )
{
   zh_retnl( ( long ) HPDF_Page_Concat( ( HPDF_Page ) zh_parptr( 1 ),
                                        ( HPDF_REAL ) zh_parnd( 2 ),
                                        ( HPDF_REAL ) zh_parnd( 3 ),
                                        ( HPDF_REAL ) zh_parnd( 4 ),
                                        ( HPDF_REAL ) zh_parnd( 5 ),
                                        ( HPDF_REAL ) zh_parnd( 6 ),
                                        ( HPDF_REAL ) zh_parnd( 7 ) ) );
}

/* HPDF_Page_MoveTo( hPage, nX, nY ) --> hStatus */
ZH_FUNC( HPDF_PAGE_MOVETO )
{
   zh_retnl( ( long ) HPDF_Page_MoveTo( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ) ) );
}

/* HPDF_Page_LineTo( hPage, nX, nY ) --> hStatus */
ZH_FUNC( HPDF_PAGE_LINETO )
{
   zh_retnl( ( long ) HPDF_Page_LineTo( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ) ) );
}

/* HPDF_Page_CurveTo( hPage, nX1, nY1, nX2, nY2, nX3, nY3  ) --> hStatus */
ZH_FUNC( HPDF_PAGE_CURVETO )
{
   zh_retnl( ( long ) HPDF_Page_CurveTo( ( HPDF_Page ) zh_parptr( 1 ),
                                         ( HPDF_REAL ) zh_parnd( 2 ),
                                         ( HPDF_REAL ) zh_parnd( 3 ),
                                         ( HPDF_REAL ) zh_parnd( 4 ),
                                         ( HPDF_REAL ) zh_parnd( 5 ),
                                         ( HPDF_REAL ) zh_parnd( 6 ),
                                         ( HPDF_REAL ) zh_parnd( 7 ) ) );
}

/* HPDF_Page_CurveTo2( hPage, nX2, nY2, nX3, nY3 ) --> hStatus */
ZH_FUNC( HPDF_PAGE_CURVETO2 )
{
   zh_retnl( ( long ) HPDF_Page_CurveTo2( ( HPDF_Page ) zh_parptr( 1 ),
                                          ( HPDF_REAL ) zh_parnd( 2 ),
                                          ( HPDF_REAL ) zh_parnd( 3 ),
                                          ( HPDF_REAL ) zh_parnd( 4 ),
                                          ( HPDF_REAL ) zh_parnd( 5 ) ) );
}

/* HPDF_Page_CurveTo3( hPage, nX1, nY1, nX3, nY3 ) --> hStatus */
ZH_FUNC( HPDF_PAGE_CURVETO3 )
{
   zh_retnl( ( long ) HPDF_Page_CurveTo3( ( HPDF_Page ) zh_parptr( 1 ),
                                          ( HPDF_REAL ) zh_parnd( 2 ),
                                          ( HPDF_REAL ) zh_parnd( 3 ),
                                          ( HPDF_REAL ) zh_parnd( 4 ),
                                          ( HPDF_REAL ) zh_parnd( 5 ) ) );
}

/* HPDF_Page_ClosePath( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_CLOSEPATH )
{
   zh_retnl( ( long ) HPDF_Page_ClosePath( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_Rectangle( hPage, nX, nY, nWidth, nHeight ) --> hStatus */
ZH_FUNC( HPDF_PAGE_RECTANGLE )
{
   zh_retnl( ( long ) HPDF_Page_Rectangle( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ), ( HPDF_REAL ) zh_parnd( 5 ) ) );
}

/* HPDF_Page_Stroke( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_STROKE )
{
   zh_retnl( ( long ) HPDF_Page_Stroke( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_ClosePathStroke( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_CLOSEPATHSTROKE )
{
   zh_retnl( ( long ) HPDF_Page_ClosePathStroke( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_SetFontAndSize( hPage, hFont, nSize ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETFONTANDSIZE )
{
   zh_retnl( ( long ) HPDF_Page_SetFontAndSize( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_Font ) zh_parptr( 2 ), ( HPDF_REAL ) zh_parnd( 3 ) ) );
}

/* HPDF_Page_BeginText( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_BEGINTEXT )
{
   zh_retnl( ( long ) HPDF_Page_BeginText( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_EndText( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_ENDTEXT )
{
   zh_retnl( ( long ) HPDF_Page_EndText( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_TextOut( hPage, nX, nY, cText ) --> hStatus */
ZH_FUNC( HPDF_PAGE_TEXTOUT )
{
   zh_retnl( ( long ) HPDF_Page_TextOut( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), zh_parc( 4 ) ) );
}

/* HPDF_Page_MoveTextPos( hPage, nX, nY ) --> hStatus */
ZH_FUNC( HPDF_PAGE_MOVETEXTPOS )
{
   zh_retnl( ( long ) HPDF_Page_MoveTextPos( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ) ) );
}

/* HPDF_Page_ShowText( hPage, cText ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SHOWTEXT )
{
   zh_retnl( ( long ) HPDF_Page_ShowText( ( HPDF_Page ) zh_parptr( 1 ), zh_parc( 2 ) ) );
}

/* HPDF_Page_Fill( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_FILL )
{
   zh_retnl( ( long ) HPDF_Page_Fill( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_Eofill( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_EOFILL )
{
   zh_retnl( ( long ) HPDF_Page_Eofill( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_FillStroke( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_FILLSTROKE )
{
   zh_retnl( ( long ) HPDF_Page_FillStroke( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_EofillStroke( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_EOFILLSTROKE )
{
   zh_retnl( ( long ) HPDF_Page_EofillStroke( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_ClosePathFillStroke( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_CLOSEPATHFILLSTROKE )
{
   zh_retnl( ( long ) HPDF_Page_ClosePathFillStroke( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_ClosePathEofillStroke( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_CLOSEPATHEOFILLSTROKE )
{
   zh_retnl( ( long ) HPDF_Page_ClosePathEofillStroke( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_EndPath( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_ENDPATH )
{
   zh_retnl( ( long ) HPDF_Page_EndPath( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_Clip( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_CLIP )
{
   zh_retnl( ( long ) HPDF_Page_Clip( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_Eoclip( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_EOCLIP )
{
   zh_retnl( ( long ) HPDF_Page_Eoclip( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_SetCharSpace( hPage, nSpaceWidth ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETCHARSPACE )
{
   zh_retnl( ( long ) HPDF_Page_SetCharSpace( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_Page_SetWordSpace( hPage, nSpaceWidth ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETWORDSPACE )
{
   zh_retnl( ( long ) HPDF_Page_SetWordSpace( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_Page_SetHorizontalScalling( hPage, nHorzScale ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETHORIZONTALSCALLING )
{
   zh_retnl( ( long ) HPDF_Page_SetHorizontalScalling( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_Page_SetTextLeading( hPage, nTextLeading ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETTEXTLEADING )
{
   zh_retnl( ( long ) HPDF_Page_SetTextLeading( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_Page_SetTextRenderingMode( hPage, nTextRenderingMode ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETTEXTRENDERINGMODE )
{
   zh_retnl( ( long ) HPDF_Page_SetTextRenderingMode( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_TextRenderingMode ) zh_parni( 2 ) ) );
}

/* HPDF_Page_SetTextRise( hPage, nTextRise ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETTEXTRISE )
{
   zh_retnl( ( long ) HPDF_Page_SetTextRise( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_Page_MoveTextPos2( hPage, nX, nY ) --> hStatus */
ZH_FUNC( HPDF_PAGE_MOVETEXTPOS2 )
{
   zh_retnl( ( long ) HPDF_Page_MoveTextPos2( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ) ) );
}

/* HPDF_Page_SetTextMatrix( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETTEXTMATRIX )
{
   zh_retnl( ( long ) HPDF_Page_SetTextMatrix( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ), ( HPDF_REAL ) zh_parnd( 5 ), ( HPDF_REAL ) zh_parnd( 6 ), ( HPDF_REAL ) zh_parnd( 7 ) ) );
}

/* HPDF_Page_MoveToNextLine( hPage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_MOVETONEXTLINE )
{
   zh_retnl( ( long ) HPDF_Page_MoveToNextLine( ( HPDF_Page ) zh_parptr( 1 ) ) );
}

/* HPDF_Page_ShowTextNextLine( hPage, cText ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SHOWTEXTNEXTLINE )
{
   zh_retnl( ( long ) HPDF_Page_ShowTextNextLine( ( HPDF_Page ) zh_parptr( 1 ), zh_parc( 2 ) ) );
}

/* HPDF_Page_ShowTextNextLineEx( hPage, nWordSpace, nCharSpace, cText ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SHOWTEXTNEXTLINEEX )
{
   zh_retnl( ( long ) HPDF_Page_ShowTextNextLineEx( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), zh_parc( 4 ) ) );
}

/* HPDF_Page_SetGrayFill( hPage, nGrayFill ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETGRAYFILL )
{
   zh_retnl( ( long ) HPDF_Page_SetGrayFill( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_Page_SetGrayStroke( hPage, nGrayStroke ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETGRAYSTROKE )
{
   zh_retnl( ( long ) HPDF_Page_SetGrayStroke( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_Page_SetRGBFill( hPage, nRGBRed, nRGBGreen, nRGBBlue ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETRGBFILL )
{
   zh_retnl( ( long ) HPDF_Page_SetRGBFill( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ) ) );
}

/* HPDF_Page_SetRGBStroke( hPage, nRGBRed, nRGBGreen, nRGBBlue ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETRGBSTROKE )
{
   zh_retnl( ( long ) HPDF_Page_SetRGBStroke( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ) ) );
}

/* HPDF_Page_SetCMYKFill( hPage, nC, nM, nY, nK ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETCMYKFILL )
{
   zh_retnl( ( long ) HPDF_Page_SetCMYKFill( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ), ( HPDF_REAL ) zh_parnd( 5 ) ) );
}

/* HPDF_Page_SetCMYKStroke( hPage, nC, nM, nY, nK ) --> hStatus */
ZH_FUNC( HPDF_PAGE_SETCMYKSTROKE )
{
   zh_retnl( ( long ) HPDF_Page_SetCMYKStroke( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ), ( HPDF_REAL ) zh_parnd( 5 ) ) );
}

/* HPDF_Page_ExecuteXObject( hPage, hImage ) --> hStatus */
ZH_FUNC( HPDF_PAGE_EXECUTEXOBJECT )
{
   zh_retnl( ( long ) HPDF_Page_ExecuteXObject( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_Image ) zh_parptr( 2 ) ) );
}

/* HPDF_Page_DrawImage( hPage, hImage, nX, nY, nWidth, nHeight ) --> hStatus */
ZH_FUNC( HPDF_PAGE_DRAWIMAGE )
{
   zh_retnl( ( long ) HPDF_Page_DrawImage( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_Image ) zh_parptr( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ), ( HPDF_REAL ) zh_parnd( 5 ), ( HPDF_REAL ) zh_parnd( 6 ) ) );
}

/* HPDF_Page_Circle( hPage, nX, nY, nRay ) --> hStatus */
ZH_FUNC( HPDF_PAGE_CIRCLE )
{
   zh_retnl( ( long ) HPDF_Page_Circle( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ) ) );
}

/* HPDF_Page_Arc( hPage, nX, nY, nRay, nAngle1, nAngle2 ) --> hStatus */
ZH_FUNC( HPDF_PAGE_ARC )
{
   zh_retnl( ( long ) HPDF_Page_Arc( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ), ( HPDF_REAL ) zh_parnd( 5 ), ( HPDF_REAL ) zh_parnd( 6 ) ) );
}

/* HPDF_Page_Ellipse( hPage, nX, nY, nxRay, nyRay ) --> hStatus */
ZH_FUNC( HPDF_PAGE_ELLIPSE )
{
   zh_retnl( ( long ) HPDF_Page_Ellipse( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ), ( HPDF_REAL ) zh_parnd( 5 ) ) );
}

/* HPDF_Page_TextRect( hPage, nLeft, nTop, nRight, nBottom, cText, nAlign ) --> hStatus */
ZH_FUNC( HPDF_PAGE_TEXTRECT )
{
   zh_retnl( ( long ) HPDF_Page_TextRect( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ), ( HPDF_REAL ) zh_parnd( 5 ), zh_parc( 6 ), ( HPDF_TextAlignment ) zh_parni( 7 ), NULL ) );
}

/* --- FONTS --- */

/* HPDF_Font_GetFontName( hFont ) --> cFontName */
ZH_FUNC( HPDF_FONT_GETFONTNAME )
{
   zh_retc( HPDF_Font_GetFontName( ( HPDF_Font ) zh_parptr( 1 ) ) );
}

/* HPDF_Font_GetEncodingName( hFont ) --> cEncoding */
ZH_FUNC( HPDF_FONT_GETENCODINGNAME )
{
   zh_retc( HPDF_Font_GetEncodingName( ( HPDF_Font ) zh_parptr( 1 ) ) );
}

/* HPDF_Font_GetUnicodeWidth( hFont, hUnicode ) --> nCharWidth */
ZH_FUNC( HPDF_FONT_GETUNICODEWIDTH )
{
   zh_retnl( ( long ) HPDF_Font_GetUnicodeWidth( ( HPDF_Font ) zh_parptr( 1 ), ( HPDF_UNICODE ) zh_parni( 2 ) ) );
}

/* HPDF_Font_GetBBox( hFont ) --> aRect */
ZH_FUNC( HPDF_FONT_GETBBOX )
{
   HPDF_Box rc;
   PZH_ITEM info = zh_itemArrayNew( 4 );

   rc = HPDF_Font_GetBBox( ( HPDF_Font ) zh_parptr( 1 ) );

   zh_arraySetND( info, 1, ( double ) rc.left );
   zh_arraySetND( info, 2, ( double ) rc.top );
   zh_arraySetND( info, 3, ( double ) rc.right );
   zh_arraySetND( info, 4, ( double ) rc.bottom );

   zh_itemReturnRelease( info );
}

/* HPDF_Font_GetAscent( hFont ) --> nAscent */
ZH_FUNC( HPDF_FONT_GETASCENT )
{
   zh_retni( ( int ) HPDF_Font_GetAscent( ( HPDF_Font ) zh_parptr( 1 ) ) );
}

/* HPDF_Font_GetDescent( hFont ) --> nDescent */
ZH_FUNC( HPDF_FONT_GETDESCENT )
{
   zh_retni( ( int ) HPDF_Font_GetDescent( ( HPDF_Font ) zh_parptr( 1 ) ) );
}

/* HPDF_Font_GetXHeight( hFont ) --> nXHeight */
ZH_FUNC( HPDF_FONT_GETXHEIGHT )
{
   zh_retnl( ( long ) HPDF_Font_GetXHeight( ( HPDF_Font ) zh_parptr( 1 ) ) );
}

/* HPDF_Font_GetCapHeight( hFont ) --> nCapsHeight */
ZH_FUNC( HPDF_FONT_GETCAPHEIGHT )
{
   zh_retnl( ( long ) HPDF_Font_GetCapHeight( ( HPDF_Font ) zh_parptr( 1 ) ) );
}

/* HPDF_Font_TextWidth( hFont, cText, nWidth ) --> aTextWidth[] { nNumChars, nNumWords, nWidth, nNumSpace } */
ZH_FUNC( HPDF_FONT_TEXTWIDTH )
{
   HPDF_TextWidth tw;
   PZH_ITEM       info = zh_itemArrayNew( 4 );

   tw = HPDF_Font_TextWidth( ( HPDF_Font ) zh_parptr( 1 ), ( const HPDF_BYTE * ) zh_parc( 2 ), zh_parni( 3 ) );

   zh_arraySetNI( info, 1, tw.numchars );
   zh_arraySetNI( info, 2, tw.numwords );
   zh_arraySetNI( info, 3, tw.width    );
   zh_arraySetNI( info, 4, tw.numspace );

   zh_itemReturnRelease( info );
}

/* HPDF_Font_MeasureText( hFont, cText, nTextLen, nWidth, nFontSize, nCharSpace, nWordSpace, lWordWrap ) --> nByteLengthTobeIncludedInWidth */
ZH_FUNC( HPDF_FONT_MEASURETEXT )
{
   zh_retni( HPDF_Font_MeasureText( ( HPDF_Font ) zh_parptr( 1 ),
                                    ( const HPDF_BYTE * ) zh_parc( 2 ),
                                    zh_parni( 3 ),
                                    ( HPDF_REAL ) zh_parnd( 4 ),
                                    ( HPDF_REAL ) zh_parnd( 5 ),
                                    ( HPDF_REAL ) zh_parnd( 6 ),
                                    ( HPDF_REAL ) zh_parnd( 7 ),
                                    zh_parl( 8 ) ? HPDF_TRUE : HPDF_FALSE,
                                    NULL ) );
}

/* --- ENCODING --- */

/* HPDF_Encoder_GetType( hEncoder ) --> nEncoderType
       nEncoderType ==
   HPDF_ENCODER_TYPE_SINGLE_BYTE      1    This encoder is an encoder for single byte characters.
   HPDF_ENCODER_TYPE_DOUBLE_BYTE      2    This encoder is an encoder for multi byte characters.
   HPDF_ENCODER_TYPE_UNINITIALIZED    3    This encoder is uninitialized. (May be it is an encoder for multi byte characters.)
   HPDF_ENCODER_UNKNOWN               4    Invalid encoder.
 */
ZH_FUNC( HPDF_ENCODER_GETTYPE )
{
   zh_retni( ( int ) HPDF_Encoder_GetType( ( HPDF_Encoder ) zh_parptr( 1 ) ) );
}

/* HPDF_Encoder_GetByteType( hEncoder, cText, nIndex ) --> nByteType
       nByteType
   HPDF_BYTE_TYPE_SINGLE     1     Single byte character.
   HPDF_BYTE_TYPE_LEAD       2     Lead byte of a double-byte character.
   HPDF_BYTE_TYPE_TRIAL      3     Trailing byte of a double-byte character.
   HPDF_BYTE_TYPE_UNKNOWN    4     Invalid encoder or cannot judge the byte type.
 */
ZH_FUNC( HPDF_ENCODER_GETBYTETYPE )
{
   zh_retni( ( int ) HPDF_Encoder_GetByteType( ( HPDF_Encoder ) zh_parptr( 1 ), zh_parc( 2 ), zh_parni( 3 ) ) );
}

/* HPDF_Encoder_GetUnicode( hEncoder, nCode ) --> nUnicode */
ZH_FUNC( HPDF_ENCODER_GETUNICODE )
{
   zh_retni( ( int ) HPDF_Encoder_GetUnicode( ( HPDF_Encoder ) zh_parptr( 1 ), ( HPDF_UINT16 ) zh_parni( 2 ) ) );
}

/* HPDF_Encoder_GetWritingMode( hEncoder ) --> nWriteMode
       nWriteMode ==
   HPDF_WMODE_HORIZONTAL    1    horizontal writing mode.
   HPDF_WMODE_VERTICAL      2    vertical writing mode;
 */
ZH_FUNC( HPDF_ENCODER_GETWRITINGMODE )
{
   zh_retni( ( int ) HPDF_Encoder_GetWritingMode( ( HPDF_Encoder ) zh_parptr( 1 ) ) );
}

/* --- OUTLINE --- */

/* HPDF_Outline_SetOpened( hOutline, lShowOpened ) --> hStatus */
ZH_FUNC( HPDF_OUTLINE_SETOPENED )
{
   zh_retnl( ( long ) HPDF_Outline_SetOpened( ( HPDF_Outline ) zh_parptr( 1 ), zh_parl( 2 ) ? HPDF_TRUE : HPDF_FALSE ) );
}

/* HPDF_Outline_SetDestination( hOutline, hDestn ) --> hStatus */
ZH_FUNC( HPDF_OUTLINE_SETDESTINATION )
{
   zh_retnl( ( long ) HPDF_Outline_SetDestination( ( HPDF_Outline ) zh_parptr( 1 ), ( HPDF_Destination ) zh_parptr( 2 ) ) );
}

/* --- DESTINATION --- */

/* HPDF_Destination_SetXYZ( hDestn, nLeft, nTop, nZoom ) --> hStatus */
ZH_FUNC( HPDF_DESTINATION_SETXYZ )
{
   zh_retnl( ( long ) HPDF_Destination_SetXYZ( ( HPDF_Destination ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ) ) );
}

/* HPDF_Destination_SetFit( hDestn ) --> hStatus */
ZH_FUNC( HPDF_DESTINATION_SETFIT )
{
   zh_retnl( ( long ) HPDF_Destination_SetFit( ( HPDF_Destination ) zh_parptr( 1 ) ) );
}

/* HPDF_Destination_SetFitH( hDestn, nTop ) --> hStatus */
ZH_FUNC( HPDF_DESTINATION_SETFITH )
{
   zh_retnl( ( long ) HPDF_Destination_SetFitH( ( HPDF_Destination ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_Destination_SetFitV( hDestn, nLeft ) --> hStatus */
ZH_FUNC( HPDF_DESTINATION_SETFITV )
{
   zh_retnl( ( long ) HPDF_Destination_SetFitV( ( HPDF_Destination ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_Destination_SetFitR( hDestn, nLeft, nBottom, nRight, nTop ) --> hStatus */
ZH_FUNC( HPDF_DESTINATION_SETFITR )
{
   zh_retnl( ( long ) HPDF_Destination_SetFitR( ( HPDF_Destination ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ), ( HPDF_REAL ) zh_parnd( 3 ), ( HPDF_REAL ) zh_parnd( 4 ), ( HPDF_REAL ) zh_parnd( 5 ) ) );
}

/* HPDF_Destination_SetFitB( hDestn ) --> hStatus */
ZH_FUNC( HPDF_DESTINATION_SETFITB )
{
   zh_retnl( ( long ) HPDF_Destination_SetFitB( ( HPDF_Destination ) zh_parptr( 1 ) ) );
}

/* HPDF_Destination_SetFitBH( hDestn, nTop ) --> hStatus */
ZH_FUNC( HPDF_DESTINATION_SETFITBH )
{
   zh_retnl( ( long ) HPDF_Destination_SetFitBH( ( HPDF_Destination ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_Destination_SetFitBV( hDestn, nTop ) --> hStatus */
ZH_FUNC( HPDF_DESTINATION_SETFITBV )
{
   zh_retnl( ( long ) HPDF_Destination_SetFitBV( ( HPDF_Destination ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* --- ExtGState --- */

/* HPDF_ExtGState_SetAlphaStroke( hGState, nValue ) --> hStatus */
ZH_FUNC( HPDF_EXTGSTATE_SETALPHASTROKE )
{
   zh_retnl( ( long ) HPDF_ExtGState_SetAlphaStroke( ( HPDF_ExtGState ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_ExtGState_SetAlphaFill( hGState, nValue ) --> hStatus */
ZH_FUNC( HPDF_EXTGSTATE_SETALPHAFILL )
{
   zh_retnl( ( long ) HPDF_ExtGState_SetAlphaFill( ( HPDF_ExtGState ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
}

/* HPDF_ExtGState_SetBlendMode( hGState, nBlendMode ) --> hStatus
      nBlendMode ==
   HPDF_BM_NORMAL
   HPDF_BM_MULTIPLY
   HPDF_BM_SCREEN
   HPDF_BM_OVERLAY
   HPDF_BM_DARKEN
   HPDF_BM_LIGHTEN
   HPDF_BM_COLOR_DODGE
   HPDF_BM_COLOR_BUM
   HPDF_BM_HARD_LIGHT
   HPDF_BM_SOFT_LIGHT
   HPDF_BM_DIFFERENCE
   HPDF_BM_EXCLUSHON
 */
ZH_FUNC( HPDF_EXTGSTATE_SETBLENDMODE )
{
   zh_retnl( ( long ) HPDF_ExtGState_SetBlendMode( ( HPDF_ExtGState ) zh_parptr( 1 ), ( HPDF_BlendMode ) zh_parni( 2 ) ) );
}

ZH_FUNC( HPDF_VERSION_TEXT )
{
   zh_retc_const( HPDF_VERSION_TEXT );
}

/* --- New Functions in LibHaru 2.2.0 --- */

/* HPDF_GetContents( hDoc, @cBuffer ) --> nStatus */
ZH_FUNC( HPDF_GETCONTENTS )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_UINT32 size = ( HPDF_UINT32 ) zh_parclen( 2 );
   HPDF_BYTE * buffer;

   if( size < 1024 )
      size = 1024;

   buffer = ( HPDF_BYTE * ) zh_xgrab( size + 1 );

   zh_retnl( ( long ) HPDF_GetContents( zh_HPDF_Doc_par( 1 ), buffer, &size ) );

   if( ! zh_storclen_buffer( ( char * ) buffer, size, 2 ) )
      zh_xfree( buffer );
#else
   zh_storc( NULL, 2 );
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_CheckError( pError ) --> nStatus */
ZH_FUNC( HPDF_CHECKERROR )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_Error error = ( HPDF_Error ) zh_parptr( 1 );

   if( error )
      zh_retnl( ( long ) HPDF_CheckError( error ) );
   else
      zh_retnl( ZH_HPDF_BADPARAM );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_Page_SetZoom( hPage, nZoom ) --> nStatus */
ZH_FUNC( HPDF_PAGE_SETZOOM )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   zh_retnl( ( long ) HPDF_Page_SetZoom( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_REAL ) zh_parnd( 2 ) ) );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

/* HPDF_Page_Create3DView( hPage, pU3d, pAnnot3d, cName ) --> pDict */
ZH_FUNC( HPDF_PAGE_CREATE3DVIEW )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   zh_retptr( ( HPDF_Dict ) HPDF_Page_Create3DView( ( HPDF_Page ) zh_parptr( 1 ), ( HPDF_U3D ) zh_parptr( 2 ), ( HPDF_Annotation ) zh_parptr( 3 ), zh_parc( 4 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_AttachFile( hDoc, cFile ) --> pEmbeddedFile */
ZH_FUNC( HPDF_ATTACHFILE )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   zh_retptr( ( HPDF_EmbeddedFile ) HPDF_AttachFile( zh_HPDF_Doc_par( 1 ), zh_parc( 2 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_ICC_LoadIccFromMem( hDoc, pMMgr, pICCDataStream, pXref, nNumComponent ) --> pOutputIntent */
ZH_FUNC( HPDF_ICC_LOADICCFROMMEM )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   HPDF_MMgr mmgr = ( HPDF_MMgr ) zh_parptr( 2 );

   if( mmgr )
      zh_retptr( ( HPDF_OutputIntent ) HPDF_ICC_LoadIccFromMem( zh_HPDF_Doc_par( 1 ), mmgr, ( HPDF_Stream ) zh_parptr( 3 ), ( HPDF_Xref ) zh_parptr( 4 ), zh_parni( 5 ) ) );
   else
      zh_retptr( NULL );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_LoadIccProfileFromFile( hDoc, cICCFileName, nNumComponent ) --> pOutputIntent */
ZH_FUNC( HPDF_LOADICCPROFILEFROMFILE )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   zh_retptr( ( HPDF_OutputIntent ) HPDF_LoadIccProfileFromFile( zh_HPDF_Doc_par( 1 ), zh_parc( 2 ), zh_parni( 3 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_UseUTFEncodings( hDoc ) --> nStatus */
ZH_FUNC( HPDF_USEUTFENCODINGS )
{
#if ZH_HPDF_VERS( 2, 3, 0 )
   zh_retnl( HPDF_UseUTFEncodings( zh_HPDF_Doc_par( 1 ) ) );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}

ZH_FUNC( ZH_HPDF_VERSION )
{
   zh_storni( HPDF_MAJOR_VERSION, 1 );
   zh_storni( HPDF_MINOR_VERSION, 2 );
   zh_storni( HPDF_BUGFIX_VERSION, 3 );
}
