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

/* HPDF_LoadPngImageFromFile( hDoc, cPNGFileName ) --> hImage */
ZH_FUNC( HPDF_LOADPNGIMAGEFROMFILE )
{
   zh_retptr( ( void * ) HPDF_LoadPngImageFromFile( zh_HPDF_Doc_par( 1 ), zh_parc( 2 ) ) );
}

/* HPDF_LoadPngImageFromFile2( hDoc, cPNGFileName ) --> hImage */
ZH_FUNC( HPDF_LOADPNGIMAGEFROMFILE2 )
{
   zh_retptr( ( void * ) HPDF_LoadPngImageFromFile2( zh_HPDF_Doc_par( 1 ), zh_parc( 2 ) ) );
}

/* HPDF_LoadRawImageFromFile( hDoc, cImageFileName, nWidth, nHeight, nColorSpace ) --> hImage
       nColorSpace
   HPDF_CS_DEVICE_GRAY
   HPDF_CS_DEVICE_RGB
   HPDF_CS_DEVICE_CMYK
 */
ZH_FUNC( HPDF_LOADRAWIMAGEFROMFILE )
{
   zh_retptr( ( void * ) HPDF_LoadRawImageFromFile( zh_HPDF_Doc_par( 1 ), zh_parc( 2 ), zh_parni( 3 ), zh_parni( 4 ), ( HPDF_ColorSpace ) zh_parni( 5 ) ) );
}

/* HPDF_LoadRawImageFromMem( hDoc, cBuffer, nWidth, nHeight, nColorSpace, nBitsPerComponents ) --> hImage */
ZH_FUNC( HPDF_LOADRAWIMAGEFROMMEM )
{
   zh_retptr( ( void * ) HPDF_LoadRawImageFromMem( zh_HPDF_Doc_par( 1 ), ( const HPDF_BYTE * ) zh_parc( 2 ), zh_parni( 3 ), zh_parni( 4 ), ( HPDF_ColorSpace ) zh_parni( 5 ), zh_parni( 6 ) ) );
}

/* HPDF_LoadJpegImageFromFile( hDoc, cHPEGFileName ) --> hImage */
ZH_FUNC( HPDF_LOADJPEGIMAGEFROMFILE )
{
   zh_retptr( ( void * ) HPDF_LoadJpegImageFromFile( zh_HPDF_Doc_par( 1 ), zh_parc( 2 ) ) );
}

/* HPDF_EXPORT(HPDF_Image)
   HPDF_LoadPngImageFromMem  (HPDF_Doc     pdf,
                    const HPDF_BYTE    *buffer,
                          HPDF_UINT     size);
 */
ZH_FUNC( HPDF_LOADPNGIMAGEFROMMEM )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   zh_retptr( ( HPDF_Image ) HPDF_LoadPngImageFromMem( zh_HPDF_Doc_par( 1 ),
                                          ( const HPDF_BYTE * ) zh_parcx( 2 ),
                                          ( HPDF_UINT ) zh_parclen( 2 ) ) );
#else
   zh_retptr( NULL );
#endif
}
/* HPDF_EXPORT(HPDF_Image)
   HPDF_LoadJpegImageFromMem   (HPDF_Doc      pdf,
                      const HPDF_BYTE     *buffer,
                            HPDF_UINT      size);
 */
ZH_FUNC( HPDF_LOADJPEGIMAGEFROMMEM )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   zh_retptr( ( HPDF_Image ) HPDF_LoadJpegImageFromMem( zh_HPDF_Doc_par( 1 ),
                                          ( const HPDF_BYTE * ) zh_parcx( 2 ),
                                          ( HPDF_UINT ) zh_parclen( 2 ) ) );
#else
   zh_retptr( NULL );
#endif
}

/* HPDF_Image_GetSize( hImage ) --> aSize[] { nW, nH } */
ZH_FUNC( HPDF_IMAGE_GETSIZE )
{
   HPDF_Point pt = HPDF_Image_GetSize( ( HPDF_Image ) zh_parptr( 1 ) );

   PZH_ITEM info = zh_itemArrayNew( 2 );

   zh_arraySetND( info, 1, ( double ) pt.x );
   zh_arraySetND( info, 2, ( double ) pt.y );

   zh_itemReturnRelease( info );
}

/* HPDF_Image_GetWidth( hImage ) --> nWidth */
ZH_FUNC( HPDF_IMAGE_GETWIDTH )
{
   zh_retni( HPDF_Image_GetWidth( ( HPDF_Image ) zh_parptr( 1 ) ) );
}

/* HPDF_Image_GetHeight( hImage ) --> nHeight */
ZH_FUNC( HPDF_IMAGE_GETHEIGHT )
{
   zh_retni( HPDF_Image_GetHeight( ( HPDF_Image ) zh_parptr( 1 ) ) );
}

/* HPDF_Image_GetBitsPerComponent( hImage ) --> nBitsPerComponent */
ZH_FUNC( HPDF_IMAGE_GETBITSPERCOMPONENT )
{
   zh_retni( HPDF_Image_GetBitsPerComponent( ( HPDF_Image ) zh_parptr( 1 ) ) );
}

/* HPDF_Image_GetColorSpace( hImage ) --> nColorSpace */
ZH_FUNC( HPDF_IMAGE_GETCOLORSPACE )
{
   HPDF_Image image = ( HPDF_Image ) zh_parptr( 1 );

   if( image )
      zh_retc( HPDF_Image_GetColorSpace( image ) );
   else
      zh_retc_null();
}

/* HPDF_Image_SetColorMask( hImage, nRGB_R_Min, nRGB_R_Max, nRGB_G_Min, nRGB_G_Max, nRGB_B_Min, nRGB_B_Max ) */
ZH_FUNC( HPDF_IMAGE_SETCOLORMASK )
{
   zh_retnl( ( long ) HPDF_Image_SetColorMask( ( HPDF_Image ) zh_parptr( 1 ),
                                               zh_parni( 2 ),
                                               zh_parni( 3 ),
                                               zh_parni( 4 ),
                                               zh_parni( 5 ),
                                               zh_parni( 6 ),
                                               zh_parni( 7 ) ) );
}

/* HPDF_Image_SetMaskImage( hImage, hImageMask ) --> hStatus */
ZH_FUNC( HPDF_IMAGE_SETMASKIMAGE )
{
   zh_retnl( ( long ) HPDF_Image_SetMaskImage( ( HPDF_Image ) zh_parptr( 1 ), ( HPDF_Image ) zh_parptr( 2 ) ) );
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Image_AddSMask  (HPDF_Image    image,
                      HPDF_Image    smask);
 */
ZH_FUNC( HPDF_IMAGE_ADDSMASK )
{
#if ZH_HPDF_VERS( 2, 2, 0 )
   zh_retnl( ( long ) HPDF_Image_AddSMask( ( HPDF_Image ) zh_parptr( 1 ), ( HPDF_Image ) zh_parptr( 2 ) ) );
#else
   zh_retnl( ZH_HPDF_NOTSUPPORTED );
#endif
}
