/*
 * Header file for the CodePages API
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * Copyright 2009 Przemyslaw Czerpak
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

#ifndef ZH_APICDP_H_
#define ZH_APICDP_H_

#include "zh_api.h"
#include "zh_init.h"

ZH_EXTERN_BEGIN

/* This hack is needed to force preprocessing if id is also a macro */
#define ZH_CODEPAGE_REQUEST( id )      ZH_CODEPAGE_REQUEST_( id )
#define ZH_CODEPAGE_REQUEST_( id )     ZH_FUNC_EXTERN( ZH_CODEPAGE_##id ); \
                                       extern void zh_codepage_ForceLink_##id( void ); \
                                       void zh_codepage_ForceLink_##id( void ) \
                                       { \
                                          ZH_FUNC_EXEC( ZH_CODEPAGE_##id ); \
                                       }
#define ZH_CODEPAGE_ANNOUNCE( id )     ZH_CODEPAGE_ANNOUNCE_( id )
#define ZH_CODEPAGE_ANNOUNCE_( id )    ZH_FUNC( ZH_CODEPAGE_##id ) {}


/* forward declaration */
struct _ZH_CODEPAGE;

#define _PZH_CODEPAGE       struct _ZH_CODEPAGE *

#define ZH_CODEPAGE_CHAR_GET( c, s, n, i, w )         (c)->wcharGet( c, s, n, i, w )
#define ZH_CODEPAGE_CHAR_PUT( c, s, n, i, w )         (c)->wcharPut( c, s, n, i, w )
#define ZH_CODEPAGE_CHAR_LEN( c, w )                  (c)->wcharLen( c, w )
#define ZH_CODEPAGE_CHAR_UPPER( c, w )                (c)->wcharUpper( c, w )
#define ZH_CODEPAGE_CHAR_LOWER( c, w )                (c)->wcharLower( c, w )
#define ZH_CODEPAGE_CHAR_FLAGS( c, w )                (c)->wcharFlags( c, w )
#define ZH_CODEPAGE_CHAR_CMP( c, s1, n1, s2, n2, e )  (c)->wcharCmp( c, s1, n1, s2, n2, e )
#define ZH_CODEPAGE_CHAR_CMPI( c, s1, n1, s2, n2, e ) (c)->wcharCmpI( c, s1, n1, s2, n2, e )

#define ZH_CODEPAGE_GET_FUNC( func ) ZH_BOOL func( _PZH_CODEPAGE cdp, const char * pSrc, ZH_SIZE nLen, ZH_SIZE * pnIndex, ZH_WCHAR * wc )
typedef ZH_CODEPAGE_GET_FUNC( ( * PZH_CODEPAGE_GET_FUNC ) );

#define ZH_CODEPAGE_PUT_FUNC( func ) ZH_BOOL func( _PZH_CODEPAGE cdp, char * pDst, ZH_SIZE nLen, ZH_SIZE * pnIndex, ZH_WCHAR wc )
typedef ZH_CODEPAGE_PUT_FUNC( ( * PZH_CODEPAGE_PUT_FUNC ) );

#define ZH_CODEPAGE_LEN_FUNC( func ) int func( _PZH_CODEPAGE cdp, ZH_WCHAR wc )
typedef ZH_CODEPAGE_LEN_FUNC( ( * PZH_CODEPAGE_LEN_FUNC ) );

#define ZH_CODEPAGE_UPPER_FUNC( func ) ZH_WCHAR func( _PZH_CODEPAGE cdp, ZH_WCHAR wc )
typedef ZH_CODEPAGE_UPPER_FUNC( ( * PZH_CODEPAGE_UPPER_FUNC ) );

#define ZH_CODEPAGE_LOWER_FUNC( func ) ZH_WCHAR func( _PZH_CODEPAGE cdp, ZH_WCHAR wc )
typedef ZH_CODEPAGE_LOWER_FUNC( ( * PZH_CODEPAGE_LOWER_FUNC ) );

#define ZH_CODEPAGE_FLAGS_FUNC( func ) int func( _PZH_CODEPAGE cdp, ZH_WCHAR wc )
typedef ZH_CODEPAGE_FLAGS_FUNC( ( * PZH_CODEPAGE_FLAGS_FUNC ) );

#define ZH_CODEPAGE_CMP_FUNC( func ) int func( _PZH_CODEPAGE cdp, const char * szFirst, ZH_SIZE nLenFirst, const char * szSecond, ZH_SIZE nLenSecond, ZH_BOOL fExact )
typedef ZH_CODEPAGE_CMP_FUNC( ( * PZH_CODEPAGE_CMP_FUNC ) );


typedef struct _ZH_UNITABLE
{
   const char *      uniID;
   const ZH_WCHAR *  uniCodes;
   ZH_UCHAR *        uniTrans;
   ZH_WCHAR          wcMax;
} ZH_UNITABLE, * PZH_UNITABLE;

typedef struct _ZH_MULTICHAR
{
   char     cFirst[ 2 ];
   char     cLast[ 2 ];
   int      sortUp;
   int      sortLo;
   ZH_WCHAR wcUp;
   ZH_WCHAR wcLo;
} ZH_MULTICHAR, * PZH_MULTICHAR;

typedef struct _ZH_CODEPAGE
{
   const char *            id;
   const char *            info;
   PZH_UNITABLE            uniTable;
   const ZH_UCHAR *        flags;
   const ZH_UCHAR *        upper;
   const ZH_UCHAR *        lower;
   const ZH_UCHAR *        sort;
   const ZH_UCHAR *        acc;
   int                     nACSort;
   int                     type;
   PZH_CODEPAGE_GET_FUNC        wcharGet;
   PZH_CODEPAGE_PUT_FUNC        wcharPut;
   PZH_CODEPAGE_LEN_FUNC        wcharLen;
   PZH_CODEPAGE_UPPER_FUNC      wcharUpper;
   PZH_CODEPAGE_LOWER_FUNC      wcharLower;
   PZH_CODEPAGE_FLAGS_FUNC      wcharFlags;
   PZH_CODEPAGE_CMP_FUNC        wcharCmp;
   PZH_CODEPAGE_CMP_FUNC        wcharCmpI;
   int                     nMulti;
   int                     nMultiUC;
   PZH_MULTICHAR           multi;
   void *                  buffer;
   struct _ZH_CODEPAGE *   next;
} ZH_CODEPAGE, * PZH_CODEPAGE;

#define ZH_CPID_437        "cp437"
#define ZH_CPID_737        "cp737"
#define ZH_CPID_775        "cp775"
#define ZH_CPID_850        "cp850"
#define ZH_CPID_852        "cp852"
#define ZH_CPID_855        "cp855"
#define ZH_CPID_857        "cp857"
#define ZH_CPID_858        "cp858"
#define ZH_CPID_860        "cp860"
#define ZH_CPID_861        "cp861"
#define ZH_CPID_862        "cp862"
#define ZH_CPID_863        "cp863"
#define ZH_CPID_864        "cp864"
#define ZH_CPID_865        "cp865"
#define ZH_CPID_866        "cp866"
#define ZH_CPID_869        "cp869"
#define ZH_CPID_874        "cp874"
#define ZH_CPID_1125       "cp1125"
#define ZH_CPID_1250       "cp1250"
#define ZH_CPID_1251       "cp1251"
#define ZH_CPID_1252       "cp1252"
#define ZH_CPID_1253       "cp1253"
#define ZH_CPID_1254       "cp1254"
#define ZH_CPID_1255       "cp1255"
#define ZH_CPID_1256       "cp1256"
#define ZH_CPID_1257       "cp1257"
#define ZH_CPID_1258       "cp1258"
#define ZH_CPID_8859_1     "iso8859-1"
#define ZH_CPID_8859_1B    "iso8859-1b"
#define ZH_CPID_8859_2     "iso8859-2"
#define ZH_CPID_8859_3     "iso8859-3"
#define ZH_CPID_8859_4     "iso8859-4"
#define ZH_CPID_8859_5     "iso8859-5"
#define ZH_CPID_8859_6     "iso8859-6"
#define ZH_CPID_8859_7     "iso8859-7"
#define ZH_CPID_8859_8     "iso8859-8"
#define ZH_CPID_8859_9     "iso8859-9"
#define ZH_CPID_8859_10    "iso8859-10"
#define ZH_CPID_8859_11    "iso8859-11"
#define ZH_CPID_8859_13    "iso8859-13"
#define ZH_CPID_8859_14    "iso8859-14"
#define ZH_CPID_8859_15    "iso8859-15"
#define ZH_CPID_8859_16    "iso8859-16"
#define ZH_CPID_KOI_8      "koi-8"
#define ZH_CPID_KOI_8U     "koi-8u"
#define ZH_CPID_KAM        "kamenicky"
#define ZH_CPID_MAZ        "mazovia"
#define ZH_CPID_MIK        "bg-mik"
#define ZH_CPID_037        "cp037"
#define ZH_CPID_424        "cp424"
#define ZH_CPID_500        "cp500"
#define ZH_CPID_856        "cp856"
#define ZH_CPID_875        "cp875"
#define ZH_CPID_1006       "cp1006"
#define ZH_CPID_1026       "cp1026"
#define ZH_CPID_10000      "macroman"
#define ZH_CPID_10006      "macgreek"
#define ZH_CPID_10007      "maccyrillic"
#define ZH_CPID_10029      "maccentraleurope"
#define ZH_CPID_10079      "maciceland"
#define ZH_CPID_10081      "macturkish"
#define ZH_CPID_ATARIST    "atarist"
#define ZH_CPID_USASCII    "us-ascii"
#define ZH_CPID_CWI2       "cwi-2"

#define ZH_UNITB_437       &zh_uniTbl_437
#define ZH_UNITB_737       &zh_uniTbl_737
#define ZH_UNITB_775       &zh_uniTbl_775
#define ZH_UNITB_850       &zh_uniTbl_850
#define ZH_UNITB_852       &zh_uniTbl_852
#define ZH_UNITB_855       &zh_uniTbl_855
#define ZH_UNITB_857       &zh_uniTbl_857
#define ZH_UNITB_858       &zh_uniTbl_858
#define ZH_UNITB_860       &zh_uniTbl_860
#define ZH_UNITB_861       &zh_uniTbl_861
#define ZH_UNITB_862       &zh_uniTbl_862
#define ZH_UNITB_863       &zh_uniTbl_863
#define ZH_UNITB_864       &zh_uniTbl_864
#define ZH_UNITB_865       &zh_uniTbl_865
#define ZH_UNITB_866       &zh_uniTbl_866
#define ZH_UNITB_869       &zh_uniTbl_869
#define ZH_UNITB_874       &zh_uniTbl_874
#define ZH_UNITB_1125      &zh_uniTbl_1125
#define ZH_UNITB_1250      &zh_uniTbl_1250
#define ZH_UNITB_1251      &zh_uniTbl_1251
#define ZH_UNITB_1252      &zh_uniTbl_1252
#define ZH_UNITB_1253      &zh_uniTbl_1253
#define ZH_UNITB_1254      &zh_uniTbl_1254
#define ZH_UNITB_1255      &zh_uniTbl_1255
#define ZH_UNITB_1256      &zh_uniTbl_1256
#define ZH_UNITB_1257      &zh_uniTbl_1257
#define ZH_UNITB_1258      &zh_uniTbl_1258
#define ZH_UNITB_8859_1    &zh_uniTbl_8859_1
#define ZH_UNITB_8859_1B   &zh_uniTbl_8859_1b
#define ZH_UNITB_8859_2    &zh_uniTbl_8859_2
#define ZH_UNITB_8859_3    &zh_uniTbl_8859_3
#define ZH_UNITB_8859_4    &zh_uniTbl_8859_4
#define ZH_UNITB_8859_5    &zh_uniTbl_8859_5
#define ZH_UNITB_8859_6    &zh_uniTbl_8859_6
#define ZH_UNITB_8859_7    &zh_uniTbl_8859_7
#define ZH_UNITB_8859_8    &zh_uniTbl_8859_8
#define ZH_UNITB_8859_9    &zh_uniTbl_8859_9
#define ZH_UNITB_8859_10   &zh_uniTbl_8859_10
#define ZH_UNITB_8859_11   &zh_uniTbl_8859_11
#define ZH_UNITB_8859_13   &zh_uniTbl_8859_13
#define ZH_UNITB_8859_14   &zh_uniTbl_8859_14
#define ZH_UNITB_8859_15   &zh_uniTbl_8859_15
#define ZH_UNITB_8859_16   &zh_uniTbl_8859_16
#define ZH_UNITB_KOI_8     &zh_uniTbl_KOI_8
#define ZH_UNITB_KOI_8U    &zh_uniTbl_KOI_8U
#define ZH_UNITB_KAM       &zh_uniTbl_kamenicky
#define ZH_UNITB_MAZ       &zh_uniTbl_mazovia
#define ZH_UNITB_MIK       &zh_uniTbl_MIK
#define ZH_UNITB_037       &zh_uniTbl_037
#define ZH_UNITB_424       &zh_uniTbl_424
#define ZH_UNITB_500       &zh_uniTbl_500
#define ZH_UNITB_856       &zh_uniTbl_856
#define ZH_UNITB_875       &zh_uniTbl_875
#define ZH_UNITB_1006      &zh_uniTbl_1006
#define ZH_UNITB_1026      &zh_uniTbl_1026
#define ZH_UNITB_10000     &zh_uniTbl_10000
#define ZH_UNITB_10006     &zh_uniTbl_10006
#define ZH_UNITB_10007     &zh_uniTbl_10007
#define ZH_UNITB_10029     &zh_uniTbl_10029
#define ZH_UNITB_10079     &zh_uniTbl_10079
#define ZH_UNITB_10081     &zh_uniTbl_10081
#define ZH_UNITB_USASCII   &zh_uniTbl_USASCII
#define ZH_UNITB_CWI2      &zh_uniTbl_CWI2
#define ZH_UNITB_UNDEF     NULL /* ((PZH_UNITABLE) (-1)) */

extern ZH_UNITABLE zh_uniTbl_437;
extern ZH_UNITABLE zh_uniTbl_737;
extern ZH_UNITABLE zh_uniTbl_775;
extern ZH_UNITABLE zh_uniTbl_850;
extern ZH_UNITABLE zh_uniTbl_852;
extern ZH_UNITABLE zh_uniTbl_855;
extern ZH_UNITABLE zh_uniTbl_857;
extern ZH_UNITABLE zh_uniTbl_858;
extern ZH_UNITABLE zh_uniTbl_860;
extern ZH_UNITABLE zh_uniTbl_861;
extern ZH_UNITABLE zh_uniTbl_862;
extern ZH_UNITABLE zh_uniTbl_863;
extern ZH_UNITABLE zh_uniTbl_864;
extern ZH_UNITABLE zh_uniTbl_865;
extern ZH_UNITABLE zh_uniTbl_866;
extern ZH_UNITABLE zh_uniTbl_869;
extern ZH_UNITABLE zh_uniTbl_874;
extern ZH_UNITABLE zh_uniTbl_1125;
extern ZH_UNITABLE zh_uniTbl_1250;
extern ZH_UNITABLE zh_uniTbl_1251;
extern ZH_UNITABLE zh_uniTbl_1252;
extern ZH_UNITABLE zh_uniTbl_1253;
extern ZH_UNITABLE zh_uniTbl_1254;
extern ZH_UNITABLE zh_uniTbl_1255;
extern ZH_UNITABLE zh_uniTbl_1256;
extern ZH_UNITABLE zh_uniTbl_1257;
extern ZH_UNITABLE zh_uniTbl_1258;
extern ZH_UNITABLE zh_uniTbl_8859_1;
extern ZH_UNITABLE zh_uniTbl_8859_1b;
extern ZH_UNITABLE zh_uniTbl_8859_2;
extern ZH_UNITABLE zh_uniTbl_8859_3;
extern ZH_UNITABLE zh_uniTbl_8859_4;
extern ZH_UNITABLE zh_uniTbl_8859_5;
extern ZH_UNITABLE zh_uniTbl_8859_6;
extern ZH_UNITABLE zh_uniTbl_8859_7;
extern ZH_UNITABLE zh_uniTbl_8859_8;
extern ZH_UNITABLE zh_uniTbl_8859_9;
extern ZH_UNITABLE zh_uniTbl_8859_10;
extern ZH_UNITABLE zh_uniTbl_8859_11;
extern ZH_UNITABLE zh_uniTbl_8859_13;
extern ZH_UNITABLE zh_uniTbl_8859_14;
extern ZH_UNITABLE zh_uniTbl_8859_15;
extern ZH_UNITABLE zh_uniTbl_8859_16;
extern ZH_UNITABLE zh_uniTbl_KOI_8;
extern ZH_UNITABLE zh_uniTbl_KOI_8U;
extern ZH_UNITABLE zh_uniTbl_kamenicky;
extern ZH_UNITABLE zh_uniTbl_mazovia;
extern ZH_UNITABLE zh_uniTbl_MIK;
extern ZH_UNITABLE zh_uniTbl_037;
extern ZH_UNITABLE zh_uniTbl_424;
extern ZH_UNITABLE zh_uniTbl_500;
extern ZH_UNITABLE zh_uniTbl_856;
extern ZH_UNITABLE zh_uniTbl_875;
extern ZH_UNITABLE zh_uniTbl_1006;
extern ZH_UNITABLE zh_uniTbl_1026;
extern ZH_UNITABLE zh_uniTbl_10000;
extern ZH_UNITABLE zh_uniTbl_10006;
extern ZH_UNITABLE zh_uniTbl_10007;
extern ZH_UNITABLE zh_uniTbl_10029;
extern ZH_UNITABLE zh_uniTbl_10079;
extern ZH_UNITABLE zh_uniTbl_10081;
extern ZH_UNITABLE zh_uniTbl_USASCII;
extern ZH_UNITABLE zh_uniTbl_CWI2;

extern ZH_EXPORT PZH_CODEPAGE zh_vmCodepage( void );
extern ZH_EXPORT void         zh_vmSetCDP( PZH_CODEPAGE pCDP );

/* character flags */
#define ZH_CODEPAGE_DIGIT    0x01
#define ZH_CODEPAGE_ALPHA    0x02
#define ZH_CODEPAGE_LOWER    0x04
#define ZH_CODEPAGE_UPPER    0x08
#define ZH_CODEPAGE_MULTI1   0x10
#define ZH_CODEPAGE_MULTI2   0x20

/* accented character sorting */
#define ZH_CODEPAGE_ACSORT_NONE          0     /* no special sorting for accented
                                             characters */
#define ZH_CODEPAGE_ACSORT_EQUAL         1     /* accented characters have the same
                                             weight as corresponding unaccented
                                             ones */
#define ZH_CODEPAGE_ACSORT_INTERLEAVED   2     /* accented characters sort after
                                             their unaccented counterparts only
                                             if the unaccented versions of all
                                             characters being compared are the
                                             same ( interleaving ) */

/* letter case sensitive sorting */
#define ZH_CODEPAGE_CSSORT_UPLO          0     /* upper letters first then lower
                                             ones */
#define ZH_CODEPAGE_CSSORT_MIXED         1     /* upper and lower letters are
                                             mixed */
#define ZH_CODEPAGE_CSSORT_IGNORE        2     /* ignore case */

/* byte order */
#define ZH_CODEPAGE_ENDIAN_NATIVE        0
#define ZH_CODEPAGE_ENDIAN_LITTLE        1
#define ZH_CODEPAGE_ENDIAN_BIG           2

/* codepage types */
#define ZH_CODEPAGE_TYPE_CUSTOM          0x0001
#define ZH_CODEPAGE_TYPE_CHARIDX         0x0002
#define ZH_CODEPAGE_TYPE_CHARUNI         0x0004
#define ZH_CODEPAGE_TYPE_BINSORT         0x0008
#define ZH_CODEPAGE_TYPE_UTF8            0x0010

/* maximal size of unicode character in 'char' representation for buffers
 * To encode all ISO 10646 Universal Character Set (UCS) values (characters
 * can be encoded in 31-bit code space) in UTF-8 we need 6 bytes.
 * UC2 characters (16-bit) encoded in UTF-8 needs 3 bytes.
 * 8 seems to be a little bit redundant and large enough for all encodings.
 * In theory some other encodings may need more bytes but I do not know any
 * one used in practice. [druzus]
 */
#define ZH_MAX_CHAR_LEN             8

/* codepage uses simple binary sorting */
#define ZH_CODEPAGE_ISBINSORT( cdp )     ( ( ( cdp )->type & ZH_CODEPAGE_TYPE_BINSORT ) != 0 )
/* codepage uses custom string decoding */
#define ZH_CODEPAGE_ISCUSTOM( cdp )      ( ( ( cdp )->type & ZH_CODEPAGE_TYPE_CUSTOM ) != 0 )
/* codepage use character indexes instead of bytes ones */
#define ZH_CODEPAGE_ISCHARIDX( cdp )     ( ( ( cdp )->type & ZH_CODEPAGE_TYPE_CHARIDX ) != 0 )
/* Chr(), Asc() and similar functions operates on Unicode values instead of bytes */
#define ZH_CODEPAGE_ISCHARUNI( cdp )     ( ( ( cdp )->type & ZH_CODEPAGE_TYPE_CHARUNI ) != 0 )
/* codepage uses UTF-8 encoding */
#define ZH_CODEPAGE_ISUTF8( cdp )        ( ( ( cdp )->type & ZH_CODEPAGE_TYPE_UTF8 ) != 0 )

#define zh_cdpGetID( cdp )          ( ( cdp )->id )

extern ZH_EXPORT ZH_BOOL      zh_cdpRegisterRaw( PZH_CODEPAGE cdp );
extern ZH_EXPORT ZH_BOOL      zh_cdpRegisterNew( const char * id,
                                                 const char * info,
                                                 PZH_UNITABLE uniTable,
                                                 const char * pszUpper,
                                                 const char * pszLower,
                                                 unsigned int nACSort,
                                                 unsigned int nCaseSort,
                                                 ZH_BOOL fUtf8 );
extern ZH_EXPORT void         zh_cdpBuildTransTable( PZH_UNITABLE uniTable );
extern ZH_EXPORT void         zh_cdpReleaseAll( void );
extern ZH_EXPORT const char * zh_cdpID( void );
extern ZH_EXPORT ZH_BOOL      zh_cdpIsUTF8( PZH_CODEPAGE cdp );
extern ZH_EXPORT PZH_CODEPAGE codepageSelect( PZH_CODEPAGE cdp );
extern ZH_EXPORT const char * codepageSelectID( const char * id );
extern ZH_EXPORT PZH_CODEPAGE zh_cdpFind( const char * id );
extern ZH_EXPORT PZH_CODEPAGE zh_cdpFindExt( const char * id );
extern ZH_EXPORT const char ** zh_codepageList( void ); /* Caller must release the pointer */


extern ZH_EXPORT ZH_BOOL      zh_cdpIsDigit( PZH_CODEPAGE cdp, int iChar );
extern ZH_EXPORT ZH_BOOL      zh_cdpIsAlpha( PZH_CODEPAGE cdp, int iChar );
extern ZH_EXPORT ZH_BOOL      zh_cdpIsLower( PZH_CODEPAGE cdp, int iChar );
extern ZH_EXPORT ZH_BOOL      zh_cdpIsUpper( PZH_CODEPAGE cdp, int iChar );

extern ZH_EXPORT int          zh_cdpcmp( const char * szFirst, ZH_SIZE nLenFirst, const char * szSecond, ZH_SIZE nLenSecond, PZH_CODEPAGE cdp, ZH_BOOL fExact );
extern ZH_EXPORT int          zh_cdpicmp( const char * szFirst, ZH_SIZE nLenFirst, const char * szSecond, ZH_SIZE nLenSecond, PZH_CODEPAGE cdp, ZH_BOOL fExact );
extern ZH_EXPORT const ZH_UCHAR * zh_cdpGetSortTab( PZH_CODEPAGE cdp );

extern ZH_EXPORT char *       zh_cdpDup( const char *, PZH_CODEPAGE, PZH_CODEPAGE );
extern ZH_EXPORT char *       zh_cdpDupn( const char *, ZH_SIZE, PZH_CODEPAGE, PZH_CODEPAGE );
extern ZH_EXPORT char *       zh_cdpnDup( const char *, ZH_SIZE *, PZH_CODEPAGE, PZH_CODEPAGE );
extern ZH_EXPORT const char * zh_cdpnDup2( const char *, ZH_SIZE, char *, ZH_SIZE *, PZH_CODEPAGE, PZH_CODEPAGE );
extern ZH_EXPORT const char * zh_cdpnDup3( const char *, ZH_SIZE, char *, ZH_SIZE *, char **, ZH_SIZE *, PZH_CODEPAGE, PZH_CODEPAGE );
extern ZH_EXPORT ZH_SIZE      zh_cdpnDupLen( const char *, ZH_SIZE, PZH_CODEPAGE, PZH_CODEPAGE );
extern ZH_EXPORT ZH_SIZE      zh_cdpnDup2Len( const char *, ZH_SIZE, ZH_SIZE, PZH_CODEPAGE, PZH_CODEPAGE );

extern ZH_EXPORT char *       zh_cdpnDupUpper( PZH_CODEPAGE cdp, const char * pszText, ZH_SIZE * pnSize );
extern ZH_EXPORT char *       zh_cdpnDupLower( PZH_CODEPAGE cdp, const char * pszText, ZH_SIZE * pnSize );
extern ZH_EXPORT ZH_SIZE      zh_cdpnDup2Upper( PZH_CODEPAGE cdp, const char * pszText, ZH_SIZE nSize, char * buffer, ZH_SIZE nBuffLen );
extern ZH_EXPORT ZH_SIZE      zh_cdpnDup2Lower( PZH_CODEPAGE cdp, const char * pszText, ZH_SIZE nSize, char * buffer, ZH_SIZE nBuffLen );

extern ZH_EXPORT ZH_SIZE      zh_cdpTransLen( const char * pSrc, ZH_SIZE nSrc, ZH_SIZE nMax, PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut );
extern ZH_EXPORT ZH_SIZE      zh_cdpTransTo( const char * pSrc, ZH_SIZE nSrc, char * pDst, ZH_SIZE nDst, PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut );

extern ZH_EXPORT ZH_WCHAR     zh_cdpGetU16Disp( PZH_CODEPAGE cdp, ZH_UCHAR ch );
extern ZH_EXPORT ZH_SIZE      zh_cdpStrToUTF8Disp( PZH_CODEPAGE cdp, const char * pSrc, ZH_SIZE nSrc, char * pDst, ZH_SIZE nDst );
extern ZH_EXPORT int          zh_cdpTranslateDispChar( int iChar, PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut );
extern ZH_EXPORT int          zh_cdpTranslateChar( int iChar, PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut );

extern ZH_EXPORT ZH_UCHAR     zh_cdpGetChar( PZH_CODEPAGE cdp, ZH_WCHAR wc );
extern ZH_EXPORT ZH_WCHAR     zh_cdpGetU16( PZH_CODEPAGE cdp, ZH_UCHAR ch );
extern ZH_EXPORT ZH_UCHAR     zh_cdpGetUC( PZH_CODEPAGE cdp, ZH_WCHAR wc, ZH_UCHAR ucDef );
extern ZH_EXPORT ZH_WCHAR     zh_cdpGetWC( PZH_CODEPAGE cdp, ZH_UCHAR ch, ZH_WCHAR wcDef );

extern ZH_EXPORT ZH_BOOL      zh_cdpGetFromUTF8( PZH_CODEPAGE cdp, ZH_UCHAR ch, int * n, ZH_WCHAR * pwc );

extern ZH_EXPORT ZH_SIZE      zh_cdpUTF8StringLength( const char * pSrc, ZH_SIZE nLen );
extern ZH_EXPORT ZH_SIZE      zh_cdpUTF8StringAt( const char * szNeedle, ZH_SIZE nLenN, const char * szHaystack, ZH_SIZE nLenH, ZH_SIZE nStart, ZH_SIZE nEnd, ZH_BOOL fReverse );
extern ZH_EXPORT ZH_WCHAR     zh_cdpUTF8StringPeek( const char * pSrc, ZH_SIZE nLen, ZH_SIZE nPos );
extern ZH_EXPORT char *       zh_cdpUTF8StringSubstr( const char * pSrc, ZH_SIZE nLen, ZH_SIZE nFrom, ZH_SIZE nCount, ZH_SIZE * pnDest );

extern ZH_EXPORT ZH_SIZE      zh_cdpUTF8AsStrLen( PZH_CODEPAGE cdp, const char * pSrc, ZH_SIZE nSrc, ZH_SIZE nMax );
extern ZH_EXPORT ZH_SIZE      zh_cdpUTF8ToStr( PZH_CODEPAGE cdp, const char * pSrc, ZH_SIZE nSrc, char * pDst, ZH_SIZE nDst );
extern ZH_EXPORT ZH_SIZE      zh_cdpStrAsUTF8Len( PZH_CODEPAGE cdp, const char * pSrc, ZH_SIZE nSrc, ZH_SIZE nMax );
extern ZH_EXPORT ZH_SIZE      zh_cdpStrToUTF8( PZH_CODEPAGE cdp, const char * pSrc, ZH_SIZE nSrc, char * pDst, ZH_SIZE nDst );

extern ZH_EXPORT ZH_SIZE      zh_cdpU16AsStrLen( PZH_CODEPAGE cdp, const ZH_WCHAR * pSrc, ZH_SIZE nSrc, ZH_SIZE nMax );
extern ZH_EXPORT ZH_SIZE      zh_cdpU16ToStr( PZH_CODEPAGE cdp, int iEndian, const ZH_WCHAR * pSrc, ZH_SIZE nSrc, char * pDst, ZH_SIZE nDst );
extern ZH_EXPORT ZH_SIZE      zh_cdpStrAsU16Len( PZH_CODEPAGE cdp, const char * pSrc, ZH_SIZE nSrc, ZH_SIZE nMax );
extern ZH_EXPORT ZH_SIZE      zh_cdpStrToU16( PZH_CODEPAGE cdp, int iEndian, const char * pSrc, ZH_SIZE nSrc, ZH_WCHAR * pDst, ZH_SIZE nDst );
extern ZH_EXPORT ZH_WCHAR *   zh_cdpStrDupU16( PZH_CODEPAGE cdp, int iEndian, const char * pSrc );
extern ZH_EXPORT ZH_WCHAR *   zh_cdpStrDupnU16( PZH_CODEPAGE cdp, int iEndian, const char * pSrc, ZH_SIZE nSrc );
extern ZH_EXPORT ZH_WCHAR *   zh_cdpnStrDupU16( PZH_CODEPAGE cdp, int iEndian, const char * pSrc, ZH_SIZE nSrc, ZH_SIZE * pnDst );

extern ZH_EXPORT ZH_WCHAR     zh_cdpGetU16Ctrl( ZH_WCHAR wc );

extern ZH_EXPORT int          zh_cdpUTF8CharSize( ZH_WCHAR wc );
extern ZH_EXPORT int          zh_cdpU16CharToUTF8( char * szUTF8, ZH_WCHAR wc );
extern ZH_EXPORT ZH_BOOL      zh_cdpUTF8ToU16NextChar( ZH_UCHAR ucChar, int * n, ZH_WCHAR * pwc );

extern ZH_EXPORT PZH_ITEM     zh_itemDeserializeCP( const char ** pBufferPtr, ZH_SIZE * pnSize, PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut );
extern ZH_EXPORT char *       zh_itemSerializeCP( PZH_ITEM pItem, int iFlags, PZH_CODEPAGE cdpIn, PZH_CODEPAGE cdpOut, ZH_SIZE * pnSize );

extern ZH_EXPORT ZH_WCHAR     zh_cdpUpperWC( PZH_CODEPAGE cdp, ZH_WCHAR wc );

/* functions operating on character indexes */
extern ZH_EXPORT ZH_SIZE      zh_cdpTextLen( PZH_CODEPAGE cdp, const char * pText, ZH_SIZE nSize );
extern ZH_EXPORT ZH_SIZE      zh_cdpTextPos( PZH_CODEPAGE cdp, const char * pText, ZH_SIZE nSize, ZH_SIZE nIndex );
extern ZH_EXPORT ZH_SIZE      zh_cdpTextPosEx( PZH_CODEPAGE cdp, const char * pText, ZH_SIZE nSize, ZH_SIZE * pnIndex );

extern ZH_EXPORT ZH_WCHAR     zh_cdpTextGetU16( PZH_CODEPAGE cdp, const char * szText, ZH_SIZE nLen );
extern ZH_EXPORT ZH_SIZE      zh_cdpTextPutU16( PZH_CODEPAGE cdp, char * szText, ZH_SIZE nSize, ZH_WCHAR wc );
extern ZH_EXPORT ZH_BOOL      zh_cdpCharEq( PZH_CODEPAGE cdp, const char * szText1, ZH_SIZE nLen1, ZH_SIZE * pnPos1,
                                                              const char * szText2, ZH_SIZE nLen2, ZH_SIZE * pnPos2 );
extern ZH_EXPORT ZH_BOOL      zh_cdpCharCaseEq( PZH_CODEPAGE cdp, const char * szText1, ZH_SIZE nLen1, ZH_SIZE * pnPos1,
                                                                  const char * szText2, ZH_SIZE nLen2, ZH_SIZE * pnPos2 );
ZH_EXTERN_END

#endif /* ZH_APICDP_H_ */
