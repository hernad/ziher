/*
 * Header file for the Item API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#ifndef ZH_APIITM_H_
#define ZH_APIITM_H_

#include "zh_api.h"

ZH_EXTERN_BEGIN

#define ZH_EVAL_PARAM_MAX_ 9

typedef struct
{
   ZH_USHORT paramCount;
   PZH_ITEM pItems[ ZH_EVAL_PARAM_MAX_ + 1 ];
} ZH_EVALINFO, * PZH_EVALINFO;

extern ZH_EXPORT PZH_ITEM     zh_evalLaunch    ( PZH_EVALINFO pEvalInfo );
extern ZH_EXPORT ZH_BOOL      zh_evalNew       ( PZH_EVALINFO pEvalInfo, PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL      zh_evalPutParam  ( PZH_EVALINFO pEvalInfo, PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL      zh_evalRelease   ( PZH_EVALINFO pEvalInfo );

extern ZH_EXPORT void         zh_evalBlock( PZH_ITEM pCodeBlock, ... );
extern ZH_EXPORT void         zh_evalBlock0( PZH_ITEM pCodeBlock );
extern ZH_EXPORT void         zh_evalBlock1( PZH_ITEM pCodeBlock, PZH_ITEM pParam );

extern ZH_EXPORT ZH_BOOL      zh_execFromArray ( PZH_ITEM pParam );

extern ZH_EXPORT PZH_ITEM     zh_itemDo        ( PZH_ITEM pItem, ZH_ULONG ulPCount, ... );
extern ZH_EXPORT PZH_ITEM     zh_itemDoC       ( const char * szFunc, ZH_ULONG ulPCount, ... );

extern ZH_EXPORT PZH_ITEM     zh_itemArrayGet  ( PZH_ITEM pArray, ZH_SIZE nIndex );
extern ZH_EXPORT PZH_ITEM     zh_itemArrayNew  ( ZH_SIZE nLen );
extern ZH_EXPORT PZH_ITEM     zh_itemArrayPut  ( PZH_ITEM pArray, ZH_SIZE nIndex, PZH_ITEM pItem );
extern ZH_EXPORT ZH_SIZE      zh_itemCopyC     ( PZH_ITEM pItem, char * szBuffer, ZH_SIZE nLen );
extern ZH_EXPORT ZH_BOOL      zh_itemFreeC     ( char * szText );
extern ZH_EXPORT char *       zh_itemGetC      ( PZH_ITEM pItem );
extern ZH_EXPORT const char * zh_itemGetCPtr   ( PZH_ITEM pItem );
extern ZH_EXPORT ZH_SIZE      zh_itemGetCLen   ( PZH_ITEM pItem );
extern ZH_EXPORT char *       zh_itemGetDS     ( PZH_ITEM pItem, char * szDate );
extern ZH_EXPORT char *       zh_itemGetTS     ( PZH_ITEM pItem, char * szDateTime );
extern ZH_EXPORT long         zh_itemGetDL     ( PZH_ITEM pItem );
extern ZH_EXPORT double       zh_itemGetTD     ( PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL      zh_itemGetTDT    ( PZH_ITEM pItem, long * plJulian, long * plMilliSec );
extern ZH_EXPORT ZH_BOOL      zh_itemGetL      ( PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL      zh_itemGetLX     ( PZH_ITEM pItem );
extern ZH_EXPORT double       zh_itemGetND     ( PZH_ITEM pItem );
extern ZH_EXPORT double       zh_itemGetNDDec  ( PZH_ITEM pItem, int * piDec );
extern ZH_EXPORT int          zh_itemGetNI     ( PZH_ITEM pItem );
extern ZH_EXPORT long         zh_itemGetNL     ( PZH_ITEM pItem );
extern ZH_EXPORT ZH_I_SIZE      zh_itemGetNS     ( PZH_ITEM pItem );
extern ZH_EXPORT ZH_MAXINT    zh_itemGetNInt   ( PZH_ITEM pItem );
extern ZH_EXPORT void         zh_itemGetNLen   ( PZH_ITEM pItem, int * piWidth, int * piDec );
extern ZH_EXPORT void *       zh_itemGetPtr    ( PZH_ITEM pItem );
extern ZH_EXPORT void *       zh_itemGetPtrGC  ( PZH_ITEM pItem, const ZH_GC_FUNCS * pFuncs );
extern ZH_EXPORT PZH_SYMBOL     zh_itemGetSymbol ( PZH_ITEM pItem );
extern ZH_EXPORT PZH_ITEM     zh_itemNew       ( PZH_ITEM pNull );
extern ZH_EXPORT void         zh_itemInit      ( PZH_ITEM pItem );
extern ZH_EXPORT ZH_USHORT    zh_itemPCount    ( void );
extern ZH_EXPORT PZH_ITEM     zh_itemParam     ( ZH_USHORT uiParam );
extern ZH_EXPORT PZH_ITEM     zh_itemPutC      ( PZH_ITEM pItem, const char * szText );
extern ZH_EXPORT PZH_ITEM     zh_itemPutCL     ( PZH_ITEM pItem, const char * szText, ZH_SIZE nLen );
extern ZH_EXPORT PZH_ITEM     zh_itemPutCConst ( PZH_ITEM pItem, const char * szText );
extern ZH_EXPORT PZH_ITEM     zh_itemPutCLConst( PZH_ITEM pItem, const char * szText, ZH_SIZE nLen );
extern ZH_EXPORT PZH_ITEM     zh_itemPutCPtr   ( PZH_ITEM pItem, char * szText );
extern ZH_EXPORT PZH_ITEM     zh_itemPutCLPtr  ( PZH_ITEM pItem, char * szText, ZH_SIZE nLen );
extern ZH_EXPORT void         zh_itemSetCMemo  ( PZH_ITEM pItem );
extern ZH_EXPORT PZH_ITEM     zh_itemPutD      ( PZH_ITEM pItem, int iYear, int iMonth, int iDay );
extern ZH_EXPORT PZH_ITEM     zh_itemPutDS     ( PZH_ITEM pItem, const char * szDate );
extern ZH_EXPORT PZH_ITEM     zh_itemPutTS     ( PZH_ITEM pItem, const char * szDateTime );
extern ZH_EXPORT PZH_ITEM     zh_itemPutDL     ( PZH_ITEM pItem, long lJulian );
extern ZH_EXPORT PZH_ITEM     zh_itemPutTD     ( PZH_ITEM pItem, double dTimeStamp );
extern ZH_EXPORT PZH_ITEM     zh_itemPutTDT    ( PZH_ITEM pItem, long lJulian, long lMilliSec );
extern ZH_EXPORT PZH_ITEM     zh_itemPutL      ( PZH_ITEM pItem, ZH_BOOL bValue );
extern ZH_EXPORT PZH_ITEM     zh_itemPutND     ( PZH_ITEM pItem, double dNumber );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNI     ( PZH_ITEM pItem, int iNumber );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNL     ( PZH_ITEM pItem, long lNumber );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNS     ( PZH_ITEM pItem, ZH_I_SIZE nNumber );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNInt   ( PZH_ITEM pItem, ZH_MAXINT nNumber );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNIntLen( PZH_ITEM pItem, ZH_MAXINT nNumber, int iWidth );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNLen   ( PZH_ITEM pItem, double dNumber, int iWidth, int iDec );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNDLen  ( PZH_ITEM pItem, double dNumber, int iWidth, int iDec );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNDDec  ( PZH_ITEM pItem, double dNumber, int iDec );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNILen  ( PZH_ITEM pItem, int iNumber, int iWidth );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNLLen  ( PZH_ITEM pItem, long lNumber, int iWidth );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNumType( PZH_ITEM pItem, double dNumber, int iDec, int iType1, int iType2 );
extern ZH_EXPORT PZH_ITEM     zh_itemPutPtr    ( PZH_ITEM pItem, void * pValue );
extern ZH_EXPORT PZH_ITEM     zh_itemPutPtrGC  ( PZH_ITEM pItem, void * pValue );
extern ZH_EXPORT PZH_ITEM     zh_itemPutSymbol ( PZH_ITEM pItem, PZH_SYMBOL pSym );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNil    ( PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL      zh_itemRelease   ( PZH_ITEM pItem );
extern ZH_EXPORT PZH_ITEM     zh_itemReturn    ( PZH_ITEM pItem );
extern ZH_EXPORT PZH_ITEM     zh_itemReturnForward( PZH_ITEM pItem );
extern ZH_EXPORT void         zh_itemReturnRelease( PZH_ITEM pItem );
extern ZH_EXPORT ZH_SIZE      zh_itemSize      ( PZH_ITEM pItem );
extern ZH_EXPORT ZH_TYPE      zh_itemType      ( PZH_ITEM pItem );
extern ZH_EXPORT const char * zh_itemTypeStr ( PZH_ITEM pItem );
#ifndef ZH_LONG_LONG_OFF
extern ZH_EXPORT ZH_LONGLONG  zh_itemGetNLL    ( PZH_ITEM pItem );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNLL    ( PZH_ITEM pItem, ZH_LONGLONG lNumber );
extern ZH_EXPORT PZH_ITEM     zh_itemPutNLLLen ( PZH_ITEM pItem, ZH_LONGLONG lNumber, int iWidth );
#endif

extern ZH_EXPORT PZH_ITEM     zh_itemParamPtr  ( ZH_USHORT uiParam, long lMask );
extern ZH_EXPORT ZH_BOOL      zh_itemParamStore( ZH_USHORT uiParam, PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL      zh_itemParamStoreForward( ZH_USHORT uiParam, PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL      zh_itemParamStoreRelease( ZH_USHORT uiParam, PZH_ITEM pItem );
extern ZH_EXPORT ZH_BOOL      zh_itemEqual     ( PZH_ITEM pItem1, PZH_ITEM pItem2 );
extern ZH_EXPORT ZH_BOOL      zh_itemCompare   ( PZH_ITEM pItem1, PZH_ITEM pItem2, ZH_BOOL bForceExact, int * piResult ); /* For compatible types compare pItem1 with pItem2 setting piResult to -1, 0 or 1 if pItem1 is <, == or > then pItem2 and return true otherwise return false. */
extern ZH_EXPORT int          zh_itemStrCmp    ( PZH_ITEM pFirst, PZH_ITEM pSecond, ZH_BOOL bForceExact ); /* our string compare */
extern ZH_EXPORT int          zh_itemStrICmp   ( PZH_ITEM pFirst, PZH_ITEM pSecond, ZH_BOOL bForceExact ); /* our string compare */
extern ZH_EXPORT void         zh_itemCopy      ( PZH_ITEM pDest, PZH_ITEM pSource ); /* copies an item to one place to another respecting its content */
extern ZH_EXPORT void         zh_itemCopyToRef ( PZH_ITEM pDest, PZH_ITEM pSource );
extern ZH_EXPORT void         zh_itemCopyFromRef( PZH_ITEM pDest, PZH_ITEM pSource );
extern ZH_EXPORT void         zh_itemMove      ( PZH_ITEM pDest, PZH_ITEM pSource ); /* moves the value of an item without incrementing of reference counters, source is cleared */
extern ZH_EXPORT void         zh_itemMoveRef   ( PZH_ITEM pDest, PZH_ITEM pSource );
extern ZH_EXPORT void         zh_itemMoveToRef ( PZH_ITEM pDest, PZH_ITEM pSource );
extern ZH_EXPORT void         zh_itemMoveFromRef( PZH_ITEM pDest, PZH_ITEM pSource );
extern ZH_EXPORT void         zh_itemClear     ( PZH_ITEM pItem );
extern ZH_EXPORT PZH_ITEM     zh_itemUnRef     ( PZH_ITEM pItem ); /* de-references passed variable */
extern ZH_EXPORT PZH_ITEM     zh_itemUnRefOnce ( PZH_ITEM pItem ); /* de-references passed variable, one step*/
extern ZH_EXPORT PZH_ITEM     zh_itemUnRefRefer( PZH_ITEM pItem ); /* de-references passed variable, leaving the last reference */
extern ZH_EXPORT PZH_ITEM     zh_itemUnRefWrite( PZH_ITEM pItem, PZH_ITEM pSource ); /* de-references passed variable for writing */
extern ZH_EXPORT PZH_ITEM     zh_itemUnShare   ( PZH_ITEM pItem ); /* un-share given string item */
extern ZH_EXPORT PZH_ITEM     zh_itemUnShareString( PZH_ITEM pItem ); /* un-share given string item - the pItem have to be valid unrefed string item */
extern ZH_EXPORT PZH_ITEM     zh_itemReSizeString( PZH_ITEM pItem, ZH_SIZE nSize ); /* Resize string buffer of given string item - the pItem have to be valid unrefed string item */
extern ZH_EXPORT ZH_BOOL      zh_itemGetWriteCL( PZH_ITEM pItem, char ** pszValue, ZH_SIZE * pnLen );
extern ZH_EXPORT PZH_ITEM     zh_itemClone     ( PZH_ITEM pItem ); /* clone the given item */
extern ZH_EXPORT void         zh_itemCloneTo   ( PZH_ITEM pDest, PZH_ITEM pSource ); /* clone the given item */
extern ZH_EXPORT char *       zh_itemStr       ( PZH_ITEM pNumber, PZH_ITEM pWidth, PZH_ITEM pDec ); /* convert a number to a string */
extern ZH_EXPORT char *       zh_itemString    ( PZH_ITEM pItem, ZH_SIZE * nLen, ZH_BOOL * bFreeReq );  /* Convert any scalar to a string */
extern ZH_EXPORT ZH_BOOL      zh_itemStrBuf    ( char *szResult, PZH_ITEM pNumber, int iSize, int iDec ); /* convert a number to a string */
extern ZH_EXPORT PZH_ITEM     zh_itemValToStr  ( PZH_ITEM pItem ); /* Convert any scalar to a string */
extern ZH_EXPORT char *       zh_itemPadConv   ( PZH_ITEM pItem, ZH_SIZE * pnSize, ZH_BOOL * bFreeReq );
extern ZH_EXPORT void         zh_itemSwap      ( PZH_ITEM pItem1, PZH_ITEM pItem2 );

extern ZH_EXPORT char *       zh_itemSerialize( PZH_ITEM pItem, int iFlags, ZH_SIZE * pnSize );
extern ZH_EXPORT PZH_ITEM     zh_itemDeserialize( const char ** pBufferPtr, ZH_SIZE * pnSize );

#if defined( _ZH_API_INTERNAL_ )

extern PZH_ITEM zh_itemPutPtrRawGC( PZH_ITEM pItem, void * pValue );

#  define zh_itemSetNil( item )           do { \
                                             if( ZH_IS_COMPLEX( item ) ) \
                                                zh_itemClear( item ); \
                                             else \
                                                (item)->type = ZH_IT_NIL; \
                                          } while( 0 )

#  define zh_itemRawCpy( dst, src )       do { *(dst) = *(src); } while( 0 )

#  define zh_itemRawSwap( dst, src )      do { \
                                             ZH_ITEM temp; \
                                             zh_itemRawCpy( &temp, dst ); \
                                             zh_itemRawCpy( dst, src ); \
                                             zh_itemRawCpy( src, &temp ); \
                                          } while( 0 )

#if 1
#  define zh_itemRawMove( dst, src )      do { \
                                             zh_itemRawCpy( dst, src ); \
                                             (src)->type = ZH_IT_NIL; \
                                          } while( 0 )
#else /* _ZH_API_INTERNAL_ */
#  define zh_itemRawMove( dst, src )      zh_itemMove( (dst), (src) )
#endif

   /* intentional low-level hack to eliminate race condition in
    * unprotected readonly access in few places in core code only.
    * zh_item[Raw]Move() moves ZH_ITEM structure members first coping
    * 'type' and then 'item' parts of ZH_ITEM. In this macro the order
    * is reverted. [druzus]
    */
#  define zh_itemSafeMove( dst, src )  do { \
                                             (dst)->item = (src)->item; \
                                             (dst)->type = (src)->type; \
                                             (src)->type = ZH_IT_NIL; \
                                          } while( 0 )

#else

#  define zh_itemSetNil( item )           zh_itemClear( (item) )

#  define zh_itemRawMove( dst, src )      zh_itemMove( (dst), (src) )

#endif /* _ZH_API_INTERNAL_ */

ZH_EXTERN_END

#endif /* ZH_APIITM_H_ */
