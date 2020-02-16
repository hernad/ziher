/*
 * JavaScript Object Notation (JSON)
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
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

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_string_api.h"
#include "zh_set.h"
#include "zh_json.h"

/*
   The application/json Media Type for JavaScript Object Notation (JSON)
   https://tools.ietf.org/html/rfc4627

      C level functions:
        char * zh_jsonEncode( PZH_ITEM pValue, ZH_SIZE * pnLen, int iIndent );
           pValue  - value to encode;
           pnLen   - if pnLen is not NULL, length of returned buffer is
                     stored to *pnLen;
           iIndent - indenting to be human readable;
           returns pointer to encoded JSON buffer. buffer must be fried
              by the caller.

        ZH_SIZE zh_jsonDecode( const char * szSource, PZH_ITEM pValue );
           szSource - JSON source;
           pValue   - item to store decoded value. Item value is
                      undetermined in case of error;
           returns number of bytes decoded from the buffer. This allows
              to use the remaining part of the buffer for some other
              purposes. Returns 0 on error.

      Ziher level functions:
        zh_jsonEncode( xValue [, lHuman = .F. | nIndent = 0 ] ) --> cJSON
        zh_jsonDecode( cJSON ) --> xValue
        zh_jsonDecode( cJSON, @xValue ) --> nLengthDecoded

      Note:
        - JSON encode functions are safe for recursive arrays and hashes.
          Recursive part of array or hash will be stored as null. JSON
          encoder still allows to use same structure in the leaves, in
          this case content will be duplicate.
          I.e.:
             xI := { 1, NIL }
             xI[ 2 ] := xI
             ? zh_jsonEncode( xI )  // [1,null]
          but:
             xI := { 1, .T. }
             xI := { 2, xI, xI }
             ? zh_jsonEncode( xI )  // [2,[1,true],[1,true]]
 */

typedef struct
{
   char *  pBuffer;
   char *  pHead;
   ZH_SIZE nAlloc;
   void ** pId;
   ZH_SIZE nAllocId;
   int     iIndent;
   int     iEolLen;
   const char * szEol;
} ZH_JSON_ENCODE_CTX, * PZH_JSON_ENCODE_CTX;


#define INDENT_SIZE  2

static void _zh_jsonCtxAdd( PZH_JSON_ENCODE_CTX pCtx, const char * szString, ZH_SIZE nLen )
{
   if( pCtx->pHead + nLen >= pCtx->pBuffer + pCtx->nAlloc )
   {
      ZH_SIZE nSize = pCtx->pHead - pCtx->pBuffer;

      pCtx->nAlloc += ( pCtx->nAlloc << 1 ) + nLen;
      pCtx->pBuffer = ( char * ) zh_xrealloc( pCtx->pBuffer, pCtx->nAlloc );
      pCtx->pHead = pCtx->pBuffer + nSize;
   }
   if( szString )
   {
      zh_xmemcpy( pCtx->pHead, szString, nLen );
      pCtx->pHead += nLen;
   }
}

static void _zh_jsonCtxAddIndent( PZH_JSON_ENCODE_CTX pCtx, ZH_SIZE nLevel )
{
   if( nLevel > 0 )
   {
      ZH_SIZE nCount = nLevel * ( pCtx->iIndent > 0 ? pCtx->iIndent : 1 );
      if( pCtx->pHead + nCount >= pCtx->pBuffer + pCtx->nAlloc )
      {
         ZH_SIZE nSize = pCtx->pHead - pCtx->pBuffer;

         pCtx->nAlloc += ( pCtx->nAlloc << 1 ) + nCount;
         pCtx->pBuffer = ( char * ) zh_xrealloc( pCtx->pBuffer, pCtx->nAlloc );
         pCtx->pHead = pCtx->pBuffer + nSize;
      }
      zh_xmemset( pCtx->pHead, pCtx->iIndent > 0 ? ' ' : '\t', nCount );
      pCtx->pHead += nCount;
   }
}

static void _zh_jsonEncode( PZH_ITEM pValue, PZH_JSON_ENCODE_CTX pCtx,
                            ZH_SIZE nLevel, ZH_BOOL fEOL, PZH_CODEPAGE cdp )
{
   /* Protection against recursive structures */
   if( ( ZH_IS_ARRAY( pValue ) || ZH_IS_HASH( pValue ) ) && zh_itemSize( pValue ) > 0 )
   {
      void * id = ZH_IS_HASH( pValue ) ? zh_hashId( pValue ) : zh_arrayId( pValue );
      ZH_SIZE nIndex;

      for( nIndex = 0; nIndex < nLevel; nIndex++ )
      {
         if( pCtx->pId[ nIndex ] == id )
         {
            if( ! fEOL && pCtx->iIndent )
               _zh_jsonCtxAddIndent( pCtx, nLevel );
            _zh_jsonCtxAdd( pCtx, "null", 4 );
            return;
         }
      }
      if( nLevel >= pCtx->nAllocId )
      {
         pCtx->nAllocId += 8;
         pCtx->pId = ( void ** ) zh_xrealloc( pCtx->pId, sizeof( void * ) * pCtx->nAllocId );
      }
      pCtx->pId[ nLevel ] = id;
   }

   if( fEOL )
   {
      --pCtx->pHead;
      _zh_jsonCtxAdd( pCtx, pCtx->szEol, pCtx->iEolLen );
   }

   if( ZH_IS_STRING( pValue ) )
   {
      ZH_SIZE nPos, nLen;
      const char * szString;
      void * hString = NULL;
      char buf[ 8 ];

      if( cdp )
      {
         szString = zh_itemGetStr( pValue, cdp, &hString, &nLen );
      }
      else
      {
         szString = zh_itemGetCPtr( pValue );
         nLen = zh_itemGetCLen( pValue );
      }

      _zh_jsonCtxAdd( pCtx, "\"", 1 );
      nPos = 0;
      while( nPos < nLen )
      {
         unsigned char uch = szString[ nPos ];
         ZH_SIZE nPos2 = nPos;
         while( uch >= ' ' && uch != '\\' && uch != '\"' )
            uch = szString[ ++nPos2 ];
         if( nPos2 > nPos )
         {
            _zh_jsonCtxAdd( pCtx, szString + nPos, nPos2 - nPos );
            if( nPos2 >= nLen )
               break;
            nPos = nPos2;
         }

         switch( uch )
         {
            case '\\':
               _zh_jsonCtxAdd( pCtx, "\\\\", 2 );
               break;
            case '\"':
               _zh_jsonCtxAdd( pCtx, "\\\"", 2 );
               break;
            case '\b':
               _zh_jsonCtxAdd( pCtx, "\\b", 2 );
               break;
            case '\f':
               _zh_jsonCtxAdd( pCtx, "\\f", 2 );
               break;
            case '\n':
               _zh_jsonCtxAdd( pCtx, "\\n", 2 );
               break;
            case '\r':
               _zh_jsonCtxAdd( pCtx, "\\r", 2 );
               break;
            case '\t':
               _zh_jsonCtxAdd( pCtx, "\\t", 2 );
               break;
            default:
               zh_snprintf( buf, sizeof( buf ), "\\u00%02X", uch );
               _zh_jsonCtxAdd( pCtx, buf, 6 );
               break;
         }
         nPos++;
      }
      _zh_jsonCtxAdd( pCtx, "\"", 1 );
      zh_strfree( hString );
   }
   else if( ZH_IS_NUMINT( pValue ) )
   {
      char buf[ 24 ];
      ZH_MAXINT nVal = zh_itemGetNInt( pValue );
      ZH_BOOL fNeg = nVal < 0;
      int i = 0;

      if( fNeg )
         nVal = -nVal;
      do
         buf[ sizeof( buf ) - ++i ] = ( nVal % 10 ) + '0';
      while( ( nVal /= 10 ) != 0 );
      if( fNeg )
         buf[ sizeof( buf ) - ++i ] = '-';
      _zh_jsonCtxAdd( pCtx, &buf[ sizeof( buf ) - i ], i );
   }
   else if( ZH_IS_NUMERIC( pValue ) )
   {
      char buf[ 64 ];
      int iDec;
      double dblValue = zh_itemGetNDDec( pValue, &iDec );

      zh_snprintf( buf, sizeof( buf ), "%.*f", iDec, dblValue );
      _zh_jsonCtxAdd( pCtx, buf, strlen( buf ) );
   }
   else if( ZH_IS_NIL( pValue ) )
   {
      _zh_jsonCtxAdd( pCtx, "null", 4 );
   }
   else if( ZH_IS_LOGICAL( pValue ) )
   {
      if( zh_itemGetL( pValue ) )
         _zh_jsonCtxAdd( pCtx, "true", 4 );
      else
         _zh_jsonCtxAdd( pCtx, "false", 5 );

   }
   else if( ZH_IS_DATE( pValue ) )
   {
      char szBuffer[ 10 ];

      zh_itemGetDS( pValue, szBuffer + 1 );
      szBuffer[ 0 ] = '\"';
      szBuffer[ 9 ] = '\"';
      _zh_jsonCtxAdd( pCtx, szBuffer, 10 );
   }
   else if( ZH_IS_TIMESTAMP( pValue ) )
   {
      char szBuffer[ 19 ];
      zh_itemGetTS( pValue, szBuffer + 1 );
      szBuffer[ 0 ] = '\"';
      szBuffer[ 18 ] = '\"';
      _zh_jsonCtxAdd( pCtx, szBuffer, 19 );
   }
   else if( ZH_IS_ARRAY( pValue ) )
   {
      ZH_SIZE nLen = zh_itemSize( pValue );

      if( nLen )
      {
         ZH_SIZE nIndex;

         if( pCtx->iIndent )
            _zh_jsonCtxAddIndent( pCtx, nLevel );

         _zh_jsonCtxAdd( pCtx, "[", 1 );

         for( nIndex = 1; nIndex <= nLen; nIndex++ )
         {
            PZH_ITEM pItem = zh_arrayGetItemPtr( pValue, nIndex );

            if( nIndex > 1 )
               _zh_jsonCtxAdd( pCtx, ",", 1 );

            if( pCtx->iIndent )
               _zh_jsonCtxAdd( pCtx, pCtx->szEol, pCtx->iEolLen );

            if( pCtx->iIndent &&
                ! ( ( ZH_IS_ARRAY( pItem ) || ZH_IS_HASH( pItem ) ) &&
                    zh_itemSize( pItem ) > 0 ) )
               _zh_jsonCtxAddIndent( pCtx, ( nLevel + 1 ) );

            _zh_jsonEncode( pItem, pCtx, nLevel + 1, ZH_FALSE, cdp );
         }
         if( pCtx->iIndent )
         {
            _zh_jsonCtxAdd( pCtx, pCtx->szEol, pCtx->iEolLen );
            _zh_jsonCtxAddIndent( pCtx, nLevel );
         }
         _zh_jsonCtxAdd( pCtx, "]", 1 );
      }
      else
         _zh_jsonCtxAdd( pCtx, "[]", 2 );
   }
   else if( ZH_IS_HASH( pValue ) )
   {
      ZH_SIZE nLen = zh_hashLen( pValue );

      if( nLen )
      {
         ZH_SIZE nIndex;

         if( pCtx->iIndent )
            _zh_jsonCtxAddIndent( pCtx, nLevel );

         _zh_jsonCtxAdd( pCtx, "{", 1 );

         for( nIndex = 1; nIndex <= nLen; nIndex++ )
         {
            PZH_ITEM pKey = zh_hashGetKeyAt( pValue, nIndex );

            if( ZH_IS_STRING( pKey ) )
            {
               PZH_ITEM pItem = zh_hashGetValueAt( pValue, nIndex );

               if( nIndex > 1 )
                  _zh_jsonCtxAdd( pCtx, ",", 1 );

               if( pCtx->iIndent )
               {
                  _zh_jsonCtxAdd( pCtx, pCtx->szEol, pCtx->iEolLen );
                  _zh_jsonCtxAddIndent( pCtx, ( nLevel + 1 ) );
               }
               _zh_jsonEncode( pKey, pCtx, nLevel + 1, ZH_FALSE, cdp );

               if( pCtx->iIndent )
               {
                  _zh_jsonCtxAdd( pCtx, ": ", 2 );
                  fEOL = ( ZH_IS_ARRAY( pItem ) || ZH_IS_HASH( pItem ) ) && zh_itemSize( pItem ) > 0;
               }
               else
               {
                  _zh_jsonCtxAdd( pCtx, ":", 1 );
                  fEOL = ZH_FALSE;
               }

               _zh_jsonEncode( pItem, pCtx, nLevel + 1, fEOL, cdp );
            }
         }
         if( pCtx->iIndent )
         {
            _zh_jsonCtxAdd( pCtx, pCtx->szEol, pCtx->iEolLen );
            _zh_jsonCtxAddIndent( pCtx, nLevel );
         }
         _zh_jsonCtxAdd( pCtx, "}", 1 );
      }
      else
         _zh_jsonCtxAdd( pCtx, "{}", 2 );
   }
   else
   {
      /* All unsupported types are replaced by null */
      _zh_jsonCtxAdd( pCtx, "null", 4 );
   }
}


static const char * _skipws( const char * szSource )
{
   while( *szSource == ' ' || *szSource == '\t' || *szSource == '\n' || *szSource == '\r' )
      szSource++;
   return szSource;
}

static const char * _zh_jsonDecode( const char * szSource, PZH_ITEM pValue, PZH_CODEPAGE cdp )
{
   if( *szSource == '\"' )
   {
      char * szDest, * szHead;
      ZH_SIZE nAlloc = 16;

      szHead = szDest = ( char * ) zh_xgrab( nAlloc );
      szSource++;
      while( *szSource != '\"' )
      {
         if( szHead + 6 >= szDest + nAlloc )
         {
            ZH_SIZE nLen = szHead - szDest;
            nAlloc += nAlloc << 1;
            szDest = ( char * ) zh_xrealloc( szDest, nAlloc );
            szHead = szDest + nLen;
         }
         if( *szSource == '\\' )
         {
            szSource++;
            switch( *szSource )
            {
               case '\"':
                  *szHead++ = '\"';
                  break;
               case '\\':
                  *szHead++ = '\\';
                  break;
               case '/':
                  *szHead++ = '/';
                  break;
               case 'b':
                  *szHead++ = '\b';
                  break;
               case 'f':
                  *szHead++ = '\f';
                  break;
               case 'n':
                  *szHead++ = '\n';
                  break;
               case 'r':
                  *szHead++ = '\r';
                  break;
               case 't':
                  *szHead++ = '\t';
                  break;
               case 'u':
               {
                  ZH_WCHAR wc = 0;
                  int i;

                  for( i = 0; i < 4; i++ )
                  {
                     char c = *++szSource;
                     wc <<= 4;
                     if( c >= '0' && c <= '9' )
                        wc += c - '0';
                     else if( c >= 'A' && c <= 'F' )
                        wc += c - 'A' + 10;
                     else if( c >= 'a' && c <= 'f' )
                        wc += c - 'a' + 10;
                     else
                     {
                        zh_xfree( szDest );
                        return NULL;
                     }
                  }
                  szHead += zh_cdpU16ToStr( cdp ? cdp : zh_vmCDP(), ZH_CODEPAGE_ENDIAN_NATIVE,
                                            &wc, 1, szHead, szDest + nAlloc - szHead );
                  break;
               }
               default:
                  zh_xfree( szDest );
                  return NULL;
            }
            szSource++;
         }
         else if( *( const unsigned char * ) szSource >= ' ' )
            *szHead++ = *szSource++;
         else
         {
            zh_xfree( szDest );
            return NULL;
         }
      }
      if( cdp && zh_vmCDP() != cdp )
         zh_itemPutStrLen( pValue, cdp, szDest, szHead - szDest );
      else
         zh_itemPutCL( pValue, szDest, szHead - szDest );
      zh_xfree( szDest );
      return szSource + 1;
   }
   else if( *szSource == '-' || ( *szSource >= '0' && *szSource <= '9' ) )
   {
      /* NOTE: this function is much less strict to number format than
               JSON syntax definition. This is allowed behaviour [Mindaugas] */
      ZH_MAXINT nValue = 0;
      double dblValue = 0;
      ZH_BOOL fNeg, fDbl = ZH_FALSE;
      int iDec = 0;

      fNeg = *szSource == '-';
      if( fNeg )
         szSource++;

      while( *szSource >= '0' && *szSource <= '9' )
      {
         nValue = nValue * 10 + *szSource - '0';
         szSource++;
      }
      if( *szSource == '.' )
      {
         double mult = 1;

         dblValue = ( double ) nValue;
         fDbl = ZH_TRUE;
         szSource++;
         while( *szSource >= '0' && *szSource <= '9' )
         {
            mult /= 10;
            dblValue += ( ( double ) ( *szSource - '0' ) ) * mult;
            szSource++;
            iDec++;
         }
      }
      if( *szSource == 'e' || *szSource == 'E' )
      {
         ZH_BOOL fNegExp;
         int iExp = 0;

         szSource++;
         fNegExp = *szSource == '-';
         if( fNegExp )
            szSource++;

         while( *szSource >= '0' && *szSource <= '9' )
         {
            iExp = iExp * 10 + *szSource - '0';
            szSource++;
         }
         if( ! fDbl )
         {
            dblValue = ( double ) nValue;
            fDbl = ZH_TRUE;
         }
         if( fNegExp )
            iDec += iExp;
         dblValue = zh_numExpConv( dblValue, fNegExp ? iExp : -iExp );
      }

      if( fDbl )
         zh_itemPutNDDec( pValue, zh_numRound( fNeg ? -dblValue : dblValue, iDec ), iDec );
      else
         zh_itemPutNInt( pValue, fNeg ? -nValue : nValue );
      return szSource;
   }
   else if( ! strncmp( szSource, "null", 4 ) )
   {
      zh_itemClear( pValue );
      return szSource + 4;
   }
   else if( ! strncmp( szSource, "true", 4 ) )
   {
      zh_itemPutL( pValue, ZH_TRUE );
      return szSource + 4;
   }
   else if( ! strncmp( szSource, "false", 5 ) )
   {
      zh_itemPutL( pValue, ZH_FALSE );
      return szSource + 5;
   }
   else if( *szSource == '[' )
   {
      zh_arrayNew( pValue, 0 );
      szSource = _skipws( szSource + 1 );
      if( *szSource != ']' )
      {
         PZH_ITEM pItem = zh_itemNew( NULL );

         for( ;; )
         {
            szSource = _zh_jsonDecode( szSource, pItem, cdp );
            if( ! szSource )
            {
               zh_itemRelease( pItem );
               return NULL;
            }
            zh_arrayAddForward( pValue, pItem );

            szSource = _skipws( szSource );
            if( *szSource == ',' )
            {
               szSource = _skipws( szSource + 1 );
               continue;
            }
            else if( *szSource == ']' )
               break;
            else
            {
               zh_itemRelease( pItem );
               return NULL;
            }
         }
         zh_itemRelease( pItem );
      }
      return szSource + 1;
   }
   else if( *szSource == '{' )
   {
      zh_hashNew( pValue );
      szSource = _skipws( szSource + 1 );
      if( *szSource != '}' )
      {
         PZH_ITEM pItemKey = zh_itemNew( NULL );
         PZH_ITEM pItemValue = zh_itemNew( NULL );

         for( ;; )
         {
            /* Do we need to check if key does not exist yet? */
            if( ( szSource = _zh_jsonDecode( szSource, pItemKey, cdp ) ) == NULL ||
                ! ZH_IS_STRING( pItemKey ) ||
                * ( szSource = _skipws( szSource ) ) != ':' ||
                ( szSource = _zh_jsonDecode( _skipws( szSource + 1 ), pItemValue, cdp ) ) == NULL)
            {
               zh_itemRelease( pItemKey );
               zh_itemRelease( pItemValue );
               return NULL;
            }

            zh_hashAdd( pValue, pItemKey, pItemValue );
            szSource = _skipws( szSource );
            if( *szSource == ',' )
            {
               szSource = _skipws( szSource + 1 );
               continue;
            }
            else if( *szSource == '}' )
               break;
            else
            {
               zh_itemRelease( pItemKey );
               zh_itemRelease( pItemValue );
               return NULL;
            }
         }
         zh_itemRelease( pItemKey );
         zh_itemRelease( pItemValue );
      }
      return szSource + 1;
   }
   return NULL;
}

/* C level API functions */

char * zh_jsonEncodeCP( PZH_ITEM pValue, ZH_SIZE * pnLen, int iIndent, PZH_CODEPAGE cdp )
{
   PZH_JSON_ENCODE_CTX pCtx;
   char * szRet;
   ZH_SIZE nLen;

   pCtx = ( PZH_JSON_ENCODE_CTX ) zh_xgrab( sizeof( ZH_JSON_ENCODE_CTX ) );
   pCtx->nAlloc = 16;
   pCtx->pHead = pCtx->pBuffer = ( char * ) zh_xgrab( pCtx->nAlloc );
   pCtx->nAllocId = 8;
   pCtx->pId = ( void ** ) zh_xgrab( sizeof( void * ) * pCtx->nAllocId );
   pCtx->iIndent = iIndent;
   pCtx->szEol = zh_setGetEOL();
   if( ! pCtx->szEol || ! pCtx->szEol[ 0 ] )
      pCtx->szEol = zh_conNewLine();
   pCtx->iEolLen = ( int ) strlen( pCtx->szEol );

   _zh_jsonEncode( pValue, pCtx, 0, ZH_FALSE, cdp );
   if( iIndent )
      _zh_jsonCtxAdd( pCtx, pCtx->szEol, pCtx->iEolLen );

   nLen = pCtx->pHead - pCtx->pBuffer;
   szRet = ( char * ) zh_xrealloc( pCtx->pBuffer, nLen + 1 );
   szRet[ nLen ] = '\0';
   zh_xfree( pCtx->pId );
   zh_xfree( pCtx );
   if( pnLen )
      *pnLen = nLen;
   return szRet;
}

char * zh_jsonEncode( PZH_ITEM pValue, ZH_SIZE * pnLen, int iIndent )
{
   return zh_jsonEncodeCP( pValue, pnLen, iIndent, NULL );
}

ZH_SIZE zh_jsonDecodeCP( const char * szSource, PZH_ITEM pValue, PZH_CODEPAGE cdp )
{
   PZH_ITEM pItem = pValue ? pValue : zh_itemNew( NULL );
   const char * sz;

   sz = szSource ? _zh_jsonDecode( _skipws( szSource ), pItem, cdp ) : NULL;
   if( ! pValue )
      zh_itemRelease( pItem );
   if( sz )
      return sz - szSource;
   return 0;
}

ZH_SIZE zh_jsonDecode( const char * szSource, PZH_ITEM pValue )
{
   return zh_jsonDecodeCP( szSource, pValue, NULL );
}

/* Ziher level API functions */

static PZH_CODEPAGE _zh_jsonCdpPar( int iParam )
{
   if( zh_pcount() >= iParam )
   {
      const char * szCdp = zh_parc( iParam );

      if( szCdp )
         return zh_cdpFindExt( szCdp );
   }
   return NULL;
}

ZH_FUNC( ZH_JSONENCODE )
{
   PZH_ITEM pItem = zh_param( 1, ZH_IT_ANY );

   if( pItem )
   {
      ZH_SIZE nLen;
      int iIndent = zh_parl( 2 ) ? INDENT_SIZE : zh_parni( 2 );
      char * szRet = zh_jsonEncodeCP( pItem, &nLen, iIndent, _zh_jsonCdpPar( 3 ) );
      zh_retclen_buffer( szRet, nLen );
   }
}

ZH_FUNC( ZH_JSONDECODE )
{
   PZH_ITEM pItem = zh_itemNew( NULL );
   ZH_SIZE nSize = zh_jsonDecodeCP( zh_parc( 1 ), pItem, _zh_jsonCdpPar( 3 ) );

   if( ZH_ISBYREF( 2 ) )
   {
      zh_retns( ( ZH_ISIZ ) nSize );
      zh_itemParamStoreForward( 2, pItem );
      zh_itemRelease( pItem );
   }
   else
      zh_itemReturnRelease( pItem );
}
