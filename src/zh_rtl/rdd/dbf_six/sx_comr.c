/*
 * SIX compatible functions:
 *       zh_LZSSxCompressMem()
 *       zh_LZSSxDecompressMem()
 *       zh_LZSSxCompressFile()
 *       zh_LZSSxDecompressFile()
 *
 *       sx_FCompress()
 *       sx_FDecompress()
 *       _sx_StrDecompress()
 *       _sx_StrCompress()
 *
 * Copyright 2007 Przemyslaw Czerpak
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

/*
   SIX uses modified version of LZSS algorithm.
   It uses 12 bits for position offset in ring buffer and 4 bits for the
   match length. LZSS is modified version of LZ77 algorithm which can store
   original bytes from input string instead of position and match length in
   the ring buffer when the match length does not reach some limit. In SIX
   the minimum match length is 3 and it is used to increase to match length
   in 4 bit offset by adding 3 also so the effective maximum match length is
   18. Of course we have to store the information about the type of item in
   compressed data to know it is (offset+length) pair or simple byte.
   SIX put 1 byte in compressed data which inform about the type of next
   8 items: bit set to 1 means normal byte, 0 means item pair.
   The funny thing is that SIX seems to effectively use only 11 bits ring
   buffer (2048 bytes size). I still do not know why. Maybe it was a hack
   for some problems with implementation of encoding or work around for
   a bug in some signed/unsigned value conversions? Or maybe someone
   wanted to reduce memory overhead in used algorithm for finding the
   longest match? The SIX was written for 16-bit DOS and the memory
   consumption was important though it should not be too much. They
   documented ~9 KiB increasing the ring buffer size should be linear to
   other used (helper) structures.
   The next interesting thing is that it dynamically overwrites the ring
   buffer with stream data and does not use any separate look ahead buffers.
   This fact suggests that SIX uses some simple implementation with
   greedy early parsing and does not look for optimal matches.
   Our algorithm has to make the same with the ring buffer to be compatible.
   UPDATE: Using smaller ring buffer without increasing the match pointer
   suggested me that it is possible that someone didn't understand it fully
   and modified already existing algorithm. I spend a while on the internet
   looking for old LZSS implementations and I've found it. IMHO in 99%
   this code is used in SIX. This is lzss.c file written by Haruhiko Okumura
   with the following note in header:
   ,---------------------------------------------------------
   |    Use, distribute, and modify this program freely.
   |    Please send me your improved versions.
   `---------------------------------------------------------
   This LZSS implementation gives exactly the same results as SIX when we
   change the ring buffer size (#define N  4096 //in the head of this file)
   to 2048. So I used exactly the same algorithm with binary tree for
   finding the longest match. The code may looks a little bit differ then
   in lzss.c because I've already implement most of it before I found
   this file and I didn't want to remove it but the original author is
   Haruhiko Okumura. The text above allow us use it in [x]Ziher.
   This version isn't for sure improved due to SIX modifications so I do
   not think I should send it to him. IMHO it is also less usable for
   other things then strict SIX compatibility - we already have much
   stronger compressions in ZLIB.
   After the LZSS compression the result could be longer then original
   strings. It looks that SIX return original string when the compressed
   one is longer then original + 253. I do not like it because there is
   no marker which informs if string was compressed or not and calling
   decompression procedure for uncompressed data gives broken results.
   I do not want to make the same.
   When file is compressed then SIX put the uncompressed file size in
   first four bytes. Similar situation is with string returned by
   sx_Compress() - first four bytes is uncompressed size in little
   endian order. This has to be done in upper level functions,
   zh_LZSSxCompressMem() and zh_LZSSxCompressFile() intentionally
   do not do that.

 */

#include "zh_api.h"
#include "zh_apifs.h"
#include "zh_item_api.h"
#include "zh_api_error.h"
#include "zh_sx_func.h"

#define ZH_SX_UNCOMPRESED  0xFFFFFFFFUL


/* number of bits for encoded item (position, length) */
#define ITEMBITS           16
/* unused DUMMY bits - who does know why SIX has it? */
#define DUMMYBITS          1
/* number of bits for position offset */
#define OFFSETBITS         ( 12 - DUMMYBITS )
/* the minimum match length to encode as new position */
#define MINLENGTH          3

/* number of bits for match length, 1 bit reserved for ITEM type */
#define LENGTHBITS         ( ITEMBITS - OFFSETBITS - DUMMYBITS )
/* the maximum match length we can encode in LENGTHBITS */
#define MAXLENGTH          ( ( 1 << LENGTHBITS ) + MINLENGTH - 1 )
/* size of ring buffer */
#define RBUFLENGTH         ( 1 << OFFSETBITS )
/* the bit mask for ring buffer */
#define RBUFMASK           ( ( 1 << OFFSETBITS ) - 1 )
/* the bit mask for match length */
#define MATCHMASK          ( ( 1 << LENGTHBITS ) - 1 )
/* get ring buffer index */
#define RBUFINDEX( i )       ( ( i ) & RBUFMASK )

/* get ring buffer offset position from low and high bytes */
#define LZSS_OFFSET( l, h )  ( ( l ) | ( ( h & ~MATCHMASK ) << ( 8 - LENGTHBITS ) ) )
/* get match length from low and high byte */
#define LZSS_LENGTH( l, h )  ( ( ( h ) & MATCHMASK ) + MINLENGTH )

/* create compressed item from match position and length */
#define LZSS_ITEM( o, l )    ( ( ( o ) << LENGTHBITS ) | ( ( l ) - MINLENGTH ) )
/* create low byte of compressed item */
#define LZSS_ITMLO( o, l )   ( ( ZH_UCHAR ) ( o ) )
/* create high byte of compressed item */
#define LZSS_ITMHI( o, l )   ( ( ZH_UCHAR ) ( ( ( ( o ) >> ( 8 - LENGTHBITS ) ) & ~MATCHMASK ) | \
                                              ( ( l ) - MINLENGTH ) ) )
/* maximum size of item set: byte with item type bits plus 8 items */
#define ITEMSETSIZE    ( ( ITEMBITS << 3 ) + 1 )

/* the size of IO buffer for file (de)compression */
#define LZSS_IOBUFLEN  8192

/* uninitialized (dummy) node in compression trees */
#define DUMMYNODE      RBUFLENGTH


typedef struct _ZH_LZSSX_COMPR
{
   PZH_FILE   pInput;
   ZH_BYTE *  inBuffer;
   ZH_SIZE    inBuffSize;
   ZH_SIZE    inBuffPos;
   ZH_SIZE    inBuffRead;
   ZH_BOOL    fInFree;

   PZH_FILE   pOutput;
   ZH_BYTE *  outBuffer;
   ZH_SIZE    outBuffSize;
   ZH_SIZE    outBuffPos;
   ZH_BOOL    fOutFree;

   ZH_SIZE    nOutSize;
   ZH_BOOL    fResult;
   ZH_BOOL    fContinue;

   ZH_UCHAR   ring_buffer[ RBUFLENGTH + MAXLENGTH - 1 ];

   ZH_SHORT   match_offset;
   ZH_SHORT   match_length;
   ZH_SHORT   parent[ RBUFLENGTH + 1 ];
   ZH_SHORT   left  [ RBUFLENGTH + 1 ];
   ZH_SHORT   right [ RBUFLENGTH + 257 ];
}
ZH_LZSSX_COMPR;
typedef ZH_LZSSX_COMPR * PZH_LZSSX_COMPR;

static void zh_LZSSxExit( PZH_LZSSX_COMPR pCompr )
{
   if( pCompr->fInFree )
      zh_xfree( pCompr->inBuffer );
   if( pCompr->fOutFree )
      zh_xfree( pCompr->outBuffer );
   zh_xfree( pCompr );
}

static PZH_LZSSX_COMPR zh_LZSSxInit(
                        PZH_FILE pInput, const ZH_BYTE * pSrcBuf, ZH_SIZE nSrcBuf,
                        PZH_FILE pOutput, ZH_BYTE * pDstBuf, ZH_SIZE nDstBuf )
{
   PZH_LZSSX_COMPR pCompr = ( PZH_LZSSX_COMPR ) zh_xgrab( sizeof( ZH_LZSSX_COMPR ) );

   if( pInput != NULL && nSrcBuf == 0 )
      nSrcBuf = LZSS_IOBUFLEN;
   if( pOutput != NULL && nDstBuf == 0 )
      nDstBuf = LZSS_IOBUFLEN;

   pCompr->pInput      = pInput;
   pCompr->inBuffer    = ( ZH_BYTE * ) ZH_UNCONST( pSrcBuf );
   pCompr->inBuffSize  = nSrcBuf;
   pCompr->inBuffPos   = 0;
   pCompr->inBuffRead  = ( pInput == NULL ) ? nSrcBuf : 0;
   pCompr->fInFree     = ( pInput != NULL && pSrcBuf == NULL );
   pCompr->pOutput     = pOutput;
   pCompr->outBuffer   = pDstBuf;
   pCompr->outBuffSize = nDstBuf;
   pCompr->outBuffPos  = 0;
   pCompr->fOutFree    = ( pOutput != NULL && pDstBuf == NULL );

   pCompr->nOutSize    = 0;
   pCompr->fResult     = ZH_TRUE;
   pCompr->fContinue   = ZH_FALSE;

   if( pCompr->fInFree )
      pCompr->inBuffer    = ( ZH_BYTE * ) zh_xgrab( nDstBuf );
   if( pCompr->fOutFree )
      pCompr->outBuffer   = ( ZH_BYTE * ) zh_xgrab( nDstBuf );

   /* initialize the ring buffer with spaces, because SIX uses
      dynamic ring buffer then we do not have to fill last MAXLENGTH
      characters */
   memset( pCompr->ring_buffer, ' ', RBUFLENGTH - 1 );

   return pCompr;
}

static ZH_BOOL zh_LZSSxFlush( PZH_LZSSX_COMPR pCompr )
{
   if( pCompr->fResult && pCompr->pOutput != NULL )
   {
      if( zh_fileWrite( pCompr->pOutput, pCompr->outBuffer,
                        pCompr->outBuffPos, -1 ) != pCompr->outBuffPos )
      {
         pCompr->fResult = ZH_FALSE;
      }
      else
      {
         pCompr->nOutSize += pCompr->outBuffPos;
         pCompr->outBuffPos = 0;
      }
   }
   return pCompr->fResult;
}

static ZH_BOOL zh_LZSSxWrite( PZH_LZSSX_COMPR pCompr, ZH_UCHAR ucVal )
{
   if( pCompr->fResult )
   {
      if( pCompr->outBuffPos == pCompr->outBuffSize )
         zh_LZSSxFlush( pCompr );
      if( pCompr->outBuffPos < pCompr->outBuffSize )
         pCompr->outBuffer[ pCompr->outBuffPos ] = ucVal;
      else
         pCompr->fResult = ZH_FALSE;
   }
   pCompr->outBuffPos++;
   return pCompr->fResult || pCompr->fContinue;
}

static int zh_LZSSxRead( PZH_LZSSX_COMPR pCompr )
{
   if( pCompr->inBuffPos < pCompr->inBuffRead )
      return ( ZH_UCHAR ) pCompr->inBuffer[ pCompr->inBuffPos++ ];

   if( pCompr->pInput != NULL )
   {
      pCompr->inBuffRead = zh_fileResult( zh_fileRead( pCompr->pInput, pCompr->inBuffer,
                                                       pCompr->inBuffSize, -1 ) );
      pCompr->inBuffPos = 0;
      if( pCompr->inBuffPos < pCompr->inBuffRead )
         return ( ZH_UCHAR ) pCompr->inBuffer[ pCompr->inBuffPos++ ];
   }
   return -1;
}

static ZH_BOOL zh_LZSSxDecode( PZH_LZSSX_COMPR pCompr )
{
   ZH_BOOL fResult = ZH_TRUE;
   ZH_USHORT itemMask;
   int offset, length, rbufidx, c, h;

   rbufidx = RBUFLENGTH - MAXLENGTH;
   itemMask = 0;

   do
   {
      itemMask >>= 1;
      /* Is the next character bitfield with type of next 8 items ? */
      if( ( itemMask & 0x0100 ) == 0 )
      {
         if( ( c = zh_LZSSxRead( pCompr ) ) == -1 )
            break;
         /* simple trick to reduce number of shift operations */
         itemMask = ( ZH_USHORT ) ( c | 0xff00 );
      }
      if( ( c = zh_LZSSxRead( pCompr ) ) == -1 )
         break;

      if( itemMask & 1 ) /* Is the next character normal byte ? */
      {
         if( ! zh_LZSSxWrite( pCompr, ( ZH_UCHAR ) c ) )
         {
            fResult = ZH_FALSE;
            break;
         }
         pCompr->ring_buffer[ rbufidx ] = ( ZH_UCHAR ) c;
         rbufidx = RBUFINDEX( rbufidx + 1 );
      }
      else /* we have an item pair (ring buffer offset : match length) */
      {
         if( ( h = zh_LZSSxRead( pCompr ) ) == -1 )
         {
            #if 0
            fResult = ZH_FALSE;
            #endif
            break;
         }
         offset = LZSS_OFFSET( c, h );   /* get offset to ring buffer */
         length = LZSS_LENGTH( c, h );   /* get match length */
         for( h = 0; h < length; h++ )
         {
            c = pCompr->ring_buffer[ RBUFINDEX( offset + h ) ];
            if( ! zh_LZSSxWrite( pCompr, ( ZH_UCHAR ) c ) )
            {
               fResult = ZH_FALSE;
               break;
            }
            /* SIX does not use additional buffers and dynamically
               overwrite the ring buffer - we have to make exactly
               the same or our results will be differ when
               abs( offset - rbufidx ) < length */
            pCompr->ring_buffer[ rbufidx ] = ( ZH_UCHAR ) c;
            rbufidx = RBUFINDEX( rbufidx + 1 );
         }
      }
   }
   while( fResult );

   if( fResult )
      fResult = zh_LZSSxFlush( pCompr );
   return fResult;
}

static void zh_LZSSxNodeInsert( PZH_LZSSX_COMPR pCompr, int r )
{
   int i, p, cmp;
   ZH_UCHAR * key;

   cmp = 1;
   key = &pCompr->ring_buffer[ r ];
   p   = RBUFLENGTH + 1 + key[ 0 ];
   pCompr->right[ r ] = pCompr->left[ r ] = DUMMYNODE;
   pCompr->match_length = 0;

   for( ;; )
   {
      if( cmp >= 0 )
      {
         if( pCompr->right[ p ] != DUMMYNODE )
            p = pCompr->right[ p ];
         else
         {
            pCompr->right[ p ] = ( ZH_SHORT ) r;
            pCompr->parent[ r ] = ( ZH_SHORT ) p;
            return;
         }
      }
      else
      {
         if( pCompr->left[ p ] != DUMMYNODE )
            p = pCompr->left[ p ];
         else
         {
            pCompr->left[ p ] = ( ZH_SHORT ) r;
            pCompr->parent[ r ] = ( ZH_SHORT ) p;
            return;
         }
      }
      for( i = 1; i < MAXLENGTH; i++ )
      {
         if( ( cmp = key[ i ] - pCompr->ring_buffer[ p + i ] ) != 0 )
            break;
      }
      if( i > pCompr->match_length )
      {
         pCompr->match_offset = ( ZH_SHORT ) p;
         pCompr->match_length = ( ZH_SHORT ) i;
         if( i >= MAXLENGTH )
            break;
      }
   }
   pCompr->parent[ r ] = pCompr->parent[ p ];
   pCompr->left[ r ]   = pCompr->left[ p ];
   pCompr->right[ r ]  = pCompr->right[ p ];
   pCompr->parent[ pCompr->left[ p ] ]  = ( ZH_SHORT ) r;
   pCompr->parent[ pCompr->right[ p ] ] = ( ZH_SHORT ) r;
   if( pCompr->right[ pCompr->parent[ p ] ] == p )
      pCompr->right[ pCompr->parent[ p ] ] = ( ZH_SHORT ) r;
   else
      pCompr->left[ pCompr->parent[ p ] ] = ( ZH_SHORT ) r;
   pCompr->parent[ p ] = DUMMYNODE;
}

static void zh_LZSSxNodeDelete( PZH_LZSSX_COMPR pCompr, int p )
{
   if( pCompr->parent[ p ] != DUMMYNODE )
   {
      int  q;
      if( pCompr->right[ p ] == DUMMYNODE )
         q = pCompr->left[ p ];
      else if( pCompr->left[ p ] == DUMMYNODE )
         q = pCompr->right[ p ];
      else
      {
         q = pCompr->left[ p ];
         if( pCompr->right[ q ] != DUMMYNODE )
         {
            do
            {
               q = pCompr->right[ q ];
            }
            while( pCompr->right[ q ] != DUMMYNODE );
            pCompr->right[ pCompr->parent[ q ] ] = pCompr->left[ q ];
            pCompr->parent[ pCompr->left[ q ] ] = pCompr->parent[ q ];
            pCompr->left[ q ] = pCompr->left[ p ];
            pCompr->parent[ pCompr->left[ p ] ] = ( ZH_SHORT ) q;
         }
         pCompr->right[ q ] = pCompr->right[ p ];
         pCompr->parent[ pCompr->right[ p ] ] = ( ZH_SHORT ) q;
      }
      pCompr->parent[ q ] = pCompr->parent[ p ];
      if( pCompr->right[ pCompr->parent[ p ] ] == p )
         pCompr->right[ pCompr->parent[ p ] ] = ( ZH_SHORT ) q;
      else
         pCompr->left[ pCompr->parent[ p ] ] = ( ZH_SHORT ) q;
      pCompr->parent[ p ] = DUMMYNODE;
   }
}

static ZH_SIZE zh_LZSSxEncode( PZH_LZSSX_COMPR pCompr )
{
   ZH_UCHAR itemSet[ ITEMSETSIZE ];
   ZH_UCHAR itemMask;
   ZH_SIZE nSize = 0;
   ZH_SHORT i, c, len, r, s, item;

   for( i = RBUFLENGTH + 1; i < RBUFLENGTH + 257; i++ )
      pCompr->right[ i ] = DUMMYNODE;
   for( i = 0; i < RBUFLENGTH; i++ )
      pCompr->parent[ i ] = DUMMYNODE;

   itemSet[ 0 ] = 0;
   item = itemMask = 1;
   s = 0;
   r = RBUFLENGTH - MAXLENGTH;

   for( len = 0; len < MAXLENGTH; len++ )
   {
      if( ( c = ( ZH_SHORT ) zh_LZSSxRead( pCompr ) ) == -1 )
         break;
      pCompr->ring_buffer[ r + len ] = ( ZH_UCHAR ) c;
   }
   if( len == 0 )
      return nSize;

   for( i = 1; i <= MAXLENGTH; i++ )
      zh_LZSSxNodeInsert( pCompr, r - i );
   zh_LZSSxNodeInsert( pCompr, r );

   do
   {
      ZH_SHORT last_match_length;

      if( pCompr->match_length > len )
         pCompr->match_length = len;
      if( pCompr->match_length < MINLENGTH )
      {
         pCompr->match_length = 1;
         itemSet[ 0 ] |= itemMask;
         itemSet[ item++ ] = pCompr->ring_buffer[ r ];
      }
      else
      {
         itemSet[ item++ ] = LZSS_ITMLO( pCompr->match_offset,
                                         pCompr->match_length );
         itemSet[ item++ ] = LZSS_ITMHI( pCompr->match_offset,
                                         pCompr->match_length );
      }
      if( ( itemMask <<= 1 ) == 0 )
      {
         for( i = 0; i < item; i++ )
         {
            if( ! zh_LZSSxWrite( pCompr, itemSet[ i ] ) )
               return ( ZH_SIZE ) -1;
         }
         nSize += item;
         itemSet[ 0 ] = 0;
         item = itemMask = 1;
      }
      last_match_length = pCompr->match_length;
      for( i = 0; i < last_match_length &&
                  ( c = ( ZH_SHORT ) zh_LZSSxRead( pCompr ) ) != -1; i++ )
      {
         zh_LZSSxNodeDelete( pCompr, s );
         pCompr->ring_buffer[ s ] = ( ZH_UCHAR ) c;
         if( s < MAXLENGTH - 1 )
            pCompr->ring_buffer[ s + RBUFLENGTH ] = ( ZH_UCHAR ) c;
         s = ( ZH_SHORT ) RBUFINDEX( s + 1 );
         r = ( ZH_SHORT ) RBUFINDEX( r + 1 );
         zh_LZSSxNodeInsert( pCompr, r );
      }
      while( i++ < last_match_length )
      {
         zh_LZSSxNodeDelete( pCompr, s );
         s = ( ZH_SHORT ) RBUFINDEX( s + 1 );
         r = ( ZH_SHORT ) RBUFINDEX( r + 1 );
         if( --len )
            zh_LZSSxNodeInsert( pCompr, r );
      }
   }
   while( len > 0 );

   if( item > 1 )
   {
      for( i = 0; i < item; i++ )
      {
         if( ! zh_LZSSxWrite( pCompr, itemSet[ i ] ) )
            return ( ZH_SIZE ) -1;
      }
      nSize += item;
   }

   if( ! zh_LZSSxFlush( pCompr ) )
      return ( ZH_SIZE ) -1;

   return nSize;
}


ZH_BOOL zh_LZSSxCompressMem( const char * pSrcBuf, ZH_SIZE nSrcLen,
                             char * pDstBuf, ZH_SIZE nDstLen,
                             ZH_SIZE * pnSize )
{
   PZH_LZSSX_COMPR pCompr;
   ZH_SIZE nSize;

   pCompr = zh_LZSSxInit( NULL, ( const ZH_BYTE * ) pSrcBuf, nSrcLen,
                          NULL, ( ZH_BYTE * ) pDstBuf, nDstLen );
   nSize = zh_LZSSxEncode( pCompr );
   zh_LZSSxExit( pCompr );
   if( pnSize )
      *pnSize = nSize;
   return nSize <= nDstLen;
}

ZH_BOOL zh_LZSSxDecompressMem( const char * pSrcBuf, ZH_SIZE nSrcLen,
                               char * pDstBuf, ZH_SIZE nDstLen )
{
   PZH_LZSSX_COMPR pCompr;
   ZH_BOOL fResult;

   pCompr = zh_LZSSxInit( NULL, ( const ZH_BYTE * ) pSrcBuf, nSrcLen,
                          NULL, ( ZH_BYTE * ) pDstBuf, nDstLen );
   fResult = zh_LZSSxDecode( pCompr );
   zh_LZSSxExit( pCompr );
   return fResult;
}

ZH_BOOL zh_LZSSxCompressFile( PZH_FILE pInput, PZH_FILE pOutput, ZH_SIZE * pnSize )
{
   PZH_LZSSX_COMPR pCompr;
   ZH_SIZE nSize;

   pCompr = zh_LZSSxInit( pInput, NULL, 0, pOutput, NULL, 0 );
   nSize = zh_LZSSxEncode( pCompr );
   zh_LZSSxExit( pCompr );
   if( pnSize )
      *pnSize = nSize;
   return nSize != ( ZH_SIZE ) -1;
}

ZH_BOOL zh_LZSSxDecompressFile( PZH_FILE pInput, PZH_FILE pOutput )
{
   PZH_LZSSX_COMPR pCompr;
   ZH_BOOL fResult;

   pCompr = zh_LZSSxInit( pInput, NULL, 0, pOutput, NULL, 0 );
   fResult = zh_LZSSxDecode( pCompr );
   zh_LZSSxExit( pCompr );
   return fResult;
}

ZH_FUNC( SX_FCOMPRESS )
{
   ZH_BOOL fRet = ZH_FALSE;
   const char * szSource = zh_parc( 1 ), * szDestin = zh_parc( 2 );

   if( szSource && *szSource && szDestin && *szDestin )
   {
      PZH_FILE pInput = zh_fileExtOpen( szSource, NULL, FO_READ | FO_DENYNONE |
                                        FXO_DEFAULTS | FXO_SHARELOCK, NULL, NULL );
      if( pInput != NULL )
      {
         PZH_FILE pOutput = zh_fileExtOpen( szDestin, NULL, FO_WRITE |
                                            FO_EXCLUSIVE | FXO_TRUNCATE |
                                            FXO_DEFAULTS | FXO_SHARELOCK, NULL, NULL );
         if( pOutput != NULL )
         {
            /* store uncompressed file size in first 4 bytes of destination
             * file in little endian order - for SIX3 compatibility
             */
            ZH_SIZE nSize = ( ZH_SIZE ) zh_fileSize( pInput );
            if( zh_fileSeek( pInput, 0, FS_SET ) == 0 )
            {
               ZH_BYTE buf[ 4 ];
               ZH_PUT_LE_UINT32( buf, nSize );
               if( zh_fileWrite( pOutput, buf, 4, -1 ) == 4 )
                  fRet = zh_LZSSxCompressFile( pInput, pOutput, NULL );
            }
            zh_fileClose( pOutput );
         }
         zh_fileClose( pInput );
      }
   }
   zh_retl( fRet );
}

ZH_FUNC( SX_FDECOMPRESS )
{
   ZH_BOOL fRet = ZH_FALSE;
   const char * szSource = zh_parc( 1 ), * szDestin = zh_parc( 2 );

   if( szSource && *szSource && szDestin && *szDestin )
   {
      PZH_FILE pInput = zh_fileExtOpen( szSource, NULL, FO_READ | FO_DENYNONE |
                                        FXO_DEFAULTS | FXO_SHARELOCK, NULL, NULL );
      if( pInput != NULL )
      {
         PZH_FILE pOutput = zh_fileExtOpen( szDestin, NULL, FO_WRITE |
                                            FO_EXCLUSIVE | FXO_TRUNCATE |
                                            FXO_DEFAULTS | FXO_SHARELOCK, NULL, NULL );
         if( pOutput != NULL )
         {
            /* skip the four bytes with original file length */
            if( zh_fileSeek( pInput, 4, FS_SET ) == 4 )
               fRet = zh_LZSSxDecompressFile( pInput, pOutput );
            zh_fileClose( pOutput );
         }
         zh_fileClose( pInput );
      }
   }
   zh_retl( fRet );
}

ZH_FUNC( _SX_STRCOMPRESS )
{
   const char * pStr = zh_parc( 1 );

   if( pStr )
   {
      ZH_SIZE nLen = zh_parclen( 1 ), nBuf, nDst;
      char * pBuf;

      /* this is for strict SIX compatibility - in general very bad idea */
      nBuf = nLen + 257;
      pBuf = ( char * ) zh_xgrab( nBuf );
      ZH_PUT_LE_UINT32( pBuf, nLen );
      if( ! zh_LZSSxCompressMem( pStr, nLen, pBuf + 4, nBuf - 4, &nDst ) )
      {
         /* It's not six compatible - it's a workaround for wrongly defined SIX behavior */
         ZH_PUT_LE_UINT32( pBuf, ZH_SX_UNCOMPRESED );
         memcpy( pBuf + 4, pStr, nLen );
         nDst = nLen;
      }
      zh_retclen( pBuf, nDst + 4 );
      zh_xfree( pBuf );
   }
   else
      zh_itemReturn( zh_param( 1, ZH_IT_ANY ) );
}

ZH_FUNC( _SX_STRDECOMPRESS )
{
   ZH_BOOL fOK = ZH_FALSE;
   const char * pStr = zh_parc( 1 );

   if( pStr )
   {
      ZH_SIZE nLen = zh_parclen( 1 ), nBuf;

      if( nLen >= 4 )
      {
         nBuf = ZH_GET_LE_UINT32( pStr );
         if( nBuf == ZH_SX_UNCOMPRESED )
         {
            zh_retclen( pStr + 4, nLen - 4 );
            fOK = ZH_TRUE;
         }
         else
         {
            char * pBuf = ( char * ) zh_xalloc( nBuf + 1 );
            if( pBuf )
            {
               fOK = zh_LZSSxDecompressMem( pStr + 4, nLen - 4, pBuf, nBuf );
               if( fOK )
                  zh_retclen_buffer( pBuf, nBuf );
               else
                  zh_xfree( pBuf );
            }
            else
            {
               PZH_ITEM pItem = zh_errRT_SubstParams( "SIXCOMPRESS", EG_MEM, 0, "possible compressed string corruption", "_SX_STRDECOMPRESS" );
               if( pItem )
                  zh_itemReturnRelease( pItem );
               return;
            }
         }
      }
   }

   if( ! fOK )
      zh_itemReturn( zh_param( 1, ZH_IT_ANY ) );
}
