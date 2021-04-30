/*
 * DBFFPT RDD
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
 *
 * The SIX memo conversion algorithms and some piece of code taken from
 * DBFCDX and DBFFPT
 *    Copyright 1999-2002 Bruno Cantero <bruno@issnet.net>
 *    Copyright 2000-2003 Horacio Roldan <ziher_ar@yahoo.com.ar> (portions)
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

#if defined( ZH_FPT_NO_READLOCK )
#  undef ZH_MEMO_SAFELOCK
#else
/*#  define ZH_MEMO_SAFELOCK */
#endif

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_string_api.h"
#include "zh_error_api.h"
#include "zh_lang_api.h"
#include "zh_init.h"
#include "zh_set.h"
#include "zh_stack.h"
#include "zh_vm.h"
#include "zh_date.h"
#include "zh_rdd_fpt.h"

#include "zh_rtl/rdd_sys.zhh"
#include "zh_codepage_api.h"

#include "../dbf_six/zh_sx_func.h"

#define FPT_TRANS_NONE     0
#define FPT_TRANS_CP       1
#define FPT_TRANS_UNICODE  2

#define FPT_DIRECT_TRANS( p )  ( zh_vmCodepage() != ( p )->area.cdPage ? FPT_TRANS_CP : FPT_TRANS_NONE )

#define FPT_BLOCK_OFFSET( b )  ( ( ZH_FOFFSET ) ( b ) * \
                                 ( ZH_FOFFSET ) pArea->ulMemoBlockSize )

/* temporary cast to suppress 32/64-bit Windows warnings */
#define ZH_ULONGCAST       ZH_ULONG

static ZH_USHORT s_uiRddIdBLOB = ( ZH_USHORT ) -1;
static ZH_USHORT s_uiRddIdFPT  = ( ZH_USHORT ) -1;

static RDDFUNCS fptSuper;

/*
 * generate Run-Time error
 */
static ZH_ERRCODE zh_memoErrorRT( FPTAREAP pArea, ZH_ERRCODE uiGenCode, ZH_ERRCODE uiSubCode,
                                  const char * szFileName, ZH_ERRCODE uiOsCode, ZH_USHORT uiFlags )
{
   ZH_ERRCODE errCode = ZH_FAILURE;

   if( zh_vmRequestQuery() == 0 )
   {
      PZH_ITEM pError = zh_errNew();

      if( uiGenCode == 0 )
         uiGenCode = zh_dbfGetEGcode( uiSubCode );
      if( uiOsCode == 0 && uiSubCode != EDBF_DATATYPE && uiSubCode != EDBF_DATAWIDTH )
         uiOsCode = zh_fsError();

      zh_errPutGenCode( pError, uiGenCode );
      zh_errPutSubCode( pError, uiSubCode );
      if( uiOsCode )
         zh_errPutOsCode( pError, uiOsCode );
      zh_errPutDescription( pError, zh_langDGetErrorDesc( uiGenCode ) );
      if( szFileName )
         zh_errPutFileName( pError, szFileName );
      if( uiFlags )
         zh_errPutFlags( pError, uiFlags );
      errCode = SELF_ERROR( &pArea->area, pError );
      zh_errRelease( pError );
   }
   return errCode;
}


static const char * zh_memoDefaultFileExt( int iType, ZH_USHORT uiRdd )
{
   if( uiRdd == s_uiRddIdBLOB )
      return DBV_MEMOEXT;

   switch( iType )
   {
      case DB_MEMO_DBT:
         return DBT_MEMOEXT;
      case DB_MEMO_FPT:
         return FPT_MEMOEXT;
      case DB_MEMO_SMT:
         return SMT_MEMOEXT;
   }
   return NULL;
}

static int zh_memoDefaultType( LPRDDNODE pRDD, ZH_ULONG ulConnect )
{
   int iType = DB_MEMO_FPT;
   PZH_ITEM pItem = zh_stackAllocItem();

   zh_itemClear( pItem );
   if( SELF_RDDINFO( pRDD, RDDI_MEMOTYPE, ulConnect, pItem ) == ZH_SUCCESS )
      iType = zh_itemGetNI( pItem );
   zh_stackPop();

   return iType;
}

/*
 * Exclusive lock memo file.
 */
static ZH_BOOL zh_fptFileLockEx( FPTAREAP pArea, ZH_BOOL fWait )
{
   ZH_BOOL fRet;

   if( ! pArea->fShared )
   {
      fRet = ZH_TRUE;
   }
   else
   {
      for( ;; )
      {
         fRet = zh_fileLock( pArea->pMemoFile, FPT_LOCKPOS, FPT_LOCKSIZE,
                             FL_LOCK | FLX_EXCLUSIVE | ( fWait ? FLX_WAIT : 0 ) );
         if( fRet || ! fWait )
            break;
         zh_releaseCPU();
      }
   }
   return fRet;
}

/*
 * Shared lock memo file.
 */
static ZH_BOOL zh_fptFileLockSh( FPTAREAP pArea, ZH_BOOL fWait )
{
   ZH_BOOL fRet;

   if( ! pArea->fShared )
   {
      fRet = ZH_TRUE;
   }
   else
   {
      for( ;; )
      {
         fRet = zh_fileLock( pArea->pMemoFile, FPT_LOCKPOS, FPT_LOCKSIZE,
                             FL_LOCK | FLX_SHARED | ( fWait ? FLX_WAIT : 0 ) );
         if( fRet || ! fWait )
            break;
         zh_releaseCPU();
      }
   }
   return fRet;
}

/*
 * Unlock memo file - exclusive lock.
 */
static ZH_BOOL zh_fptFileUnLockEx( FPTAREAP pArea )
{
   if( pArea->fShared )
   {
      zh_fileFlush( pArea->pMemoFile, ZH_FALSE );
      return zh_fileLock( pArea->pMemoFile, FPT_LOCKPOS, FPT_LOCKSIZE, FL_UNLOCK );
   }
   else
      return ZH_TRUE;
}

/*
 * Unlock memo file - shared lock
 */
static ZH_BOOL zh_fptFileUnLockSh( FPTAREAP pArea )
{
   if( pArea->fShared )
   {
      zh_fileFlush( pArea->pMemoFile, ZH_FALSE );
      return zh_fileLock( pArea->pMemoFile, FPT_LOCKPOS, FPT_LOCKSIZE, FL_UNLOCK );
   }
   else
      return ZH_TRUE;
}

/*
 * Check if MEMO file has direct access to data
 */
static ZH_BOOL zh_fptHasDirectAccess( FPTAREAP pArea )
{
   return pArea->bMemoType == DB_MEMO_FPT &&
          ( pArea->uiMemoVersion == DB_MEMOVER_FLEX ||
            pArea->uiMemoVersion == DB_MEMOVER_CLIP );
}

/*
 * Lock root block pointer.
 */
static ZH_BOOL zh_fptRootBlockLock( FPTAREAP pArea )
{
   ZH_BOOL fRet;

   if( ! pArea->fShared )
      fRet = ZH_TRUE;
   else
      fRet = zh_fileLock( pArea->pMemoFile, FPT_ROOTBLOCK_OFFSET, 4,
                          FL_LOCK | FLX_EXCLUSIVE );
   return fRet;
}

/*
 * Unlock root block pointer.
 */
static ZH_BOOL zh_fptRootBlockUnLock( FPTAREAP pArea )
{
   if( pArea->fShared )
   {
      zh_fileFlush( pArea->pMemoFile, ZH_FALSE );
      return zh_fileLock( pArea->pMemoFile, FPT_ROOTBLOCK_OFFSET, 4, FL_UNLOCK );
   }
   else
      return ZH_TRUE;
}

/*
 * Read root block pointer.
 */
static ZH_ERRCODE zh_fptGetRootBlock( FPTAREAP pArea, ZH_ULONG * pulBlock )
{
   *pulBlock = 0;

   if( zh_fptHasDirectAccess( pArea ) )
   {
      ZH_BYTE buffer[ 4 ];

      if( zh_fileReadAt( pArea->pMemoFile, buffer, 4, FPT_ROOTBLOCK_OFFSET ) == 4 )
      {
         *pulBlock = ZH_GET_LE_UINT32( buffer );
         return ZH_SUCCESS;
      }
      else
         return EDBF_READ;
   }
   return EDBF_UNSUPPORTED;
}

/*
 * Write root block pointer.
 */
static ZH_ERRCODE zh_fptPutRootBlock( FPTAREAP pArea, ZH_ULONG ulBlock )
{
   if( zh_fptHasDirectAccess( pArea ) )
   {
      ZH_BYTE buffer[ 4 ];

      ZH_PUT_LE_UINT32( buffer, ulBlock );
      if( zh_fileWriteAt( pArea->pMemoFile, buffer, 4, FPT_ROOTBLOCK_OFFSET ) == 4 )
         return ZH_SUCCESS;
      else
         return EDBF_WRITE;
   }
   return EDBF_UNSUPPORTED;
}

/*
   GARBAGE COLLECTOR:
   I don't have any documentation about it. All I know is reverse engineering
   or analyzes of other sources. If any one can tell me something more about it
   then I will be really glad. I use method one for SixMemo and method 2 for
   FLEX memos.

   Method 1.
   FPTHEADER->reserved2[492]     is a list of free pages,
                                 6 bytes for each page
                                    size[2]  (size in blocks) (little endian)
                                    block[4] (block number) (little endian)
                                 signature1[12] has to be cut down to
                                 10 bytes. The last 2 bytes becomes the
                                 number of entries in free block list (max 82)

   Method 2.
   FPTHEADER->flexDir[4]         is a little endian offset to page
                                 (1024 bytes size) where header is:
                                    type[4] = 1000 (big endian)
                                    size[4] = 1010 (big endian)
                                 then
                                    nItem[2] number of item (little endian)
                                 then 1008 bytes with free blocks list
                                 (max 126 entries) in format:
                                    offset[4]   (little endian)
                                    size[4]     (little endian)
                                 nItem is always odd and after read we have
                                 to recalculate it:
                                    nItem = ( nItem - 3 ) / 4
            if FPTHEADER->flexDir = 0 then we can create it by allocating
            two 1024 bytes pages for flexRev and flexDir page.
               FPTHEADER->flexRev[4] 1024 bytes in next free block
               FPTHEADER->flexDir[4] next 1024 bytes
            flexRev page is copy of flexDir page but the items are stored
            in reversed form size[4] first then offset[4]
               size[4]     (little endian)
               offset[4]   (little endian)
            before writing GC pages (dir and rev, both has to be synced)
            we should first sort the entries moving the shortest blocks
            to the beginning so when we where looking for free block we
            can scan the list from the beginning finding the first one
            large enough. unused bytes in GC page should be filled with 0xAD
            when we free fpt block we should set in its header:
               type[4] = 1001 (big endian)
               size[4] = rest of block size (block size - 8) (big endian)

   It's a tree. The node type
   is marked in the first two bytes of GC page encoded as bit field with
   the number of items 2 - means branch node, 3-leaf node. The value in
   GC node is calculated as:
      ( nItem << 2 ) | FPTGCNODE_TYPE
   Each item in branch node has 12 bytes and inside them 3 32-bit little
   endian values in pages sorted by offset the are:
      offset,size,subpage
   and in pages sorted by size:
      size,offset,subpage
   size and offset is the biggest (the last one) value in subpage(s)
   and subpage is offset of subpage int the file.
   All values in GC pages are in bytes not blocks - it creates the
   FPT file size limit 2^32 - if they will be in blocks then the
   the FPT file size will be limited by 2^32*block_size
   It's time to implement it ;-)
 */

/*
 * Sort GC free memo block list by size.
 */
static void zh_fptSortGCitems( LPMEMOGCTABLE pGCtable )
{
   ZH_BOOL fMoved = ZH_TRUE;
   int l;

   /* this table should be already quite good sorted so this simple
      algorithms will be the most efficient one.
      It will need only one or two passes */
   l = pGCtable->usItems - 1;
   while( fMoved )
   {
      int i, j;

      fMoved = ZH_FALSE;
      j = l;
      for( i = 0; i < j; i++ )
      {
         if( pGCtable->pGCitems[ i ].ulSize > pGCtable->pGCitems[ i + 1 ].ulSize )
         {
            ZH_ULONG ulOffset, ulSize;
            ZH_BOOL fChanged;

            ulOffset = pGCtable->pGCitems[ i + 1 ].ulOffset;
            ulSize   = pGCtable->pGCitems[ i + 1 ].ulSize;
            fChanged = pGCtable->pGCitems[ i + 1 ].fChanged;
            pGCtable->pGCitems[ i + 1 ].ulSize   = pGCtable->pGCitems[ i ].ulSize;
            pGCtable->pGCitems[ i + 1 ].ulOffset = pGCtable->pGCitems[ i ].ulOffset;
            pGCtable->pGCitems[ i + 1 ].fChanged = pGCtable->pGCitems[ i ].fChanged;
            pGCtable->pGCitems[ i ].ulSize       = ulSize;
            pGCtable->pGCitems[ i ].ulOffset     = ulOffset;
            pGCtable->pGCitems[ i ].fChanged     = fChanged;
            fMoved = ZH_TRUE;
            pGCtable->bChanged |= 2;
            l = i;
         }
      }
   }
}

/*
 * Pack GC free memo block list - try to join free blocks.
 */
static void zh_fptPackGCitems( LPMEMOGCTABLE pGCtable )
{
   int i, j;

   /* TODO: better algorithm this primitive one can be too slow for big
      free block list table */
   for( i = 0; i < pGCtable->usItems; i++ )
   {
      if( pGCtable->pGCitems[ i ].ulOffset != 0 &&
          pGCtable->pGCitems[ i ].ulSize != 0 )
      {
         ZH_ULONG ulEnd = pGCtable->pGCitems[ i ].ulOffset + pGCtable->pGCitems[ i ].ulSize;
         if( ulEnd == pGCtable->ulNextBlock )
         {
            pGCtable->ulNextBlock -= pGCtable->pGCitems[ i ].ulSize;
            pGCtable->pGCitems[ i ].ulOffset = pGCtable->pGCitems[ i ].ulSize = 0;
            pGCtable->bChanged |= 2;
            i = -1;
         }
         else
         {
            for( j = i + 1; j < pGCtable->usItems; j++ )
            {
               if( ulEnd == pGCtable->pGCitems[ j ].ulOffset )
               {
                  pGCtable->pGCitems[ i ].ulSize  += pGCtable->pGCitems[ j ].ulSize;
                  pGCtable->pGCitems[ i ].fChanged = ZH_TRUE;
                  pGCtable->pGCitems[ j ].ulOffset = pGCtable->pGCitems[ j ].ulSize = 0;
                  pGCtable->bChanged |= 2;
                  i = -1;
                  break;
               }
            }
         }
      }
   }

   /* remove empty items */
   for( i = j = 0; i < pGCtable->usItems; i++ )
   {
      if( pGCtable->pGCitems[ i ].ulOffset != 0 &&
          pGCtable->pGCitems[ i ].ulSize != 0 )
      {
         if( i > j )
         {
            pGCtable->pGCitems[ j ].ulOffset = pGCtable->pGCitems[ i ].ulOffset;
            pGCtable->pGCitems[ j ].ulSize   = pGCtable->pGCitems[ i ].ulSize;
            pGCtable->pGCitems[ j ].fChanged = pGCtable->pGCitems[ i ].fChanged;
         }
         j++;
      }
   }
   pGCtable->usItems = ( ZH_USHORT ) j;
}

/*
 * Write proper header into modified GC free memo blocks.
 */
static ZH_ERRCODE zh_fptWriteGCitems( FPTAREAP pArea, LPMEMOGCTABLE pGCtable, ZH_USHORT usItem )
{
   FPTBLOCK fptBlock;
   ZH_ERRCODE errCode = ZH_SUCCESS;
   int i;
#if 0
   int iStart, iStop;
#endif

   ZH_SYMBOL_UNUSED( usItem );

#if 0
   if( usItem == 0 )
   {
      iStart = 0;
      iStop = pGCtable->usItems;
   }
   else
   {
      iStart = usItem;
      iStop = usItem + 1;
   }
#endif

   for( i = 0; i < pGCtable->usItems; i++ )
   {
      if( pGCtable->pGCitems[ i ].fChanged )
      {
         if( ( pArea->uiMemoVersion == DB_MEMOVER_FLEX ||
               pArea->uiMemoVersion == DB_MEMOVER_CLIP ) &&
             /* TODO: check what FLEX/CL53 exactly does in such situations */
             /* Tests show that FLEX/CL53 does not reuse larger blocks
                which can leave 8 or less dummy bytes so such problem
                does not exists. [druzus] */
             pGCtable->pGCitems[ i ].ulSize * pArea->ulMemoBlockSize >=
             sizeof( FPTBLOCK ) )
         {
            ZH_PUT_BE_UINT32( fptBlock.type, FPTIT_FLEX_UNUSED );
            ZH_PUT_BE_UINT32( fptBlock.size, pArea->ulMemoBlockSize *
                              pGCtable->pGCitems[ i ].ulSize - sizeof( FPTBLOCK ) );
            if( zh_fileWriteAt( pArea->pMemoFile, &fptBlock,
                                sizeof( FPTBLOCK ),
                                FPT_BLOCK_OFFSET( pGCtable->pGCitems[ i ].ulOffset ) ) !=
                sizeof( FPTBLOCK ) )
            {
               errCode = EDBF_WRITE;
            }
            pArea->fMemoFlush = ZH_TRUE;
         }
         pGCtable->pGCitems[ i ].fChanged = ZH_FALSE;
      }
   }
   return errCode;
}

/*
 * Add new block to GC free memo blocks list.
 */
static ZH_ERRCODE zh_fptGCfreeBlock( FPTAREAP pArea, LPMEMOGCTABLE pGCtable,
                                     ZH_ULONG ulOffset, ZH_ULONG ulByteSize, ZH_BOOL fRaw )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;
   ZH_ULONG ulSize;

   if( pArea->bMemoType == DB_MEMO_DBT )
   {
      return ZH_SUCCESS;
   }
   else if( pArea->bMemoType == DB_MEMO_FPT && ! fRaw )
   {
      if( ulByteSize == 0 )
      {
         FPTBLOCK fptBlock;

         if( zh_fileReadAt( pArea->pMemoFile, &fptBlock,
                            sizeof( FPTBLOCK ), FPT_BLOCK_OFFSET( ulOffset ) ) ==
             sizeof( FPTBLOCK ) )
         {
            ulByteSize = ZH_GET_BE_UINT32( fptBlock.size ) + sizeof( FPTBLOCK );
         }
      }
      else
      {
         ulByteSize += sizeof( FPTBLOCK );
      }
   }

   ulSize = ( ulByteSize + pArea->ulMemoBlockSize - 1 ) / pArea->ulMemoBlockSize;

   if( ulSize == 0 )
   {
      return EDBF_CORRUPT;
   }

   if( ulOffset + ulSize == pGCtable->ulNextBlock )
   {
      pGCtable->ulNextBlock -= ulSize;
      pGCtable->bChanged |= 1;
      zh_fptPackGCitems( pGCtable );
   }
   else
   {
      ZH_BOOL fChanged = ZH_FALSE;
      int i;

      for( i = 0; i < pGCtable->usItems; i++ )
      {
         if( pGCtable->pGCitems[ i ].ulOffset + pGCtable->pGCitems[ i ].ulSize == ulOffset )
         {
            ulOffset = pGCtable->pGCitems[ i ].ulOffset;
            ulSize   = pGCtable->pGCitems[ i ].ulSize += ulSize;
            fChanged = pGCtable->pGCitems[ i ].fChanged = ZH_TRUE;
            break;
         }
         if( pGCtable->pGCitems[ i ].ulOffset == ulOffset + ulSize )
         {
            pGCtable->pGCitems[ i ].ulOffset = ulOffset;
            ulSize   = pGCtable->pGCitems[ i ].ulSize += ulSize;
            fChanged = pGCtable->pGCitems[ i ].fChanged = ZH_TRUE;
            break;
         }
      }
      if( ! fChanged )
      {
         if( pGCtable->usItems <= pGCtable->usMaxItem )
         {
            if( pGCtable->pGCitems == NULL )
            {
               pGCtable->pGCitems = ( LPMEMOGCITEM ) zh_xgrab( sizeof( MEMOGCITEM ) * ( pGCtable->usMaxItem + 1 ) );
            }
            pGCtable->pGCitems[ pGCtable->usItems ].ulOffset = ulOffset;
            pGCtable->pGCitems[ pGCtable->usItems ].ulSize   = ulSize;
            pGCtable->pGCitems[ pGCtable->usItems ].fChanged = fChanged = ZH_TRUE;
            pGCtable->usItems++;
         }
         else if( pGCtable->pGCitems[ 0 ].ulSize < ulSize )
         {
            if( pGCtable->ulNextBlock == pGCtable->pGCitems[ 0 ].ulOffset +
                pGCtable->pGCitems[ 0 ].ulSize )
            {
               pGCtable->ulNextBlock -= pGCtable->pGCitems[ 0 ].ulSize;
            }
            else if( pGCtable->pGCitems[ 0 ].fChanged )
            {
               errCode = zh_fptWriteGCitems( pArea, pGCtable, 0 );
            }
            pGCtable->pGCitems[ 0 ].ulOffset = ulOffset;
            pGCtable->pGCitems[ 0 ].ulSize   = ulSize;
            pGCtable->pGCitems[ 0 ].fChanged = fChanged = ZH_TRUE;
         }
      }

      if( fChanged )
      {
         pGCtable->bChanged |= 2;
         zh_fptPackGCitems( pGCtable );
         zh_fptSortGCitems( pGCtable );
      }
   }

   return errCode;
}

/*
 * Get free memo block from GC free memo blocks list or allocate new one.
 */
static ZH_ERRCODE zh_fptGCgetFreeBlock( FPTAREAP pArea, LPMEMOGCTABLE pGCtable,
                                        ZH_ULONG * ulOffset, ZH_ULONG ulByteSize,
                                        ZH_BOOL fRaw )
{
   ZH_BOOL fAlloc = ZH_FALSE;
   ZH_ULONG ulSize;
   int i;


   if( pArea->bMemoType == DB_MEMO_SMT || fRaw )
   {
      ulSize = ( ulByteSize + pArea->ulMemoBlockSize - 1 ) /
               pArea->ulMemoBlockSize;
   }
   else if( pArea->bMemoType == DB_MEMO_FPT )
   {
      ulSize = ( ulByteSize + sizeof( FPTBLOCK ) + pArea->ulMemoBlockSize - 1 ) /
               pArea->ulMemoBlockSize;
   }
   else if( pArea->bMemoType == DB_MEMO_DBT )
   {
      ulSize = ( ulByteSize + pArea->ulMemoBlockSize ) /
               pArea->ulMemoBlockSize;
   }
   else
   {
      ulSize = ( ulByteSize + pArea->ulMemoBlockSize - 1 ) /
               pArea->ulMemoBlockSize;
   }

   for( i = 0; i < pGCtable->usItems; i++ )
   {
      if( pGCtable->pGCitems[ i ].ulSize >= ulSize )
      {
         *ulOffset = pGCtable->pGCitems[ i ].ulOffset;
         pGCtable->pGCitems[ i ].ulOffset += ulSize;
         pGCtable->pGCitems[ i ].ulSize   -= ulSize;
         if( pGCtable->pGCitems[ i ].ulSize == 0 )
         {
            while( ++i < pGCtable->usItems )
            {
               pGCtable->pGCitems[ i - 1 ].ulOffset = pGCtable->pGCitems[ i ].ulOffset;
               pGCtable->pGCitems[ i - 1 ].ulSize   = pGCtable->pGCitems[ i ].ulSize;
            }
            pGCtable->usItems--;
         }
         else
         {
            pGCtable->pGCitems[ i ].fChanged = ZH_TRUE;
            zh_fptSortGCitems( pGCtable );
         }
         pGCtable->bChanged |= 2;
         fAlloc = ZH_TRUE;
         break;
      }
   }
   if( ! fAlloc )
   {
      *ulOffset = pGCtable->ulNextBlock;
      pGCtable->ulNextBlock += ulSize;
      pGCtable->bChanged |= 1;
   }
   return ZH_SUCCESS;
}

/*
 * Init GC table free memo block list.
 */
static void zh_fptInitGCdata( LPMEMOGCTABLE pGCtable )
{
   memset( pGCtable, 0, sizeof( MEMOGCTABLE ) );
}

/*
 * Clean GC table free memo block list.
 */
static void zh_fptDestroyGCdata( LPMEMOGCTABLE pGCtable )
{
   if( pGCtable->pGCitems != NULL )
   {
      zh_xfree( pGCtable->pGCitems );
      pGCtable->pGCitems = NULL;
      pGCtable->usItems = 0;
   }
   pGCtable->bChanged = 0;
}

/*
 * Read GC table from memo file.
 */
static ZH_ERRCODE zh_fptReadGCdata( FPTAREAP pArea, LPMEMOGCTABLE pGCtable )
{
   ZH_SIZE nRead;

   zh_fptDestroyGCdata( pGCtable );
   memset( &pGCtable->fptHeader, 0, sizeof( FPTHEADER ) );

   nRead = zh_fileReadAt( pArea->pMemoFile, &pGCtable->fptHeader,
                          sizeof( FPTHEADER ), 0 );
   if( nRead >= 512 && nRead != ( ZH_SIZE ) FS_ERROR )
   {
      int i;

      if( pArea->bMemoType == DB_MEMO_SMT || pArea->bMemoType == DB_MEMO_DBT )
         pGCtable->ulNextBlock = ZH_GET_LE_UINT32( pGCtable->fptHeader.nextBlock );
      else
         pGCtable->ulNextBlock = ZH_GET_BE_UINT32( pGCtable->fptHeader.nextBlock );
      pGCtable->ulPrevBlock = pGCtable->ulNextBlock;

      if( pArea->uiMemoVersion == DB_MEMOVER_SIX ||
          pArea->bMemoType == DB_MEMO_SMT )
      {
         pGCtable->bType = DB_MEMOVER_SIX;
         pGCtable->usMaxItem = MAX_SIXFREEBLOCKS;
         pGCtable->usItems = ZH_GET_LE_UINT16( pGCtable->fptHeader.nGCitems );
         if( pGCtable->usItems > pGCtable->usMaxItem )
         {
            return EDBF_CORRUPT;
         }

         pGCtable->pGCitems = ( LPMEMOGCITEM ) zh_xgrab( sizeof( MEMOGCITEM ) * ( pGCtable->usMaxItem + 1 ) );

         for( i = 0; i < pGCtable->usItems; i++ )
         {
            pGCtable->pGCitems[ i ].ulSize   = ZH_GET_LE_UINT16( &pGCtable->fptHeader.reserved2[ i * 6 ] );
            pGCtable->pGCitems[ i ].ulOffset = ZH_GET_LE_UINT32( &pGCtable->fptHeader.reserved2[ i * 6 + 2 ] );
            pGCtable->pGCitems[ i ].fChanged = ZH_FALSE;
         }
      }
      else if( pArea->bMemoType == DB_MEMO_FPT &&
               ( pArea->uiMemoVersion == DB_MEMOVER_FLEX ||
                 pArea->uiMemoVersion == DB_MEMOVER_CLIP ) )
      {
         FPTBLOCK fptBlock;
         ZH_BYTE * bPageBuf;

         pGCtable->bType = DB_MEMOVER_FLEX;
         pGCtable->usMaxItem = MAX_FLEXFREEBLOCKS;
         pGCtable->ulRevPage = ZH_GET_LE_UINT32( pGCtable->fptHeader.flexRev );
         pGCtable->ulDirPage = ZH_GET_LE_UINT32( pGCtable->fptHeader.flexDir );
         pGCtable->ulCounter = ZH_GET_LE_UINT32( pGCtable->fptHeader.counter );
         if( pGCtable->ulDirPage )
         {
            if( zh_fileReadAt( pArea->pMemoFile, &fptBlock,
                               sizeof( FPTBLOCK ), pGCtable->ulDirPage ) !=
                sizeof( FPTBLOCK ) ||
                ZH_GET_BE_UINT32( fptBlock.type ) != FPTIT_FLEX_GC )
            {
               return EDBF_CORRUPT;
            }
            pGCtable->ulSize = ZH_GET_BE_UINT32( fptBlock.size );
            bPageBuf = ( ZH_BYTE * ) zh_xgrab( pGCtable->ulSize );
            if( zh_fileReadAt( pArea->pMemoFile, bPageBuf,
                               pGCtable->ulSize,
                               pGCtable->ulDirPage + sizeof( FPTBLOCK ) ) !=
                pGCtable->ulSize )
            {
               zh_xfree( bPageBuf );
               return EDBF_CORRUPT;
            }
            pGCtable->usMaxItem = ( ZH_USHORT ) ( ( pGCtable->ulSize - 2 ) >> 3 );
            pGCtable->usItems = ( ZH_GET_LE_UINT16( bPageBuf ) - 3 ) >> 2;

            pGCtable->pGCitems = ( LPMEMOGCITEM ) zh_xgrab( sizeof( MEMOGCITEM ) *
                     ( ZH_MIN( pGCtable->usItems, pGCtable->usMaxItem ) + 1 ) );

            for( i = 0; i < pGCtable->usItems; i++ )
            {
               pGCtable->pGCitems[ i ].ulOffset = ZH_GET_LE_UINT32( &bPageBuf[ i * 8 + 2 ] ) /
                                                  pArea->ulMemoBlockSize;
               pGCtable->pGCitems[ i ].ulSize = ZH_GET_LE_UINT32( &bPageBuf[ i * 8 + 6 ] ) /
                                                pArea->ulMemoBlockSize;
               pGCtable->pGCitems[ i ].fChanged = ZH_FALSE;
            }
            zh_xfree( bPageBuf );
         }
      }

      if( pGCtable->pGCitems )
         zh_fptSortGCitems( pGCtable );

      return ZH_SUCCESS;
   }
   return EDBF_READ;
}

/*
 * Write GC table into memo file.
 */
static ZH_ERRCODE zh_fptWriteGCdata( FPTAREAP pArea, LPMEMOGCTABLE pGCtable )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   if( pGCtable->bChanged > 0 )
   {
      ZH_ULONG ulHdrSize = 512;
      int i, j;

      if( pGCtable->bType == DB_MEMOVER_SIX )
      {
         ZH_USHORT usItems = ZH_MIN( pGCtable->usItems, pGCtable->usMaxItem );
         ZH_PUT_LE_UINT16( pGCtable->fptHeader.nGCitems, usItems );
         memset( pGCtable->fptHeader.reserved2, 0, sizeof( pGCtable->fptHeader.reserved2 ) );
         j = pGCtable->usItems - usItems;
         for( i = j; i < pGCtable->usItems; i++ )
         {
            ZH_PUT_LE_UINT16( &pGCtable->fptHeader.reserved2[ ( i - j ) * 6 ],
                              ( ( ZH_USHORT ) pGCtable->pGCitems[ i ].ulSize ) );
            ZH_PUT_LE_UINT32( &pGCtable->fptHeader.reserved2[ ( i - j ) * 6 + 2 ],
                              pGCtable->pGCitems[ i ].ulOffset );
         }
      }
      else if( pGCtable->bType == DB_MEMOVER_FLEX )
      {
         ulHdrSize = sizeof( FPTHEADER );
         pGCtable->ulCounter++;
         if( pGCtable->usItems == 0 && pGCtable->ulDirPage )
         {
            ZH_ULONG ulOffset = pGCtable->ulDirPage;
            ZH_ULONG ulSize = ( pGCtable->ulSize + pArea->ulMemoBlockSize - 1 ) /
                              pArea->ulMemoBlockSize;
            if( pGCtable->ulRevPage )
            {
               ulSize <<= 1;
               if( pGCtable->ulDirPage > pGCtable->ulRevPage )
               {
                  ulOffset = pGCtable->ulRevPage;
               }
            }
            ulOffset /= pArea->ulMemoBlockSize;
            if( ulOffset + ulSize == pGCtable->ulNextBlock )
            {
               pGCtable->ulDirPage = pGCtable->ulRevPage = 0;
               pGCtable->ulNextBlock -= ulSize;
            }
         }
         else if( pGCtable->usItems > 0 && ! pGCtable->ulDirPage )
         {
            pGCtable->ulSize = FLEXGCPAGE_SIZE;
            errCode = zh_fptGCgetFreeBlock( pArea, pGCtable,
                                &pGCtable->ulDirPage, pGCtable->ulSize, ZH_FALSE );
            if( errCode == ZH_SUCCESS )
            {
               pGCtable->ulDirPage *= pArea->ulMemoBlockSize;
               errCode = zh_fptGCgetFreeBlock( pArea, pGCtable,
                                &pGCtable->ulRevPage, pGCtable->ulSize, ZH_FALSE );
               pGCtable->ulRevPage *= pArea->ulMemoBlockSize;
            }
            pGCtable->bChanged |= 2;
         }
         if( pGCtable->ulDirPage && pGCtable->bChanged > 1 )
         {
            FPTBLOCK fptBlock;
            ZH_BYTE * bPageBuf;
            ZH_USHORT usItems = ZH_MIN( pGCtable->usItems, pGCtable->usMaxItem );

            ZH_PUT_BE_UINT32( fptBlock.type, FPTIT_FLEX_GC );
            ZH_PUT_BE_UINT32( fptBlock.size, pGCtable->ulSize );
            bPageBuf = ( ZH_BYTE * ) zh_xgrab( pGCtable->ulSize );
            memset( bPageBuf, 0xAD, pGCtable->ulSize );
            ZH_PUT_LE_UINT16( bPageBuf, ( ( ZH_USHORT ) usItems << 2 ) + 3 );
            j = pGCtable->usItems - usItems;
            for( i = j; i < pGCtable->usItems; i++ )
            {
               ZH_PUT_LE_UINT32( &bPageBuf[ ( i - j ) * 8 + 2 ],
                                 pGCtable->pGCitems[ i ].ulOffset * pArea->ulMemoBlockSize );
               ZH_PUT_LE_UINT32( &bPageBuf[ ( i - j ) * 8 + 6 ],
                                 pGCtable->pGCitems[ i ].ulSize * pArea->ulMemoBlockSize );
            }
            if( zh_fileWriteAt( pArea->pMemoFile, &fptBlock,
                                sizeof( FPTBLOCK ), pGCtable->ulDirPage ) !=
                sizeof( FPTBLOCK ) ||
                zh_fileWriteAt( pArea->pMemoFile, bPageBuf, pGCtable->ulSize,
                                pGCtable->ulDirPage + sizeof( FPTBLOCK ) ) !=
                pGCtable->ulSize )
            {
               errCode = EDBF_WRITE;
            }
            else if( pGCtable->ulRevPage )
            {
               for( i = j; i < pGCtable->usItems; i++ )
               {
                  ZH_PUT_LE_UINT32( &bPageBuf[ ( i - j ) * 8 + 2 ],
                                    ( ( ZH_USHORT ) pGCtable->pGCitems[ i ].ulSize * pArea->ulMemoBlockSize ) );
                  ZH_PUT_LE_UINT32( &bPageBuf[ ( i - j ) * 8 + 6 ],
                                    pGCtable->pGCitems[ i ].ulOffset * pArea->ulMemoBlockSize );
               }
               if( zh_fileWriteAt( pArea->pMemoFile, &fptBlock,
                                   sizeof( FPTBLOCK ), pGCtable->ulRevPage ) !=
                   sizeof( FPTBLOCK ) ||
                   zh_fileWriteAt( pArea->pMemoFile, bPageBuf, pGCtable->ulSize,
                                   pGCtable->ulRevPage + sizeof( FPTBLOCK ) ) !=
                   pGCtable->ulSize )
               {
                  errCode = EDBF_WRITE;
               }
            }
            zh_xfree( bPageBuf );
         }
         ZH_PUT_LE_UINT32( pGCtable->fptHeader.flexRev, pGCtable->ulRevPage );
         ZH_PUT_LE_UINT32( pGCtable->fptHeader.flexDir, pGCtable->ulDirPage );
         ZH_PUT_LE_UINT32( pGCtable->fptHeader.counter, pGCtable->ulCounter );
      }

      if( pGCtable->bChanged > 1 && errCode == ZH_SUCCESS )
      {
         errCode = zh_fptWriteGCitems( pArea, pGCtable, 0 );
      }
      if( errCode == ZH_SUCCESS )
      {
         if( pArea->bMemoType == DB_MEMO_SMT || pArea->bMemoType == DB_MEMO_DBT )
            ZH_PUT_LE_UINT32( pGCtable->fptHeader.nextBlock, pGCtable->ulNextBlock );
         else
            ZH_PUT_BE_UINT32( pGCtable->fptHeader.nextBlock, pGCtable->ulNextBlock );
         if( zh_fileWriteAt( pArea->pMemoFile, &pGCtable->fptHeader,
                             ulHdrSize, 0 ) != ulHdrSize )
         {
            errCode = EDBF_WRITE;
         }
         else if( pGCtable->ulNextBlock < pGCtable->ulPrevBlock )
         {
            /* trunc file */
            zh_fileTruncAt( pArea->pMemoFile, FPT_BLOCK_OFFSET( pGCtable->ulNextBlock ) );
         }
      }
      pArea->fMemoFlush = ZH_TRUE;
      pGCtable->bChanged = 0;
   }
   return errCode;
}

/*
 * Return the size of memo.
 */
static ZH_ULONG zh_fptGetMemoLen( FPTAREAP pArea, ZH_USHORT uiIndex )
{
   ZH_ULONG ulBlock, ulSize, ulType;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptGetMemoLen(%p, %hu)", ( void * ) pArea, uiIndex ) );

   if( zh_dbfGetMemoData( ( DBFAREAP ) pArea, uiIndex - 1, &ulBlock, &ulSize,
                          &ulType ) == ZH_SUCCESS )
   {
      if( ulBlock != 0 )
      {
         if( ulSize == 0 && ( pArea->bMemoType == DB_MEMO_DBT ||
                              pArea->bMemoType == DB_MEMO_FPT ) )
         {
            ZH_FOFFSET fOffset = FPT_BLOCK_OFFSET( ulBlock );
            FPTBLOCK fptBlock;

            if( pArea->bMemoType == DB_MEMO_DBT )
            {
               ZH_BYTE pBlock[ DBT_DEFBLOCKSIZE ];
               ZH_SIZE n;

               do
               {
                  ZH_SIZE nLen = zh_fileReadAt( pArea->pMemoFile, pBlock, DBT_DEFBLOCKSIZE, fOffset );
                  if( nLen == 0 || nLen == ( ZH_SIZE ) FS_ERROR )
                     break;
                  fOffset += nLen;
                  n = 0;
                  while( n < nLen && pBlock[ n ] != 0x1A )
                     n++;
                  ulSize += ( ZH_ULONGCAST ) n;
               }
               while( n == DBT_DEFBLOCKSIZE );
            }
            else if( zh_fileReadAt( pArea->pMemoFile, &fptBlock,
                                    sizeof( FPTBLOCK ), fOffset ) ==
                     sizeof( FPTBLOCK ) )
               ulSize = ZH_GET_BE_UINT32( fptBlock.size );
         }
         return ulSize;
      }
   }
   return 0;
}

/*
 * Return the type of memo.
 */
static const char * zh_fptGetMemoType( FPTAREAP pArea, ZH_USHORT uiIndex )
{
   ZH_ULONG ulBlock, ulSize, ulType;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptGetMemoType(%p, %hu)", ( void * ) pArea, uiIndex ) );

   if( zh_dbfGetMemoData( ( DBFAREAP ) pArea, uiIndex - 1, &ulBlock, &ulSize,
                          &ulType ) == ZH_SUCCESS )
   {
      if( ulBlock != 0 )
      {
         if( ulType == 0 && pArea->bMemoType == DB_MEMO_FPT )
         {
            FPTBLOCK fptBlock;
            if( zh_fileReadAt( pArea->pMemoFile, &fptBlock,
                               sizeof( FPTBLOCK ), FPT_BLOCK_OFFSET( ulBlock ) ) !=
                sizeof( FPTBLOCK ) )
               return "U";
            ulType = ZH_GET_BE_UINT32( fptBlock.type );
         }
      }

      if( ulType == 0 )
         return "M";

      if( pArea->bMemoType == DB_MEMO_FPT )
      {
         switch( ulType )
         {
            case FPTIT_SIX_LNUM:
            case FPTIT_SIX_DNUM:
               return "N";
            case FPTIT_SIX_LDATE:
               return "D";
            case FPTIT_SIX_LOG:
               return "L";
            case FPTIT_SIX_CHAR:
               return "M";
            case FPTIT_SIX_ARRAY:
               return "A";
#if 0
            case FPTIT_SIX_BLOCK:
            case FPTIT_SIX_VREF:
            case FPTIT_SIX_MREF:
#endif
            case FPTIT_FLEX_ARRAY:
            case FPTIT_FLEX_VOARR:
               return "A";
            case FPTIT_FLEX_OBJECT:
            case FPTIT_FLEX_VOOBJ:
               return "O";
            case FPTIT_FLEX_NIL:
               return "U";
            case FPTIT_FLEX_TRUE:
            case FPTIT_FLEX_FALSE:
               return "L";
            case FPTIT_FLEX_LDATE:
               return "D";
            case FPTIT_FLEX_CHAR:
            case FPTIT_FLEX_UCHAR:
            case FPTIT_FLEX_SHORT:
            case FPTIT_FLEX_USHORT:
            case FPTIT_FLEX_LONG:
            case FPTIT_FLEX_ULONG:
            case FPTIT_FLEX_DOUBLE:
            case FPTIT_FLEX_LDOUBLE:
               return "N";
            case FPTIT_TEXT:
               return "M";
            case FPTIT_PICT:
            case FPTIT_FLEX_COMPRCH:
               return "C";
         }
         return "U";
      }
      else if( pArea->bMemoType == DB_MEMO_SMT )
      {
         switch( ulType )
         {
            case SMT_IT_NIL:
               return "U";
            case SMT_IT_CHAR:
               return "M";
            case SMT_IT_INT:
            case SMT_IT_DOUBLE:
               return "N";
            case SMT_IT_DATE:
               return "D";
            case SMT_IT_LOGICAL:
               return "L";
            case SMT_IT_ARRAY:
               return "A";
         }
         return "U";
      }
      return "M";
   }

   return "U";
}

/*
 * Calculate the size of SMT memo item
 */
static ZH_ULONG zh_fptCountSMTItemLength( FPTAREAP pArea, PZH_ITEM pItem,
                                          ZH_ULONG * pulArrayCount, int iTrans )
{
   ZH_ULONG ulLen, u, ulSize;

   switch( zh_itemType( pItem ) )
   {
      case ZH_IT_ARRAY: /* ZH_IT_OBJECT == ZH_IT_ARRAY */
         ( *pulArrayCount )++;
         ulSize = 3;
         ulLen = ( ZH_ULONGCAST ) zh_arrayLen( pItem );
         if( ulLen > 0xFFFF )
            ulLen = 0xFFFF;
         for( u = 1; u <= ulLen; u++ )
         {
            ulSize += zh_fptCountSMTItemLength( pArea, zh_arrayGetItemPtr( pItem, u ), pulArrayCount, iTrans );
         }
         break;
      case ZH_IT_MEMO:
      case ZH_IT_STRING:
         if( iTrans == FPT_TRANS_UNICODE )
         {
            ulLen = ( ZH_ULONGCAST ) zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE, NULL, 0xFFFF ) * sizeof( ZH_WCHAR );
         }
         else
         {
            ulLen = ( ZH_ULONGCAST ) zh_itemGetCLen( pItem );
            if( iTrans == FPT_TRANS_CP && ulLen > 0 )
            {
               ulLen = ( ZH_ULONGCAST ) zh_cdpnDup2Len( zh_itemGetCPtr( pItem ), ulLen, 0xFFFF,
                                                        zh_vmCodepage(), pArea->area.cdPage );
            }
            else
            {
               if( ulLen > 0xFFFF )
                  ulLen = 0xFFFF;
            }
         }
         ulSize = ulLen + 3;
         break;
      case ZH_IT_LOGICAL:
         ulSize = 2;
         break;
      case ZH_IT_DATE:
      case ZH_IT_TIMESTAMP:
         ulSize = 5;
         break;
      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
      {
         ZH_MAXINT iVal = zh_itemGetNInt( pItem );
         if( ZH_LIM_INT32( iVal ) )
         {
            ulSize = 5;
            break;
         }
      }
      /* fallthrough */
      case ZH_IT_DOUBLE:
         ulSize = 11;
         break;
      case ZH_IT_NIL:
      default:
         ulSize = 1;
   }
   return ulSize;
}

/*
 * Calculate the size of SMT memo data
 */
static ZH_ERRCODE zh_fptCountSMTDataLength( FPTAREAP pArea, ZH_FOFFSET * pfOffset )
{
   ZH_USHORT u, uiSize;
   ZH_BYTE buffer[ 2 ];

   if( zh_fileReadAt( pArea->pMemoFile, buffer, 1, *pfOffset ) != 1 )
      return EDBF_READ;

   *pfOffset += 1;
   switch( buffer[ 0 ] )
   {
      case SMT_IT_ARRAY:
         if( zh_fileReadAt( pArea->pMemoFile, buffer, 2, *pfOffset ) != 2 )
            return EDBF_READ;
         *pfOffset += 2;
         uiSize = ZH_GET_LE_UINT16( buffer );
         for( u = 0; u < uiSize; u++ )
         {
            ZH_ERRCODE errCode = zh_fptCountSMTDataLength( pArea, pfOffset );
            if( errCode != ZH_SUCCESS )
               return errCode;
         }
         break;

      case SMT_IT_CHAR:
         if( zh_fileReadAt( pArea->pMemoFile, buffer, 2, *pfOffset ) != 2 )
            return EDBF_READ;
         uiSize = ZH_GET_LE_UINT16( buffer );
         *pfOffset += uiSize + 2;
         break;

      case SMT_IT_INT:
      case SMT_IT_DATE:
         *pfOffset += 4;
         break;

      case SMT_IT_DOUBLE:
         *pfOffset += 10;
         break;

      case SMT_IT_LOGICAL:
         *pfOffset += 1;
         break;

      case SMT_IT_NIL:
         break;

      default:
         return EDBF_CORRUPT;
   }

   return ZH_SUCCESS;
}

/*
 * Write VM item as SMT memos.
 */
static void zh_fptStoreSMTItem( FPTAREAP pArea, PZH_ITEM pItem, ZH_BYTE ** bBufPtr, int iTrans )
{
   ZH_ULONG ulLen, u;

   switch( zh_itemType( pItem ) )
   {
      case ZH_IT_ARRAY:
         *( *bBufPtr )++ = SMT_IT_ARRAY;
         ulLen = ( ZH_ULONGCAST ) zh_arrayLen( pItem );
         if( ulLen > 0xFFFF )
            ulLen = 0xFFFF;
         ZH_PUT_LE_UINT16( *bBufPtr, ulLen );
         *bBufPtr += 2;
         for( u = 1; u <= ulLen; u++ )
         {
            zh_fptStoreSMTItem( pArea, zh_arrayGetItemPtr( pItem, u ),
                                bBufPtr, iTrans );
         }
         break;

      case ZH_IT_STRING:
      case ZH_IT_MEMO:
         *( *bBufPtr )++ = SMT_IT_CHAR;
         if( iTrans == FPT_TRANS_UNICODE )
         {
            ulLen = ( ZH_ULONGCAST ) zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE, NULL, 0xFFFF );
            ulLen = ( ZH_ULONGCAST ) zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                                        ( ZH_WCHAR * ) *bBufPtr + 2, ulLen );
            ulLen *= sizeof( ZH_WCHAR );
         }
         else
         {
            ulLen = ( ZH_ULONGCAST ) zh_itemGetCLen( pItem );
            if( ulLen > 0 )
            {
               u = 0xFFFF;
               if( iTrans == FPT_TRANS_CP )
               {
                  ZH_SIZE nSize = u;
                  zh_cdpnDup2( zh_itemGetCPtr( pItem ), ulLen,
                               ( char * ) *bBufPtr + 2, &nSize,
                               zh_vmCodepage(), pArea->area.cdPage );
                  ulLen = ( ZH_ULONG ) nSize;
               }
               else
               {
                  if( ulLen > u )
                     ulLen = u;
                  memcpy( *bBufPtr + 2, zh_itemGetCPtr( pItem ), ulLen );
               }
            }
         }
         ZH_PUT_LE_UINT16( *bBufPtr, ulLen );
         *bBufPtr += ulLen + 2;
         break;

      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
      {
         ZH_MAXINT iVal = zh_itemGetNInt( pItem );
         if( ZH_LIM_INT32( iVal ) )
         {
            *( *bBufPtr )++ = SMT_IT_INT;
            ZH_PUT_LE_UINT32( *bBufPtr, iVal );
            *bBufPtr += 4;
            break;
         }
      }
      /* fallthrough */
      case ZH_IT_DOUBLE:
      {
         double dVal = zh_itemGetND( pItem );
         int iWidth, iDec;
         zh_itemGetNLen( pItem, &iWidth, &iDec );
         if( iDec )
            iWidth += iDec + 1;
         *( *bBufPtr )++ = SMT_IT_DOUBLE;
         *( *bBufPtr )++ = ( ZH_BYTE ) iWidth;
         *( *bBufPtr )++ = ( ZH_BYTE ) iDec;
         ZH_PUT_LE_DOUBLE( *bBufPtr, dVal );
         *bBufPtr += 8;
         break;
      }
      case ZH_IT_DATE:
      case ZH_IT_TIMESTAMP:
      {
         ZH_LONG lVal;
         *( *bBufPtr )++ = SMT_IT_DATE;
         lVal = zh_itemGetDL( pItem );
         ZH_PUT_LE_UINT32( *bBufPtr, lVal );
         *bBufPtr += 4;
         break;
      }
      case ZH_IT_LOGICAL:
         *( *bBufPtr )++ = SMT_IT_LOGICAL;
         *( *bBufPtr )++ = zh_itemGetL( pItem ) ? 1 : 0;
         break;

      case ZH_IT_NIL:
      default:
         *( *bBufPtr )++ = SMT_IT_NIL;
         break;
   }
}

/*
 * Read SMT item from file
 */
static ZH_ERRCODE zh_fptReadRawSMTItem( FPTAREAP pArea, PZH_ITEM pItem, ZH_FOFFSET * pfOffset, int iTrans )
{
   ZH_ULONG ulLen, u;
   ZH_BYTE buffer[ 10 ];
   char * pBuffer;

   if( zh_fileReadAt( pArea->pMemoFile, buffer, 1, *pfOffset ) != 1 )
      return EDBF_READ;

   *pfOffset += 1;
   switch( buffer[ 0 ] )
   {
      case SMT_IT_ARRAY:
         if( zh_fileReadAt( pArea->pMemoFile, buffer, 2, *pfOffset ) != 2 )
            return EDBF_READ;

         *pfOffset += 2;
         ulLen = ZH_GET_LE_UINT16( buffer );
         zh_arrayNew( pItem, ulLen );
         for( u = 1; u <= ulLen; u++ )
         {
            ZH_ERRCODE errCode = zh_fptReadRawSMTItem( pArea,
                           zh_arrayGetItemPtr( pItem, u ), pfOffset, iTrans );
            if( errCode != ZH_SUCCESS )
               return errCode;
         }
         break;

      case SMT_IT_CHAR:
         if( zh_fileReadAt( pArea->pMemoFile, buffer, 2, *pfOffset ) != 2 )
            return EDBF_READ;
         *pfOffset += 2;
         ulLen = ZH_GET_LE_UINT16( buffer );
         pBuffer = ( char * ) zh_xgrab( ulLen + 1 );
         if( ulLen > 0 && zh_fileReadAt( pArea->pMemoFile, pBuffer, ulLen, *pfOffset ) != ulLen )
         {
            zh_xfree( pBuffer );
            return EDBF_READ;
         }
         *pfOffset += ulLen;
         if( iTrans == FPT_TRANS_UNICODE )
         {
            zh_itemPutStrLenU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                 ( const ZH_WCHAR * ) pBuffer, ulLen >> 1 );
            zh_xfree( pBuffer );
         }
         else
         {
            if( iTrans == FPT_TRANS_CP && ulLen > 0 )
            {
               ZH_SIZE nSize = ulLen + 1;
               ZH_SIZE nLen = ulLen;
               zh_cdpnDup3( pBuffer, ulLen, pBuffer, &nLen, &pBuffer, &nSize,
                            pArea->area.cdPage, zh_vmCodepage() );
               ulLen = ( ZH_ULONG ) nLen;
            }
            zh_itemPutCLPtr( pItem, pBuffer, ulLen );
         }
         break;

      case SMT_IT_INT:
         if( zh_fileReadAt( pArea->pMemoFile, buffer, 4, *pfOffset ) != 4 )
            return EDBF_READ;
         *pfOffset += 4;
         zh_itemPutNInt( pItem, ( ZH_LONG ) ZH_GET_LE_UINT32( buffer ) );
         break;

      case SMT_IT_DOUBLE:
      {
         int iWidth, iDec;
         if( zh_fileReadAt( pArea->pMemoFile, buffer, 10, *pfOffset ) != 10 )
            return EDBF_READ;
         *pfOffset += 10;
         iWidth = buffer[ 0 ];
         iDec = buffer[ 1 ];
         if( iDec )
            iWidth -= iDec + 1;
         zh_itemPutNDLen( pItem, ZH_GET_LE_DOUBLE( &buffer[ 2 ] ), iWidth, iDec );
         break;
      }
      case SMT_IT_DATE:
         if( zh_fileReadAt( pArea->pMemoFile, buffer, 4, *pfOffset ) != 4 )
            return EDBF_READ;
         *pfOffset += 4;
         zh_itemPutDL( pItem, ( long ) ZH_GET_LE_UINT32( buffer ) );
         break;

      case SMT_IT_LOGICAL:
         if( zh_fileReadAt( pArea->pMemoFile, buffer, 1, *pfOffset ) != 1 )
            return EDBF_READ;
         *pfOffset += 1;
         zh_itemPutL( pItem, buffer[ 0 ] != 0 );
         break;

      case SMT_IT_NIL:
         zh_itemClear( pItem );
         break;

      default:
         zh_itemClear( pItem );
         return EDBF_CORRUPT;
   }

   return ZH_SUCCESS;
}

/*
 * Read SMT item from memory buffer.
 */
static ZH_ERRCODE zh_fptReadSMTItem( FPTAREAP pArea, ZH_BYTE ** pbMemoBuf, ZH_BYTE * bBufEnd, PZH_ITEM pItem, int iTrans )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   if( bBufEnd - ( *pbMemoBuf ) >= 1 )
   {
      ZH_ULONG ulLen, u;

      switch( *( *pbMemoBuf )++ )
      {
         case SMT_IT_ARRAY:
            if( bBufEnd - ( *pbMemoBuf ) < 2 )
            {
               errCode = EDBF_CORRUPT;
               break;
            }
            ulLen = ZH_GET_LE_UINT16( *pbMemoBuf );
            *pbMemoBuf += 2;
            if( bBufEnd - ( *pbMemoBuf ) < ( ZH_LONG ) ulLen )
            {
               errCode = EDBF_CORRUPT;
               break;
            }
            zh_arrayNew( pItem, ulLen );
            for( u = 1; u <= ulLen; u++ )
            {
               errCode = zh_fptReadSMTItem( pArea, pbMemoBuf, bBufEnd,
                                            zh_arrayGetItemPtr( pItem, u ), iTrans );
               if( errCode != ZH_SUCCESS )
                  break;
            }
            break;

         case SMT_IT_CHAR:
            if( bBufEnd - ( *pbMemoBuf ) < 2 )
            {
               errCode = EDBF_CORRUPT;
               break;
            }
            ulLen = ZH_GET_LE_UINT16( *pbMemoBuf );
            *pbMemoBuf += 2;
            if( bBufEnd - ( *pbMemoBuf ) < ( ZH_LONG ) ulLen )
            {
               errCode = EDBF_CORRUPT;
            }
            else
            {
               char * pszStr = ( char * ) ( *pbMemoBuf );
               *pbMemoBuf += ulLen;
               if( iTrans == FPT_TRANS_UNICODE )
               {
                  zh_itemPutStrLenU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                       ( const ZH_WCHAR * ) pszStr, ulLen >> 1 );
               }
               else if( iTrans == FPT_TRANS_CP && ulLen != 0 )
               {
                  ZH_SIZE nLen = ulLen;
                  pszStr = zh_cdpnDup( pszStr, &nLen, pArea->area.cdPage, zh_vmCodepage() );
                  ulLen = ( ZH_ULONG ) nLen;
                  zh_itemPutCLPtr( pItem, pszStr, ulLen );
               }
               else
                  zh_itemPutCL( pItem, pszStr, ulLen );
            }
            break;

         case SMT_IT_INT:
            if( bBufEnd - ( *pbMemoBuf ) < 4 )
            {
               errCode = EDBF_CORRUPT;
               break;
            }
            zh_itemPutNInt( pItem, ( ZH_LONG ) ZH_GET_LE_UINT32( *pbMemoBuf ) );
            *pbMemoBuf += 4;
            break;

         case SMT_IT_DOUBLE:
         {
            int iWidth, iDec;
            if( bBufEnd - ( *pbMemoBuf ) < 10 )
            {
               errCode = EDBF_CORRUPT;
               break;
            }
            iWidth = *( *pbMemoBuf )++;
            iDec   = *( *pbMemoBuf )++;
            if( iDec )
               iWidth -= iDec + 1;
            zh_itemPutNDLen( pItem, ZH_GET_LE_DOUBLE( *pbMemoBuf ), iWidth, iDec );
            *pbMemoBuf += 8;
            break;
         }
         case SMT_IT_DATE:
            if( bBufEnd - ( *pbMemoBuf ) < 4 )
            {
               errCode = EDBF_CORRUPT;
               break;
            }
            zh_itemPutDL( pItem, ( long ) ZH_GET_LE_UINT32( *pbMemoBuf ) );
            *pbMemoBuf += 4;
            break;

         case SMT_IT_LOGICAL:
            if( bBufEnd - ( *pbMemoBuf ) < 1 )
            {
               errCode = EDBF_CORRUPT;
               break;
            }
            zh_itemPutL( pItem, *( *pbMemoBuf )++ != 0 );
            break;

         case SMT_IT_NIL:
            zh_itemClear( pItem );
            break;

         default:
            zh_itemClear( pItem );
            errCode = EDBF_CORRUPT;
            break;
      }
   }
   else
      errCode = EDBF_CORRUPT;

   return errCode;
}

/*
 * Calculate the size of SIX memo item
 */
static ZH_ULONG zh_fptCountSixItemLength( FPTAREAP pArea, PZH_ITEM pItem,
                                          ZH_ULONG * pulArrayCount, int iTrans )
{
   ZH_ULONG ulLen, u, ulSize;

   switch( zh_itemType( pItem ) )
   {
      case ZH_IT_ARRAY: /* ZH_IT_OBJECT == ZH_IT_ARRAY */
         ( *pulArrayCount )++;
         ulSize = SIX_ITEM_BUFSIZE;
         ulLen = ( ZH_ULONGCAST ) zh_arrayLen( pItem );
         if( pArea->uiMemoVersion == DB_MEMOVER_SIX )
         {
            /* only 2 bytes (ZH_SHORT) for SIX compatibility */
            ulLen = ZH_MIN( ulLen, 0xFFFF );
         }
         for( u = 1; u <= ulLen; u++ )
         {
            ulSize += zh_fptCountSixItemLength( pArea, zh_arrayGetItemPtr( pItem, u ), pulArrayCount, iTrans );
         }
         break;
      case ZH_IT_MEMO:
      case ZH_IT_STRING:
         ulSize = SIX_ITEM_BUFSIZE;
         /* only 2 bytes (ZH_SHORT) for SIX compatibility */
         u = pArea->uiMemoVersion == DB_MEMOVER_SIX ? 0xFFFF : ULONG_MAX;
         if( iTrans == FPT_TRANS_UNICODE )
         {
            ulLen = ( ZH_ULONGCAST ) zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE, NULL, u ) * sizeof( ZH_WCHAR );
         }
         else
         {
            ulLen = ( ZH_ULONGCAST ) zh_itemGetCLen( pItem );
            if( iTrans == FPT_TRANS_CP && ulLen > 0 )
            {
               ulLen = ( ZH_ULONGCAST ) zh_cdpnDup2Len( zh_itemGetCPtr( pItem ), ulLen, u,
                                                        zh_vmCodepage(), pArea->area.cdPage );
            }
            else
            {
               if( ulLen > u )
                  ulLen = u;
            }
         }
         ulSize += ulLen;
         break;
      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
      case ZH_IT_DOUBLE:
      case ZH_IT_DATE:
      case ZH_IT_TIMESTAMP:
      case ZH_IT_LOGICAL:
      default:
         ulSize = SIX_ITEM_BUFSIZE;
   }
   return ulSize;
}

/*
 * Write fpt vartype as SIX memos.
 */
static ZH_ULONG zh_fptStoreSixItem( FPTAREAP pArea, PZH_ITEM pItem, ZH_BYTE ** bBufPtr, int iTrans )
{
   ZH_ULONG ulLen, u, ulSize;
   int iWidth, iDec;

   memset( *bBufPtr, '\0', SIX_ITEM_BUFSIZE );
   ulSize = SIX_ITEM_BUFSIZE;
   switch( zh_itemType( pItem ) )
   {
      case ZH_IT_ARRAY: /* ZH_IT_OBJECT == ZH_IT_ARRAY */
         ZH_PUT_LE_UINT16( &( *bBufPtr )[ 0 ], FPTIT_SIX_ARRAY );
         ulLen = ( ZH_ULONGCAST ) zh_arrayLen( pItem );
         if( pArea->uiMemoVersion == DB_MEMOVER_SIX )
         {
            /* only 2 bytes (ZH_SHORT) for SIX compatibility */
            ulLen = ZH_MIN( ulLen, 0xFFFF );
         }
         ZH_PUT_LE_UINT32( &( *bBufPtr )[ 2 ], ulLen );
         *bBufPtr += SIX_ITEM_BUFSIZE;
         for( u = 1; u <= ulLen; u++ )
         {
            ulSize += zh_fptStoreSixItem( pArea, zh_arrayGetItemPtr( pItem, u ), bBufPtr, iTrans );
         }
         break;

      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
      {
         ZH_MAXINT iVal = zh_itemGetNInt( pItem );
         zh_itemGetNLen( pItem, &iWidth, &iDec );
         if( ZH_LIM_INT32( iVal ) )
         {
            ZH_PUT_LE_UINT16( &( *bBufPtr )[ 0 ], FPTIT_SIX_LNUM );
            ZH_PUT_LE_UINT16( &( *bBufPtr )[ 2 ], iWidth );
            ZH_PUT_LE_UINT16( &( *bBufPtr )[ 4 ], iDec );
            ZH_PUT_LE_UINT32( &( *bBufPtr )[ 6 ], iVal );
            *bBufPtr += SIX_ITEM_BUFSIZE;
         }
         else
         {
            ZH_PUT_LE_UINT16( &( *bBufPtr )[ 0 ], FPTIT_SIX_DNUM );
            ZH_PUT_LE_UINT16( &( *bBufPtr )[ 2 ], iWidth );
            ZH_PUT_LE_UINT16( &( *bBufPtr )[ 4 ], iDec );
            ZH_PUT_LE_DOUBLE( &( *bBufPtr )[ 6 ], ( double ) iVal );
            *bBufPtr += SIX_ITEM_BUFSIZE;
         }
         break;
      }
      case ZH_IT_DOUBLE:
      {
         double dVal = zh_itemGetND( pItem );
         zh_itemGetNLen( pItem, &iWidth, &iDec );
         ZH_PUT_LE_UINT16( &( *bBufPtr )[ 0 ], FPTIT_SIX_DNUM );
         ZH_PUT_LE_UINT16( &( *bBufPtr )[ 2 ], iWidth );
         ZH_PUT_LE_UINT16( &( *bBufPtr )[ 4 ], iDec );
         ZH_PUT_LE_DOUBLE( &( *bBufPtr )[ 6 ], dVal );
         *bBufPtr += SIX_ITEM_BUFSIZE;
         break;
      }
      case ZH_IT_DATE:
      case ZH_IT_TIMESTAMP:
      {
         ZH_LONG lVal = zh_itemGetDL( pItem );
         ZH_PUT_LE_UINT16( &( *bBufPtr )[ 0 ], FPTIT_SIX_LDATE );
         ZH_PUT_LE_UINT32( &( *bBufPtr )[ 6 ], lVal );
         *bBufPtr += SIX_ITEM_BUFSIZE;
         break;
      }
      case ZH_IT_LOGICAL:
         ZH_PUT_LE_UINT16( &( *bBufPtr )[ 0 ], FPTIT_SIX_LOG );
         ( *bBufPtr )[ 6 ] = zh_itemGetL( pItem ) ? 1 : 0;
         *bBufPtr += SIX_ITEM_BUFSIZE;
         break;

      case ZH_IT_STRING:
      case ZH_IT_MEMO:
         ZH_PUT_LE_UINT16( &( *bBufPtr )[ 0 ], FPTIT_SIX_CHAR );
         /* only 2 bytes (ZH_SHORT) for SIX compatibility */
         u = pArea->uiMemoVersion == DB_MEMOVER_SIX ? 0xFFFF : ULONG_MAX;
         if( iTrans == FPT_TRANS_UNICODE )
         {
            ulLen = ( ZH_ULONGCAST ) zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE, NULL, u );
            ulLen = ( ZH_ULONGCAST ) zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                                        ( ZH_WCHAR * ) *bBufPtr + SIX_ITEM_BUFSIZE, ulLen );
            ulLen *= sizeof( ZH_WCHAR );
         }
         else
         {
            ulLen = ( ZH_ULONGCAST ) zh_itemGetCLen( pItem );
            if( ulLen > 0 )
            {
               if( iTrans == FPT_TRANS_CP )
               {
                  ZH_SIZE nSize = u;
                  zh_cdpnDup2( zh_itemGetCPtr( pItem ), ulLen,
                               ( char * ) *bBufPtr + SIX_ITEM_BUFSIZE, &nSize,
                               zh_vmCodepage(), pArea->area.cdPage );
                  ulLen = ( ZH_ULONG ) nSize;
               }
               else
               {
                  if( ulLen > u )
                     ulLen = u;
                  memcpy( *bBufPtr + SIX_ITEM_BUFSIZE, zh_itemGetCPtr( pItem ), ulLen );
               }
            }
         }
         ZH_PUT_LE_UINT32( &( *bBufPtr )[ 2 ], ulLen );
         *bBufPtr += ulLen + SIX_ITEM_BUFSIZE;
         break;
      default:
         ZH_PUT_LE_UINT16( &( *bBufPtr )[ 0 ], FPTIT_SIX_NIL );
         *bBufPtr += SIX_ITEM_BUFSIZE;
         break;
   }
   return ulSize;
}

/*
 * Read SIX item from memo.
 */
static ZH_ERRCODE zh_fptReadSixItem( FPTAREAP pArea, ZH_BYTE ** pbMemoBuf, ZH_BYTE * bBufEnd, PZH_ITEM pItem, int iTrans )
{
   ZH_ULONG ulLen, u;
   ZH_ERRCODE errCode = ZH_SUCCESS;

   ulLen = SIX_ITEM_BUFSIZE;
   if( bBufEnd - ( *pbMemoBuf ) >= ( ZH_LONG ) ulLen )
   {
      ZH_USHORT usType = ZH_GET_LE_UINT16( &( *pbMemoBuf )[ 0 ] );
      switch( usType )
      {
         case FPTIT_SIX_LNUM:
            zh_itemPutNL( pItem, ( long ) ZH_GET_LE_UINT32( &( *pbMemoBuf )[ 6 ] ) );
            break;

         case FPTIT_SIX_DNUM:
            zh_itemPutNDLen( pItem, ZH_GET_LE_DOUBLE( &( *pbMemoBuf )[ 6 ] ),
                             ZH_GET_LE_UINT16( &( *pbMemoBuf )[ 2 ] ),
                             ZH_GET_LE_UINT16( &( *pbMemoBuf )[ 4 ] ) );
            break;

         case FPTIT_SIX_LDATE:
            zh_itemPutDL( pItem, ( long ) ZH_GET_LE_UINT32( &( *pbMemoBuf )[ 6 ] ) );
            break;

         case FPTIT_SIX_LOG:
            zh_itemPutL( pItem, ZH_GET_LE_UINT16( &( *pbMemoBuf )[ 6 ] ) != 0 );
            break;

         case FPTIT_SIX_CHAR:
            ulLen = ZH_GET_LE_UINT32( &( *pbMemoBuf )[ 2 ] );
            if( pArea->uiMemoVersion == DB_MEMOVER_SIX )
            {
               ulLen &= 0xFFFF; /* only 2 bytes (ZH_SHORT) for SIX compatibility */
            }
            ( *pbMemoBuf ) += SIX_ITEM_BUFSIZE;
            if( bBufEnd - ( *pbMemoBuf ) >= ( ZH_LONG ) ulLen )
            {
               char * pszStr = ( char * ) ( *pbMemoBuf );

               if( iTrans == FPT_TRANS_UNICODE )
               {
                  zh_itemPutStrLenU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                       ( const ZH_WCHAR * ) pszStr, ulLen >> 1 );
               }
               else
               {
                  if( iTrans == FPT_TRANS_CP && ulLen > 0 )
                  {
                     ZH_SIZE nSize = ulLen;
                     pszStr = zh_cdpnDup( pszStr, &nSize, pArea->area.cdPage, zh_vmCodepage() );
                     zh_itemPutCLPtr( pItem, pszStr, nSize );
                  }
                  else
                     zh_itemPutCL( pItem, pszStr, ulLen );
               }
            }
            else
               errCode = EDBF_CORRUPT;

            break;
#if 0
         case FPTIT_SIX_BLOCK:
         case FPTIT_SIX_VREF:
         case FPTIT_SIX_MREF:
#endif
         case FPTIT_SIX_ARRAY:
            ulLen = ZH_GET_LE_UINT32( &( *pbMemoBuf )[ 2 ] );
            if( pArea->uiMemoVersion == DB_MEMOVER_SIX )
            {
               ulLen &= 0xFFFF;   /* only 2 bytes (ZH_SHORT) for SIX compatibility */
            }
            ( *pbMemoBuf ) += SIX_ITEM_BUFSIZE;
            zh_arrayNew( pItem, ulLen );
            for( u = 1; u <= ulLen; u++ )
            {
               errCode = zh_fptReadSixItem( pArea, pbMemoBuf, bBufEnd,
                                            zh_arrayGetItemPtr( pItem, u ), iTrans );
               if( errCode != ZH_SUCCESS )
                  break;
            }
            ulLen = 0;
            break;

         case FPTIT_SIX_NIL:
            zh_itemClear( pItem );
            break;

         default:
            errCode = EDBF_CORRUPT;
            zh_itemClear( pItem );
            break;
      }
      *pbMemoBuf += ulLen;
   }
   else
      errCode = EDBF_CORRUPT;

   return errCode;
}

/*
 * Calculate the size of FLEX memo item
 */
static ZH_ULONG zh_fptCountFlexItemLength( FPTAREAP pArea, PZH_ITEM pItem,
                                           ZH_ULONG * pulArrayCount, int iTrans )
{
   ZH_ULONG ulLen, u, ulSize = 1;
   ZH_MAXINT iVal;

   switch( zh_itemType( pItem ) )
   {
      case ZH_IT_ARRAY:
         ( *pulArrayCount )++;
         ulSize += 2;
         ulLen = zh_arrayLen( pItem ) & 0xFFFF;
         for( u = 1; u <= ulLen; u++ )
         {
            ulSize += zh_fptCountFlexItemLength( pArea, zh_arrayGetItemPtr( pItem, u ), pulArrayCount, iTrans );
         }
         break;
      case ZH_IT_MEMO:
      case ZH_IT_STRING:
         if( iTrans == FPT_TRANS_UNICODE )
         {
            ulLen = ( ZH_ULONGCAST ) zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE, NULL, 0xFFFF ) * sizeof( ZH_WCHAR );
         }
         else
         {
            ulLen = ( ZH_ULONGCAST ) zh_itemGetCLen( pItem );
            if( iTrans == FPT_TRANS_CP && ulLen > 0 )
            {
               ulLen = ( ZH_ULONGCAST ) zh_cdpnDup2Len( zh_itemGetCPtr( pItem ), ulLen, 0xFFFF,
                                                        zh_vmCodepage(), pArea->area.cdPage );
            }
            else
            {
               if( ulLen > 0xFFFF )
                  ulLen = 0xFFFF;
            }
         }
         if( ulLen > 0 )
            ulSize += ulLen + 2;
         break;
      case ZH_IT_DATE:
      case ZH_IT_TIMESTAMP:
         ulSize += 4;
         break;
      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
         iVal = zh_itemGetNInt( pItem );
         ulSize += ( ZH_LIM_INT8( iVal ) ? 2 :
                   ( ZH_LIM_INT16( iVal ) ? 3 :
                   ( ZH_LIM_INT32( iVal ) ? 5 : 10 ) ) );
         break;
      case ZH_IT_DOUBLE:
         ulSize += 10;
         break;
   }
   return ulSize;
}

/*
 * Store in buffer fpt vartype as FLEX memos.
 */
static void zh_fptStoreFlexItem( FPTAREAP pArea, PZH_ITEM pItem, ZH_BYTE ** bBufPtr, int iTrans )
{
   ZH_ULONG ulLen, u;
   int iWidth, iDec;

   switch( zh_itemType( pItem ) )
   {
      case ZH_IT_ARRAY:
         ulLen = zh_arrayLen( pItem ) & 0xFFFF;
         *( *bBufPtr )++ = FPTIT_FLEXAR_ARAY;
         ZH_PUT_LE_UINT16( *bBufPtr, ( ZH_USHORT ) ulLen );
         *bBufPtr += 2;
         for( u = 1; u <= ulLen; u++ )
         {
            zh_fptStoreFlexItem( pArea, zh_arrayGetItemPtr( pItem, u ), bBufPtr, iTrans );
         }
         break;
      case ZH_IT_MEMO:
      case ZH_IT_STRING:
         ulLen = ( ZH_ULONGCAST ) zh_itemGetCLen( pItem );
         if( ulLen == 0 )
         {
            *( *bBufPtr )++ = FPTIT_FLEXAR_NUL;
         }
         else
         {
            *( *bBufPtr )++ = FPTIT_FLEXAR_STR;
            u = 0xFFFF;
            if( iTrans == FPT_TRANS_UNICODE )
            {
               ulLen = ( ZH_ULONGCAST ) zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE, NULL, u );
               ulLen = ( ZH_ULONGCAST ) zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                                           ( ZH_WCHAR * ) *bBufPtr + 2, ulLen );
               ulLen *= sizeof( ZH_WCHAR );
            }
            else if( iTrans == FPT_TRANS_CP )
            {
               ZH_SIZE nSize = u;
               zh_cdpnDup2( zh_itemGetCPtr( pItem ), ulLen,
                            ( char * ) *bBufPtr + 2, &nSize,
                            zh_vmCodepage(), pArea->area.cdPage );
               ulLen = ( ZH_ULONG ) nSize;
            }
            else
            {
               if( ulLen > u )
                  ulLen = u;
               memcpy( *bBufPtr + 2, zh_itemGetCPtr( pItem ), ulLen );
            }
            ZH_PUT_LE_UINT16( *bBufPtr, ( ZH_USHORT ) ulLen );
            *bBufPtr += ulLen + 2;
         }
         break;
      case ZH_IT_DATE:
      case ZH_IT_TIMESTAMP:
      {
         ZH_LONG lVal;
         *( *bBufPtr )++ = FPTIT_FLEXAR_DATEJ;
         lVal = zh_itemGetDL( pItem );
         ZH_PUT_LE_UINT32( *bBufPtr, lVal );
         *bBufPtr += 4;
         break;
      }
      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
      {
         ZH_MAXINT iVal = zh_itemGetNInt( pItem );
         zh_itemGetNLen( pItem, &iWidth, &iDec );
         if( ZH_LIM_INT8( iVal ) )
         {
            *( *bBufPtr )++ = FPTIT_FLEXAR_CHAR1;
            *( *bBufPtr )++ = ( ZH_BYTE ) iVal;
            *( *bBufPtr )++ = ( ZH_BYTE ) iWidth;
         }
         else if( ZH_LIM_INT16( iVal ) )
         {
            *( *bBufPtr )++ = FPTIT_FLEXAR_SHORT1;
            ZH_PUT_LE_UINT16( *bBufPtr, iVal );
            *bBufPtr += 2;
            *( *bBufPtr )++ = ( ZH_BYTE ) iWidth;
         }
         else if( ZH_LIM_INT32( iVal ) )
         {
            *( *bBufPtr )++ = FPTIT_FLEXAR_LONG1;
            ZH_PUT_LE_UINT32( *bBufPtr, iVal );
            *bBufPtr += 4;
            *( *bBufPtr )++ = ( ZH_BYTE ) iWidth;
         }
         else
         {
            *( *bBufPtr )++ = FPTIT_FLEXAR_DOUBLE2;
            *( *bBufPtr )++ = ( ZH_BYTE ) iWidth;
            *( *bBufPtr )++ = ( ZH_BYTE ) iDec;
            ZH_PUT_LE_DOUBLE( *bBufPtr, ( double ) iVal );
            *bBufPtr += 8;
         }
         break;
      }
      case ZH_IT_DOUBLE:
      {
         double dVal = zh_itemGetND( pItem );
         zh_itemGetNLen( pItem, &iWidth, &iDec );
         if( iDec )
            iWidth += iDec + 1;
         *( *bBufPtr )++ = FPTIT_FLEXAR_DOUBLE2;
         *( *bBufPtr )++ = ( ZH_BYTE ) iWidth;
         *( *bBufPtr )++ = ( ZH_BYTE ) iDec;
         ZH_PUT_LE_DOUBLE( *bBufPtr, dVal );
         *bBufPtr += 8;
         break;
      }
      case ZH_IT_LOGICAL:
         *( *bBufPtr )++ = zh_itemGetL( pItem ) ?
                           FPTIT_FLEXAR_TRUE : FPTIT_FLEXAR_FALSE;
         break;
      case ZH_IT_NIL:
      default:
         *( *bBufPtr )++ = FPTIT_FLEXAR_NIL;
   }
}

/*
 * Read FLEX item from memo.
 */
static ZH_ERRCODE zh_fptReadFlexItem( FPTAREAP pArea, ZH_BYTE ** pbMemoBuf, ZH_BYTE * bBufEnd, PZH_ITEM pItem, ZH_BOOL bRoot, int iTrans )
{
   ZH_BYTE usType;
   ZH_ULONG ulLen, i;
   ZH_ERRCODE errCode = ZH_SUCCESS;

   if( bRoot )
      usType = FPTIT_FLEXAR_ARAY;
   else if( bBufEnd - ( *pbMemoBuf ) > 0 )
      usType = *( *pbMemoBuf )++;
   else
      return EDBF_CORRUPT;

   switch( usType )
   {
      case FPTIT_FLEXAR_NIL:
         zh_itemClear( pItem );
         break;
      case FPTIT_FLEXAR_TRUE:
         zh_itemPutL( pItem, ZH_TRUE );
         break;
      case FPTIT_FLEXAR_FALSE:
         zh_itemPutL( pItem, ZH_FALSE );
         break;
      case FPTIT_FLEXAR_LOGIC:
         if( bBufEnd - ( *pbMemoBuf ) >= 1 )
            zh_itemPutL( pItem, *( *pbMemoBuf )++ != 0 );
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_DATEJ:
      case FPTIT_FLEXAR_DATEX:
         if( bBufEnd - ( *pbMemoBuf ) >= 4 )
         {
            zh_itemPutDL( pItem, ( long ) ZH_GET_LE_UINT32( *pbMemoBuf ) );
            *pbMemoBuf += 4;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_CHAR:
         if( bBufEnd - ( *pbMemoBuf ) >= 1 )
            zh_itemPutNI( pItem, ( signed char ) *( *pbMemoBuf )++ );
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_CHAR1:
         if( bBufEnd - ( *pbMemoBuf ) >= 2 )
         {
            zh_itemPutNILen( pItem, ( signed char ) **pbMemoBuf, ( *pbMemoBuf )[ 1 ] );
            *pbMemoBuf += 2;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_CHAR2:
         if( bBufEnd - ( *pbMemoBuf ) >= 3 )
         {
            int iLen = ( *pbMemoBuf )[ 1 ], iDec = ( *pbMemoBuf )[ 2 ];
            if( iDec )
            {
               iLen -= iDec + 1;
               zh_itemPutNDLen( pItem, ( signed char ) **pbMemoBuf, iLen, iDec );
            }
            else
               zh_itemPutNILen( pItem, ( signed char ) **pbMemoBuf, iLen );
            *pbMemoBuf += 3;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_UCHAR:
         if( bBufEnd - ( *pbMemoBuf ) >= 1 )
            zh_itemPutNI( pItem, ( unsigned char ) *( *pbMemoBuf )++ );
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_UCHAR1:
         if( bBufEnd - ( *pbMemoBuf ) >= 2 )
         {
            zh_itemPutNILen( pItem, ( unsigned char ) **pbMemoBuf, ( *pbMemoBuf )[ 1 ] );
            *pbMemoBuf += 2;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_UCHAR2:
         if( bBufEnd - ( *pbMemoBuf ) >= 3 )
         {
            int iLen = ( *pbMemoBuf )[ 1 ], iDec = ( *pbMemoBuf )[ 2 ];
            if( iDec )
            {
               iLen -= iDec + 1;
               zh_itemPutNDLen( pItem, ( unsigned char ) **pbMemoBuf, iLen, iDec );
            }
            else
               zh_itemPutNILen( pItem, ( unsigned char ) **pbMemoBuf, iLen );
            *pbMemoBuf += 3;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_SHORT:
         if( bBufEnd - ( *pbMemoBuf ) >= 2 )
         {
            zh_itemPutNI( pItem, ( ZH_SHORT ) ZH_GET_LE_UINT16( *pbMemoBuf ) );
            *pbMemoBuf += 2;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_SHORT1:
         if( bBufEnd - ( *pbMemoBuf ) >= 3 )
         {
            zh_itemPutNILen( pItem, ( ZH_SHORT ) ZH_GET_LE_UINT16( *pbMemoBuf ),
                             ( *pbMemoBuf )[ 2 ] );
            *pbMemoBuf += 3;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_SHORT2:
         if( bBufEnd - ( *pbMemoBuf ) >= 4 )
         {
            int iLen = ( *pbMemoBuf )[ 2 ], iDec = ( *pbMemoBuf )[ 3 ];
            if( iDec )
            {
               iLen -= iDec + 1;
               zh_itemPutNDLen( pItem, ( ZH_SHORT ) ZH_GET_LE_UINT16( *pbMemoBuf ), iLen, iDec );
            }
            else
               zh_itemPutNILen( pItem, ( ZH_SHORT ) ZH_GET_LE_UINT16( *pbMemoBuf ), iLen );
            *pbMemoBuf += 4;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_USHORT:
         if( bBufEnd - ( *pbMemoBuf ) >= 2 )
         {
            zh_itemPutNInt( pItem, ( ZH_USHORT ) ZH_GET_LE_UINT16( *pbMemoBuf ) );
            *pbMemoBuf += 2;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_USHORT1:
         if( bBufEnd - ( *pbMemoBuf ) >= 3 )
         {
            zh_itemPutNIntLen( pItem, ( ZH_USHORT ) ZH_GET_LE_UINT16( *pbMemoBuf ),
                               ( *pbMemoBuf )[ 2 ] );
            *pbMemoBuf += 3;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_USHORT2:
         if( bBufEnd - ( *pbMemoBuf ) >= 4 )
         {
            int iLen = ( *pbMemoBuf )[ 2 ], iDec = ( *pbMemoBuf )[ 3 ];
            if( iDec )
            {
               iLen -= iDec + 1;
               zh_itemPutNDLen( pItem, ( ZH_USHORT ) ZH_GET_LE_UINT16( *pbMemoBuf ), iLen, iDec );
            }
            else
               zh_itemPutNIntLen( pItem, ( ZH_USHORT ) ZH_GET_LE_UINT16( *pbMemoBuf ), iLen );
            *pbMemoBuf += 4;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_LONG:
         if( bBufEnd - ( *pbMemoBuf ) >= 4 )
         {
            zh_itemPutNL( pItem, ( long ) ZH_GET_LE_UINT32( *pbMemoBuf ) );
            *pbMemoBuf += 4;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_LONG1:
         if( bBufEnd - ( *pbMemoBuf ) >= 5 )
         {
            zh_itemPutNLLen( pItem, ( ZH_LONG ) ZH_GET_LE_UINT32( *pbMemoBuf ),
                             ( *pbMemoBuf )[ 4 ] );
            *pbMemoBuf += 5;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_LONG2:
         if( bBufEnd - ( *pbMemoBuf ) >= 6 )
         {
            int iLen = ( *pbMemoBuf )[ 4 ], iDec = ( *pbMemoBuf )[ 5 ];
            if( iDec )
            {
               iLen -= iDec + 1;
               zh_itemPutNDLen( pItem, ( ZH_LONG ) ZH_GET_LE_UINT32( *pbMemoBuf ), iLen, iDec );
            }
            else
               zh_itemPutNLLen( pItem, ( ZH_LONG ) ZH_GET_LE_UINT32( *pbMemoBuf ), iLen );
            *pbMemoBuf += 6;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_ULONG2:
         if( bBufEnd - ( *pbMemoBuf ) >= 6 )
         {
            int iLen = ( *pbMemoBuf )[ 4 ], iDec = ( *pbMemoBuf )[ 5 ];
            if( iDec )
            {
               iLen -= iDec + 1;
               zh_itemPutNDLen( pItem, ( ZH_ULONG ) ZH_GET_LE_UINT32( *pbMemoBuf ), iLen, iDec );
            }
            else
               zh_itemPutNIntLen( pItem, ( ZH_ULONG ) ZH_GET_LE_UINT32( *pbMemoBuf ), iLen );
            *pbMemoBuf += 6;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_DOUBLE:
         if( bBufEnd - ( *pbMemoBuf ) >= 8 )
         {
            zh_itemPutND( pItem, ZH_GET_LE_DOUBLE( *pbMemoBuf ) );
            *pbMemoBuf += 8;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_DOUBLE2:
         if( bBufEnd - ( *pbMemoBuf ) >= 10 )
         {
            int iLen = ( *pbMemoBuf )[ 0 ], iDec = ( *pbMemoBuf )[ 1 ];
            if( iDec )
               iLen -= iDec + 1;
            zh_itemPutNDLen( pItem, ZH_GET_LE_DOUBLE( *pbMemoBuf + 2 ), iLen, iDec );
            *pbMemoBuf += 10;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_LDOUBLE:
         if( bBufEnd - ( *pbMemoBuf ) >= 10 )
         {
            /* TODO: write a cross platform converter from
                     10 digit long double to double */
            zh_itemPutND( pItem, 0.0 /* ZH_GET_LE_DOUBLE( *pbMemoBuf ) */ );
            *pbMemoBuf += 10;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      case FPTIT_FLEXAR_NUL:
         zh_itemPutCL( pItem, NULL, 0 );
         break;

      case FPTIT_FLEXAR_STR:
         if( bBufEnd - ( *pbMemoBuf ) >= 2 )
         {
            ulLen = ZH_GET_LE_UINT16( *pbMemoBuf );
            *pbMemoBuf += 2;
            if( bBufEnd - ( *pbMemoBuf ) >= ( ZH_LONG ) ulLen )
            {
               char * pszStr = ( char * ) ( *pbMemoBuf );
               *pbMemoBuf += ulLen;

               if( iTrans == FPT_TRANS_UNICODE )
               {
                  zh_itemPutStrLenU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                       ( const ZH_WCHAR * ) pszStr, ulLen >> 1 );
               }
               else if( iTrans == FPT_TRANS_CP && ulLen != 0 )
               {
                  ZH_SIZE nLen = ulLen;
                  pszStr = zh_cdpnDup( pszStr, &nLen, pArea->area.cdPage, zh_vmCodepage() );
                  zh_itemPutCLPtr( pItem, pszStr, nLen );
               }
               else
                  zh_itemPutCL( pItem, pszStr, ulLen );
            }
            else
               errCode = EDBF_CORRUPT;
         }
         else
            errCode = EDBF_CORRUPT;
         break;

      case FPTIT_FLEXAR_ARAY:
         if( bBufEnd - ( *pbMemoBuf ) >= 2 )
         {
            ulLen = ZH_GET_LE_UINT16( *pbMemoBuf );
            *pbMemoBuf += 2;
            if( bBufEnd - ( *pbMemoBuf ) >= ( ZH_LONG ) ulLen )
            {
               zh_arrayNew( pItem, ulLen );
               for( i = 1; i <= ulLen; i++ )
               {
                  errCode = zh_fptReadFlexItem( pArea, pbMemoBuf, bBufEnd,
                                                zh_arrayGetItemPtr( pItem, i ), ZH_FALSE, iTrans );
                  if( errCode != ZH_SUCCESS )
                  {
                     break;
                  }
               }
            }
            else
               errCode = EDBF_CORRUPT;
         }
         else
            errCode = EDBF_CORRUPT;
         break;
      default:
         #if 0
         fprintf( stderr, "Unknown FLEX array item: 0x%x = %d\n", usType, usType ); fflush( stderr );
         #endif
         errCode = EDBF_CORRUPT;
         zh_itemClear( pItem );
         break;
   }
   return errCode;
}

static ZH_ERRCODE zh_fptCopyToRawFile( PZH_FILE pSrc, ZH_FOFFSET from,
                                       PZH_FILE pDst,
                                       ZH_FOFFSET size )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   if( size )
   {
      ZH_FOFFSET written = 0;
      ZH_SIZE nBufSize;
      ZH_BYTE * pBuffer;

      nBufSize = ( ZH_SIZE ) ZH_MIN( 0x10000, size );
      pBuffer = ( ZH_BYTE * ) zh_xgrab( nBufSize );

      do
      {
         ZH_SIZE nRead = zh_fileReadAt( pSrc, pBuffer, ( ZH_SIZE )
                                        ZH_MIN( ( ZH_FOFFSET ) nBufSize, size - written ),
                                        from + written );
         if( nRead == 0 || nRead == ( ZH_SIZE ) FS_ERROR )
            errCode = EDBF_READ;
         else if( zh_fileWrite( pDst, pBuffer, nRead, -1 ) != nRead )
            errCode = EDBF_WRITE;
         else
            written += nRead;
      }
      while( errCode == ZH_SUCCESS && written < size );

      zh_xfree( pBuffer );
   }

   return errCode;
}

static ZH_ERRCODE zh_fptCopyToFile( PZH_FILE pSrc, ZH_FOFFSET from,
                                    PZH_FILE pDst, ZH_FOFFSET to,
                                    ZH_FOFFSET size )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   if( size )
   {
      ZH_FOFFSET written = 0;
      ZH_SIZE nBufSize;
      ZH_BYTE * pBuffer;

      nBufSize = ( ZH_SIZE ) ZH_MIN( 0x10000, size );
      pBuffer = ( ZH_BYTE * ) zh_xgrab( nBufSize );

      do
      {
         ZH_SIZE nRead = zh_fileReadAt( pSrc, pBuffer, ( ZH_SIZE )
                                        ZH_MIN( ( ZH_FOFFSET ) nBufSize, size - written ),
                                        from + written );
         if( nRead == 0 || nRead == ( ZH_SIZE ) FS_ERROR )
            errCode = EDBF_READ;
         else if( zh_fileWriteAt( pDst, pBuffer, nRead,
                                  to + written ) != nRead )
            errCode = EDBF_WRITE;
         else
            written += nRead;
      }
      while( errCode == ZH_SUCCESS && written < size );

      zh_xfree( pBuffer );
   }

   return errCode;
}

static ZH_ERRCODE zh_fptReadRawBlock( FPTAREAP pArea, ZH_BYTE * bBuffer, PZH_FILE pFile,
                                      ZH_ULONG ulBlock, ZH_ULONG ulSize )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   if( ulBlock == 0 )
      return EDBF_CORRUPT;

   if( pFile != NULL )
   {
      errCode = zh_fptCopyToRawFile( pArea->pMemoFile, FPT_BLOCK_OFFSET( ulBlock ),
                                     pFile, ulSize );
   }
   else
   {
      if( zh_fileReadAt( pArea->pMemoFile, bBuffer, ulSize,
                         FPT_BLOCK_OFFSET( ulBlock ) ) != ulSize )
         errCode = EDBF_READ;
   }

   return errCode;
}

static ZH_ERRCODE zh_fptReadBlobBlock( FPTAREAP pArea, PZH_ITEM pItem,
                                       PZH_FILE pFile, ZH_ULONG ulBlock,
                                       ZH_USHORT uiMode )
{
   ZH_ULONG ulSize;
   ZH_BYTE buffer[ 4 ];

   if( ulBlock == 0 )
      return EDBF_CORRUPT;

   /* TODO: uiMode => BLOB_IMPORT_COMPRESS, BLOB_IMPORT_ENCRYPT */
   ZH_SYMBOL_UNUSED( uiMode );

   if( zh_fileReadAt( pArea->pMemoFile, buffer, 4, FPT_BLOCK_OFFSET( ulBlock ) ) != 4 )
      return EDBF_READ;

   ulSize = ZH_GET_LE_UINT32( buffer );
   if( pFile != NULL )
      return zh_fptCopyToRawFile( pArea->pMemoFile, FPT_BLOCK_OFFSET( ulBlock ) + 4,
                                  pFile, ulSize );

   if( ulSize == 0 )
      zh_itemPutC( pItem, NULL );
   else
   {
      ZH_BYTE * bBuffer = ( ZH_BYTE * ) zh_xalloc( ulSize + 1 );

      if( ! bBuffer )
      {
         /* in most cases this means that file is corrupted */
         return EDBF_CORRUPT;
      }
      if( zh_fileReadAt( pArea->pMemoFile, bBuffer, ulSize,
                         FPT_BLOCK_OFFSET( ulBlock ) + 4 ) != ulSize )
      {
         zh_xfree( bBuffer );
         return EDBF_READ;
      }
      zh_itemPutCLPtr( pItem, ( char * ) bBuffer, ulSize );
   }
   return ZH_SUCCESS;
}

static ZH_ERRCODE zh_fptReadSMTBlock( FPTAREAP pArea, PZH_ITEM pItem,
                                      ZH_ULONG ulBlock, ZH_ULONG ulSize, int iTrans )
{
   if( ulBlock == 0 )
      return EDBF_CORRUPT;

   if( ulSize == 0 )
   {
      ZH_FOFFSET fOffset = FPT_BLOCK_OFFSET( ulBlock );
      return zh_fptReadRawSMTItem( pArea, pItem, &fOffset, iTrans );
   }
   else
   {
      ZH_ERRCODE errCode;
      ZH_BYTE * bBuffer = ( ZH_BYTE * ) zh_xalloc( ulSize ), * bMemoBuf;

      if( ! bBuffer )
      {
         /* in most cases this means that file is corrupted */
         return EDBF_CORRUPT;
      }

      if( zh_fileReadAt( pArea->pMemoFile, bBuffer, ulSize,
                         FPT_BLOCK_OFFSET( ulBlock ) ) != ulSize )
      {
         errCode = EDBF_READ;
      }
      else
      {
         bMemoBuf = bBuffer;
         errCode = zh_fptReadSMTItem( pArea, &bMemoBuf, bMemoBuf + ulSize, pItem, iTrans );
      }
      zh_xfree( bBuffer );
      return errCode;
   }
}

/*
 * Read fpt vartype memos.
 */
static ZH_ERRCODE zh_fptGetMemo( FPTAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem,
                                 PZH_FILE pFile, ZH_ULONG ulBlock, ZH_ULONG ulStart,
                                 ZH_ULONG ulCount, int iTrans )
{
   ZH_ERRCODE errCode;
   ZH_ULONG ulSize = 0, ulType = 0;
   char * pBuffer;
   ZH_BYTE * bMemoBuf;
   FPTBLOCK fptBlock;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptGetMemo(%p, %hu, %p, %p, %lu, %lu, %d)", ( void * ) pArea, uiIndex, ( void * ) pItem, ( void * ) pFile, ulStart, ulCount, iTrans ) );

   if( uiIndex )
   {
      errCode = zh_dbfGetMemoData( ( DBFAREAP ) pArea, uiIndex - 1,
                                   &ulBlock, &ulSize, &ulType );
   }
   else if( ! zh_fptHasDirectAccess( pArea ) )
   {
      errCode = EDBF_UNSUPPORTED;
   }
   else
   {
      errCode = ZH_SUCCESS;
   }

   if( errCode != ZH_SUCCESS )
      return errCode;

   if( ulBlock > 0 )
   {
      ZH_FOFFSET fOffset = FPT_BLOCK_OFFSET( ulBlock );
      if( pArea->bMemoType == DB_MEMO_FPT )
      {
         if( zh_fileReadAt( pArea->pMemoFile, &fptBlock,
                            sizeof( FPTBLOCK ), fOffset ) != sizeof( FPTBLOCK ) )
         {
            return EDBF_READ;
         }
         fOffset += sizeof( FPTBLOCK );
         ulType = ZH_GET_BE_UINT32( fptBlock.type );
         ulSize = ZH_GET_BE_UINT32( fptBlock.size );
      }
      else
      {
         if( pArea->bMemoType == DB_MEMO_DBT )
         {
            ulSize = zh_fptGetMemoLen( pArea, uiIndex );
            ulType = FPTIT_BINARY;
         }
      }

      if( ulStart || ulCount )
      {
         if( pArea->bMemoType == DB_MEMO_FPT )
         {
            if( ulType != FPTIT_TEXT && ulType != FPTIT_PICT )
               ulStart = ulCount = 0;
         }
         else if( pArea->bMemoType == DB_MEMO_SMT )
         {
            if( ulType != SMT_IT_CHAR )
               ulStart = ulCount = 0;
         }
      }

      if( ulStart >= ulSize )
         ulSize = 0;
      else
         ulSize -= ulStart;
      if( ulCount && ulCount < ulSize )
         ulSize = ulCount;
      if( ulStart && ulSize )
         fOffset += ulStart;

      if( pFile != NULL )
      {
         return zh_fptCopyToRawFile( pArea->pMemoFile, fOffset, pFile, ulSize );
      }

      if( pArea->bMemoType == DB_MEMO_FPT )
      {
         pBuffer = ( char * ) zh_xalloc( ZH_MAX( ulSize + 1, 8 ) );
         if( pBuffer )
            memset( pBuffer, '\0', 8 );
      }
      else
      {
         pBuffer = ( char * ) zh_xalloc( ulSize + 1 );
      }

      if( ! pBuffer )
      {
         /* in most cases this means that file is corrupted */
         return EDBF_CORRUPT;
      }

      if( ulSize != 0 && zh_fileReadAt( pArea->pMemoFile, pBuffer, ulSize,
                                        fOffset ) != ulSize )
      {
         errCode = EDBF_READ;
      }
      else if( pArea->bMemoType == DB_MEMO_DBT )
      {
         if( iTrans == FPT_TRANS_UNICODE )
         {
            zh_itemPutStrLenU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                 ( const ZH_WCHAR * ) pBuffer, ulSize >> 1 );
            zh_xfree( pBuffer );
         }
         else
         {
            if( iTrans == FPT_TRANS_CP && ulSize != 0 )
            {
               ZH_SIZE nSize = ulSize;
               ZH_SIZE nBufSize = ulSize + 1;
               zh_cdpnDup3( pBuffer, ulSize, pBuffer, &nSize,
                            &pBuffer, &nBufSize,
                            pArea->area.cdPage, zh_vmCodepage() );
               ulSize = ( ZH_ULONG ) nSize;
            }
            zh_itemPutCLPtr( pItem, pBuffer, ulSize );
         }
         zh_itemSetCMemo( pItem );
         pBuffer = NULL;
      }
      else if( pArea->bMemoType == DB_MEMO_SMT )
      {
         if( ulType == SMT_IT_CHAR )
         {
            if( iTrans == FPT_TRANS_UNICODE )
            {
               zh_itemPutStrLenU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                    ( const ZH_WCHAR * ) pBuffer, ulSize >> 1 );
               zh_xfree( pBuffer );
            }
            else
            {
               if( iTrans == FPT_TRANS_CP && ulSize != 0 )
               {
                  ZH_SIZE nSize = ulSize;
                  ZH_SIZE nBufSize = ulSize + 1;
                  zh_cdpnDup3( pBuffer, ulSize, pBuffer, &nSize,
                               &pBuffer, &nBufSize,
                               pArea->area.cdPage, zh_vmCodepage() );
                  ulSize = ( ZH_ULONG ) nSize;
               }
               zh_itemPutCLPtr( pItem, pBuffer, ulSize );
            }
            zh_itemSetCMemo( pItem );
            pBuffer = NULL;
         }
         else if( ! ulSize || pBuffer[ 0 ] != ( char ) ulType )
         {
            errCode = EDBF_CORRUPT;
            zh_itemClear( pItem );
         }
         else
         {
            bMemoBuf = ( ZH_BYTE * ) pBuffer;
            errCode = zh_fptReadSMTItem( pArea, &bMemoBuf, bMemoBuf + ulSize, pItem, iTrans );
         }
      }
      else
      {
         switch( ulType )
         {
            case FPTIT_SIX_LNUM:
            case FPTIT_SIX_DNUM:
            case FPTIT_SIX_LDATE:
            case FPTIT_SIX_LOG:
            case FPTIT_SIX_CHAR:
            case FPTIT_SIX_ARRAY:
#if 0
            case FPTIT_SIX_BLOCK:
            case FPTIT_SIX_VREF:
            case FPTIT_SIX_MREF:
#endif
               bMemoBuf = ( ZH_BYTE * ) pBuffer;
               errCode = zh_fptReadSixItem( pArea, &bMemoBuf, bMemoBuf + ulSize, pItem, iTrans );
               break;
            case FPTIT_FLEX_ARRAY:
               bMemoBuf = ( ZH_BYTE * ) pBuffer;
               errCode = zh_fptReadFlexItem( pArea, &bMemoBuf, bMemoBuf + ulSize, pItem, ZH_TRUE, iTrans );
               break;
            case FPTIT_FLEX_NIL:
               zh_itemClear( pItem );
               break;
            case FPTIT_FLEX_TRUE:
               zh_itemPutL( pItem, ZH_TRUE );
               break;
            case FPTIT_FLEX_FALSE:
               zh_itemPutL( pItem, ZH_FALSE );
               break;
            case FPTIT_FLEX_LDATE:
               zh_itemPutDL( pItem, ( long ) ZH_GET_LE_UINT32( pBuffer ) );
               break;
            case FPTIT_FLEX_CHAR:
               zh_itemPutNI( pItem, ( signed char ) pBuffer[ 0 ] );
               break;
            case FPTIT_FLEX_UCHAR:
               zh_itemPutNI( pItem, ( unsigned char ) pBuffer[ 0 ] );
               break;
            case FPTIT_FLEX_SHORT:
               zh_itemPutNI( pItem, ( short ) ZH_GET_LE_UINT16( pBuffer ) );
               break;
            case FPTIT_FLEX_USHORT:
               zh_itemPutNInt( pItem, ZH_GET_LE_UINT16( pBuffer ) );
               break;
            case FPTIT_FLEX_LONG:
               zh_itemPutNL( pItem, ( long ) ZH_GET_LE_UINT32( pBuffer ) );
               break;
            case FPTIT_FLEX_ULONG:
               zh_itemPutNInt( pItem, ZH_GET_LE_UINT32( pBuffer ) );
               break;
            case FPTIT_FLEX_DOUBLE:
               zh_itemPutND( pItem, ZH_GET_LE_DOUBLE( pBuffer ) );
               break;
            case FPTIT_FLEX_LDOUBLE:
               /* TODO: write a cross platform converter from
                        10 digit long double to double */
               zh_itemPutND( pItem, 0.0 /* ZH_GET_LE_DOUBLE( pBuffer ) */ );
               break;
            case FPTIT_TEXT:
               if( iTrans == FPT_TRANS_UNICODE )
               {
                  zh_itemPutStrLenU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                                       ( const ZH_WCHAR * ) pBuffer, ulSize >> 1 );
                  zh_xfree( pBuffer );
               }
               else
               {
                  if( iTrans == FPT_TRANS_CP && ulSize != 0 )
                  {
                     ZH_SIZE nSize = ulSize;
                     ZH_SIZE nBufSize = ulSize + 1;
                     zh_cdpnDup3( pBuffer, ulSize, pBuffer, &nSize,
                                  &pBuffer, &nBufSize,
                                  pArea->area.cdPage, zh_vmCodepage() );
                     ulSize = ( ZH_ULONG ) nSize;
                  }
                  zh_itemPutCLPtr( pItem, pBuffer, ulSize );
               }
               pBuffer = NULL;
               zh_itemSetCMemo( pItem );
               break;
            case FPTIT_PICT:
               zh_itemPutCLPtr( pItem, pBuffer, ulSize );
               pBuffer = NULL;
               break;
            default:
               zh_itemClear( pItem );
               break;
         }
      }
      if( pBuffer )
         zh_xfree( pBuffer );
   }
   else
   {
      zh_itemPutC( pItem, NULL );
      zh_itemSetCMemo( pItem );
   }
   return errCode;
}

/*
 * Write memo data.
 */
static ZH_ERRCODE zh_fptWriteMemo( FPTAREAP pArea, ZH_ULONG ulBlock, ZH_ULONG ulSize,
                                   const ZH_BYTE * bBufPtr, PZH_FILE pFile,
                                   ZH_ULONG ulType, ZH_ULONG ulLen, ZH_ULONG * pulStoredBlock )
{
   MEMOGCTABLE fptGCtable;
   ZH_ERRCODE errCode;
   ZH_BOOL bWrite;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptWriteMemo(%p, %lu, %lu, %p, %p, %lu, %lu, %p)",
                            ( void * ) pArea, ulBlock, ulSize, ( const void * ) bBufPtr, ( void * ) pFile, ulType, ulLen, ( void * ) pulStoredBlock ) );

   bWrite = ( ulLen != 0 || ( pArea->bMemoType == DB_MEMO_FPT &&
              ulType != FPTIT_TEXT && ulType != FPTIT_BINARY &&
              ulType != FPTIT_DUMMY ) );

   if( ulBlock == 0 && ! bWrite )
   {
      *pulStoredBlock = 0;
      return ZH_SUCCESS;
   }

   zh_fptInitGCdata( &fptGCtable );
   errCode = zh_fptReadGCdata( pArea, &fptGCtable );
   if( errCode != ZH_SUCCESS )
   {
      return errCode;
   }

   if( ulBlock > 0 )
   {
      errCode = zh_fptGCfreeBlock( pArea, &fptGCtable, ulBlock, ulSize,
                                   ulType == FPTIT_DUMMY );
      if( errCode != ZH_SUCCESS )
      {
         zh_fptDestroyGCdata( &fptGCtable );
         return errCode;
      }
   }

   /* Write memo header and data */
   if( bWrite )
   {
      ZH_FOFFSET fOffset;

      errCode = zh_fptGCgetFreeBlock( pArea, &fptGCtable, pulStoredBlock, ulLen,
                                      ulType == FPTIT_DUMMY );
      if( errCode != ZH_SUCCESS )
      {
         zh_fptDestroyGCdata( &fptGCtable );
         return errCode;
      }

      fOffset = FPT_BLOCK_OFFSET( *pulStoredBlock );
      if( pArea->bMemoType == DB_MEMO_FPT && ulType != FPTIT_DUMMY )
      {
         FPTBLOCK fptBlock;
         ZH_PUT_BE_UINT32( fptBlock.type, ulType );
         ZH_PUT_BE_UINT32( fptBlock.size, ulLen );
         if( zh_fileWriteAt( pArea->pMemoFile, &fptBlock,
                             sizeof( FPTBLOCK ), fOffset ) != sizeof( FPTBLOCK ) )
            errCode = EDBF_WRITE;
         else
            fOffset += sizeof( FPTBLOCK );
      }

      if( errCode == ZH_SUCCESS && ulLen > 0 )
      {
         /* TODO: uiMode => BLOB_IMPORT_COMPRESS, BLOB_IMPORT_ENCRYPT */
         if( pFile != NULL )
         {
            ZH_SIZE nWritten = 0, nBufSize = ZH_MIN( ( 1 << 16 ), ulLen );
            ZH_BYTE * bBuffer = ( ZH_BYTE * ) zh_xgrab( nBufSize );

            do
            {
               ZH_SIZE nRead = zh_fileRead( pFile, bBuffer,
                                            ZH_MIN( nBufSize, ulLen - nWritten ), -1 );
               if( nRead == 0 || nRead == ( ZH_SIZE ) FS_ERROR )
                  errCode = EDBF_READ;
               else if( zh_fileWriteAt( pArea->pMemoFile, bBuffer,
                                        nRead, fOffset ) != nRead )
                  errCode = EDBF_WRITE;
               else
               {
                  nWritten += nRead;
                  fOffset += nRead;
               }
            }
            while( errCode == ZH_SUCCESS && nWritten < ulLen );

            zh_xfree( bBuffer );
         }
         else
         {
            if( zh_fileWriteAt( pArea->pMemoFile, bBufPtr,
                                ulLen, fOffset ) != ulLen )
               errCode = EDBF_WRITE;
            else
               fOffset += ulLen;
         }
      }
      /* if written block is smaller then block size we should write at last
         block byte 0xAF to be FLEX compatible */
      if( errCode == ZH_SUCCESS )
      {
         if( pArea->bMemoType == DB_MEMO_DBT )
         {
            zh_fileWriteAt( pArea->pMemoFile, "\x1A\x1A", 2, fOffset );
         }
         else if( pArea->uiMemoVersion == DB_MEMOVER_FLEX &&
                  ( ulLen + sizeof( FPTBLOCK ) ) % pArea->ulMemoBlockSize != 0 )
         {
            ZH_ULONG ulBlocks = ( ulLen + sizeof( FPTBLOCK ) + pArea->ulMemoBlockSize - 1 ) /
                                pArea->ulMemoBlockSize;
            zh_fileWriteAt( pArea->pMemoFile, "\xAF", 1,
                            FPT_BLOCK_OFFSET( *pulStoredBlock + ulBlocks ) - 1 );
         }
      }
      pArea->fMemoFlush = ZH_TRUE;
   }
   else
   {
      *pulStoredBlock = 0;
   }

   if( errCode == ZH_SUCCESS )
   {
      errCode = zh_fptWriteGCdata( pArea, &fptGCtable );
   }
   zh_fptDestroyGCdata( &fptGCtable );

   return errCode;
}

/*
 * Assign a value to the specified memo field.
 */
static ZH_ERRCODE zh_fptPutMemo( FPTAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem,
                                 ZH_ULONG * pulBlock, int iTrans )
{
   ZH_ULONG ulBlock = 0, ulSize, ulType, ulOldSize = 0, ulOldType = 0, ulArrayCount = 0;
   ZH_BYTE itmBuffer[ FLEX_ITEM_BUFSIZE ];
   const ZH_BYTE * bBufPtr = NULL;
   ZH_BYTE * bBufAlloc = NULL, * pbTmp;
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptPutMemo(%p, %hu, %p, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem, ( void * ) pulBlock ) );

   if( ZH_IS_STRING( pItem ) )
   {
      ulType = FPTIT_TEXT;
      if( iTrans == FPT_TRANS_UNICODE )
      {
         ulSize = ( ZH_ULONGCAST ) zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE, NULL, 0 ) * sizeof( ZH_WCHAR );
         if( ulSize > 0 )
         {
            bBufPtr = bBufAlloc = ( ZH_BYTE * ) zh_xgrab( ulSize );
            zh_itemCopyStrU16( pItem, ZH_CODEPAGE_ENDIAN_LITTLE,
                               ( ZH_WCHAR * ) bBufAlloc, ulSize / sizeof( ZH_WCHAR ) );
         }
      }
      else
      {
         ulSize = ( ZH_ULONGCAST ) zh_itemGetCLen( pItem );
         bBufPtr = ( const ZH_BYTE * ) zh_itemGetCPtr( pItem );
         if( iTrans == FPT_TRANS_CP && ulSize > 0 )
         {
            ZH_SIZE nSize = ulSize;
            bBufAlloc = ( ZH_BYTE * ) zh_cdpnDup( ( const char * ) bBufPtr, &nSize,
                                                  zh_vmCodepage(), pArea->area.cdPage );
            bBufPtr = bBufAlloc;
            ulSize = ( ZH_ULONG ) nSize;
         }
      }
      if( pArea->bMemoType == DB_MEMO_SMT )
      {
         ulType = SMT_IT_CHAR;
      }
   }
   else if( pArea->bMemoType == DB_MEMO_DBT )
   {
      return EDBF_DATATYPE;
   }
   else if( pArea->bMemoType == DB_MEMO_SMT )
   {
      ulSize = zh_fptCountSMTItemLength( pArea, pItem, &ulArrayCount, iTrans );
      if( ulSize == 0 )
         return EDBF_DATATYPE;
      pbTmp = bBufAlloc = ( ZH_BYTE * ) zh_xgrab( ulSize );
      zh_fptStoreSMTItem( pArea, pItem, &pbTmp, iTrans );
      ulType = ( ZH_ULONG ) bBufAlloc[ 0 ];
      bBufPtr = bBufAlloc;
   }
   else if( pArea->uiMemoVersion == DB_MEMOVER_SIX )
   {
      if( ZH_IS_NIL( pItem ) )
      {
         ulType = FPTIT_SIX_NIL;
         ulSize = 0;
      }
      else
      {
         ulSize = zh_fptCountSixItemLength( pArea, pItem, &ulArrayCount, iTrans );
         if( ulSize > 0 )
         {
            pbTmp = bBufAlloc = ( ZH_BYTE * ) zh_xgrab( ulSize );
            zh_fptStoreSixItem( pArea, pItem, &pbTmp, iTrans );
            ulType = ( ZH_ULONG ) ZH_GET_LE_UINT16( bBufAlloc );
            bBufPtr = bBufAlloc;
         }
         else
         {
            return EDBF_DATATYPE;
         }
      }
   }
   else if( pArea->uiMemoVersion == DB_MEMOVER_FLEX )
   {
      switch( zh_itemType( pItem ) )
      {
         case ZH_IT_ARRAY:
            ulType = FPTIT_FLEX_ARRAY;
            ulSize = zh_fptCountFlexItemLength( pArea, pItem, &ulArrayCount, iTrans ) - 1;
            if( ulSize > 0 )
            {
               pbTmp = bBufAlloc = ( ZH_BYTE * ) zh_xgrab( ulSize + 1 );
               zh_fptStoreFlexItem( pArea, pItem, &pbTmp, iTrans );
               bBufPtr = bBufAlloc + 1; /* FLEX doesn't store the first byte of array ID */
            }
            break;
         case ZH_IT_NIL:
            ulType = FPTIT_FLEX_NIL;
            ulSize = 0;
            break;
         case ZH_IT_LOGICAL:
            ulType = zh_itemGetL( pItem ) ? FPTIT_FLEX_TRUE : FPTIT_FLEX_FALSE;
            ulSize = 0;
            break;
         case ZH_IT_DATE:
         case ZH_IT_TIMESTAMP:
         {
            ZH_LONG lVal = zh_itemGetDL( pItem );
            ulType = FPTIT_FLEX_LDATE;
            ulSize = 4;
            ZH_PUT_LE_UINT32( itmBuffer, lVal );
            bBufPtr = itmBuffer;
            break;
         }
         case ZH_IT_INTEGER:
         case ZH_IT_LONG:
         {
            ZH_MAXINT iVal = zh_itemGetNInt( pItem );
            if( ZH_LIM_INT8( iVal ) )
            {
               ulType = FPTIT_FLEX_CHAR;
               ulSize = 1;
               *itmBuffer = ( ZH_BYTE ) iVal;
               bBufPtr = itmBuffer;
            }
            else if( ZH_LIM_INT16( iVal ) )
            {
               ulType = FPTIT_FLEX_SHORT;
               ulSize = 2;
               ZH_PUT_LE_UINT16( itmBuffer, iVal );
               bBufPtr = itmBuffer;
            }
            else if( ZH_LIM_INT32( iVal ) )
            {
               ulType = FPTIT_FLEX_LONG;
               ulSize = 4;
               ZH_PUT_LE_UINT32( itmBuffer, iVal );
               bBufPtr = itmBuffer;
            }
            else
            {
               double d = ( double ) iVal;
               ulType = FPTIT_FLEX_DOUBLE;
               ulSize = 8;
               ZH_PUT_LE_DOUBLE( itmBuffer, d );
               bBufPtr = itmBuffer;
            }
            break;
         }
         case ZH_IT_DOUBLE:
         {
            double d = zh_itemGetND( pItem );
            ulType = FPTIT_FLEX_DOUBLE;
            ulSize = 8;
            ZH_PUT_LE_DOUBLE( itmBuffer, d );
            bBufPtr = itmBuffer;
            break;
         }
         default:
            ulType = FPTIT_BINARY;
            ulSize = 0;
            break;
      }
   }
   else
   {
      return EDBF_DATATYPE;
   }

   if( uiIndex )
   {
      errCode = zh_dbfGetMemoData( ( DBFAREAP ) pArea, uiIndex - 1,
                                   &ulBlock, &ulOldSize, &ulOldType );
   }
   else if( ! pulBlock || ! zh_fptHasDirectAccess( pArea ) )
   {
      errCode = EDBF_UNSUPPORTED;
   }
   else
   {
      ulBlock = *pulBlock;
      errCode = ZH_SUCCESS;
   }

   if( errCode == ZH_SUCCESS )
      errCode = zh_fptWriteMemo( pArea, ulBlock, ulOldSize, bBufPtr, NULL,
                                 ulType, ulSize, &ulBlock );

   if( bBufAlloc != NULL )
      zh_xfree( bBufAlloc );

   if( errCode == ZH_SUCCESS )
   {
      if( uiIndex )
         zh_dbfSetMemoData( ( DBFAREAP ) pArea, uiIndex - 1, ulBlock, ulSize, ulType );
      else
         *pulBlock = ulBlock;
   }
   return errCode;
}

#ifdef ZH_MEMO_SAFELOCK
/*
 * Check if memo field has any data
 */
static ZH_BOOL zh_fptHasMemoData( FPTAREAP pArea, ZH_USHORT uiIndex )
{
   if( --uiIndex < pArea->area.uiFieldCount )
   {
      LPFIELD pField = pArea->area.lpFields + uiIndex;

      if( pField->uiType == ZH_FT_ANY )
      {
         if( pField->uiLen >= 6 )
         {
            ZH_BYTE * pFieldBuf = pArea->pRecord + pArea->pFieldOffset[ uiIndex ];
            ZH_USHORT uiType = ZH_GET_LE_UINT16( pFieldBuf + pField->uiLen - 2 );

            switch( uiType )
            {
               case ZH_VF_ARRAY:
               case ZH_VF_BLOB:
               case ZH_VF_BLOBCOMPRESS:
               case ZH_VF_BLOBENCRYPT:
                  return ZH_TRUE;
               case ZH_VF_DNUM:
                  return pField->uiLen <= 12;
               default:
                  return uiType <= ZH_VF_CHAR && pField->uiLen - 2 < uiType;
            }
         }
      }
      else if( pField->uiType == ZH_FT_MEMO ||
               pField->uiType == ZH_FT_IMAGE ||
               pField->uiType == ZH_FT_BLOB ||
               pField->uiType == ZH_FT_OLE )
      {
         ZH_BYTE * pFieldBuf = pArea->pRecord + pArea->pFieldOffset[ uiIndex ];
         ZH_USHORT uiLen = pField->uiLen;

         if( uiLen == 4 )
            return ZH_GET_LE_UINT32( pFieldBuf ) != 0;
         if( uiLen == 10 )
         {
            if( pArea->bMemoType == DB_MEMO_SMT )
               return ZH_GET_LE_UINT32( ( ( LPSMTFIELD ) pFieldBuf )->block ) != 0;
            do
            {
               if( *pFieldBuf >= '1' && *pFieldBuf <= '9' )
                  return ZH_TRUE;
               ++pFieldBuf;
            }
            while( --uiLen );
         }
      }
   }
   return ZH_FALSE;
}
#endif

static ZH_ERRCODE zh_fptLockForRead( FPTAREAP pArea, ZH_USHORT uiIndex, ZH_BOOL * fUnLock )
{
   ZH_ERRCODE errCode;
   ZH_BOOL fLocked;

   *fUnLock = ZH_FALSE;
#ifdef ZH_MEMO_SAFELOCK
   if( pArea->lpdbPendingRel )
   {
      errCode = SELF_FORCEREL( &pArea->area );
      if( errCode != ZH_SUCCESS )
         return errCode;
   }

   if( ( uiIndex > 0 && pArea->area.lpFields[ uiIndex - 1 ].uiType == ZH_FT_ANY &&
         pArea->area.lpFields[ uiIndex - 1 ].uiLen < 6 ) ||
       ! pArea->fPositioned || ! pArea->fShared ||
       pArea->fFLocked || pArea->fRecordChanged )
   {
      fLocked = ZH_TRUE;
   }
   else
   {
      PZH_ITEM pRecNo = zh_itemNew( NULL ), pResult = zh_itemNew( NULL );

      errCode = SELF_RECINFO( &pArea->area, pRecNo, DBRI_LOCKED, pResult );
      fLocked = zh_itemGetL( pResult );
      zh_itemRelease( pRecNo );
      zh_itemRelease( pResult );
      if( errCode != ZH_SUCCESS )
         return errCode;
   }

   if( ! fLocked )
   {
      if( ! pArea->fValidBuffer || uiIndex == 0 ||
          zh_fptHasMemoData( pArea, uiIndex ) )
      {
         if( ! zh_fptFileLockSh( pArea, ZH_TRUE ) )
            return ZH_FAILURE;

         *fUnLock = ZH_TRUE;
         pArea->fValidBuffer = ZH_FALSE;
      }
   }
#else
   ZH_SYMBOL_UNUSED( uiIndex );
#endif
   /* update any pending relations and reread record if necessary */
   errCode = SELF_DELETED( &pArea->area, &fLocked );

   return errCode;
}

static ZH_ERRCODE zh_fptGetVarField( FPTAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem, PZH_FILE pFile )
{
   LPFIELD pField;
   ZH_ERRCODE errCode;
   ZH_BOOL fUnLock = ZH_FALSE;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptGetVarField(%p, %hu, %p, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem, ( void * ) pFile ) );

   pField = pArea->area.lpFields + uiIndex - 1;

   if( pField->uiType == ZH_FT_ANY )
   {
      ZH_USHORT uiType;
      ZH_BYTE * pFieldBuf;

      errCode = zh_fptLockForRead( pArea, uiIndex, &fUnLock );
      if( errCode != ZH_SUCCESS )
         return errCode;

      pFieldBuf = pArea->pRecord + pArea->pFieldOffset[ uiIndex - 1 ];
      if( pField->uiLen >= 6 )
         uiType = ZH_GET_LE_UINT16( pFieldBuf + pField->uiLen - 2 );
      else
         uiType = 0;

      if( pField->uiLen == 3 || uiType == ZH_VF_DATE )
         zh_itemPutDL( pItem, zh_sxPtoD( ( char * ) pFieldBuf ) );
      else if( pField->uiLen == 4 || uiType == ZH_VF_INT )
         zh_itemPutNIntLen( pItem, ( ZH_MAXINT ) ZH_GET_LE_INT32( pFieldBuf ), 10 );
      else if( pField->uiLen == 2 )
         zh_itemPutNIntLen( pItem, ( int ) ZH_GET_LE_INT16( pFieldBuf ), 10 );
      else if( pField->uiLen == 1 )
         zh_itemPutNILen( pItem, ( signed char ) pFieldBuf[ 0 ], 4 );
      else if( pField->uiLen >= 6 )
      {
         ZH_ULONG ulBlock = ZH_GET_LE_UINT32( pFieldBuf + pField->uiLen - 6 );

         if( uiType <= ZH_VF_CHAR ) /* 64000 max string size */
         {
            const char * pString;
            char * pAlloc = NULL, * pPtr;
            ZH_ULONG ulLen = uiType;

            if( uiType <= pField->uiLen - 2 )
            {
               pString = ( const char * ) pFieldBuf;
               if( ulLen > 0 )
               {
                  if( ( pField->uiFlags & ZH_FF_BINARY ) == 0 &&
                           zh_vmCodepage() != pArea->area.cdPage )
                  {
                     ZH_SIZE nLen = ulLen;
                     pString = pAlloc = zh_cdpnDup( pString, &nLen,
                                                    pArea->area.cdPage, zh_vmCodepage() );
                     ulLen = ( ZH_ULONG ) nLen;
                  }
               }
            }
            else
            {
               ZH_ULONG ulSize = ulLen;
               pString = pPtr = pAlloc = ( char * ) zh_xgrab( ulLen + 1 );

               if( pField->uiLen > 6 )
               {
                  ZH_USHORT uiVLen = pField->uiLen - 6;
                  memcpy( pPtr, pFieldBuf, uiVLen );
                  ulSize -= uiVLen;
                  pPtr += uiVLen;
               }
               errCode = zh_fptReadRawBlock( pArea, ( ZH_BYTE * ) pPtr, NULL, ulBlock, ulSize );
               if( errCode == ZH_SUCCESS && ulLen > 0 &&
                   ( pField->uiFlags & ZH_FF_BINARY ) == 0 &&
                   zh_vmCodepage() != pArea->area.cdPage )
               {
                  ZH_SIZE nLen;
                  ZH_SIZE nSize;
                  ulSize = ulLen + 1;
                  nLen = ulLen;
                  nSize = ulSize;
                  pString = zh_cdpnDup3( pString, ulLen, pAlloc, &nLen,
                                         &pAlloc, &nSize, pArea->area.cdPage, zh_vmCodepage() );
                  ulLen = ( ZH_ULONG ) nLen;
               }
            }

            if( errCode == ZH_SUCCESS )
            {
               if( pFile != NULL )
               {
                  if( zh_fileWrite( pFile, pString, ulLen, -1 ) != ulLen )
                     errCode = EDBF_WRITE;
               }
               else if( pAlloc )
               {
                  zh_itemPutCLPtr( pItem, pAlloc, ulLen );
                  pAlloc = NULL;
               }
               else
                  zh_itemPutCL( pItem, pString, ulLen );
            }
            if( pAlloc )
               zh_xfree( pAlloc );
         }
         else if( uiType == ZH_VF_LOG )
         {
            if( pFile != NULL )
               errCode = EDBF_DATATYPE;
            else
               zh_itemPutL( pItem, pFieldBuf[ 0 ] != 0 );
         }
         else if( uiType == ZH_VF_DNUM ) /* n>12 VFIELD else MEMO (bLen[1],bDec[1],dVal[8]) */
         {
            if( pFile != NULL )
               errCode = EDBF_DATATYPE;
            else
            {
               ZH_BYTE pBuffer[ 11 ];

               /* should be <= 11 - it's SIX bug but I replicated it for
                  compatibility */
               if( pField->uiLen <= 12 )
               {
                  errCode = zh_fptReadRawBlock( pArea, pBuffer, NULL, ulBlock, 11 );
                  if( errCode == ZH_SUCCESS )
                  {
                     if( pBuffer[ 0 ] == SMT_IT_DOUBLE )
                        pFieldBuf = pBuffer + 1;
                     else
                        errCode = EDBF_CORRUPT;
                  }
               }
               if( errCode == ZH_SUCCESS )
               {
                  int iWidth, iDec;
                  iWidth = *pFieldBuf++;
                  iDec = *pFieldBuf++;
                  if( iDec )
                     iWidth += iDec + 1;
                  zh_itemPutNDLen( pItem, ZH_GET_LE_DOUBLE( pFieldBuf ), iWidth, iDec );
               }
            }
         }
         else if( uiType == ZH_VF_ARRAY ) /* MEMO only as SMT ARRAY */
         {
            if( pFile != NULL )
               errCode = EDBF_DATATYPE;
            else
               errCode = zh_fptReadSMTBlock( pArea, pItem, ulBlock, 0,
                                             ( pField->uiFlags & ZH_FF_BINARY ) == 0 &&
                                             zh_vmCodepage() != pArea->area.cdPage ?
                                             FPT_TRANS_CP : FPT_TRANS_NONE );
         }
         else if( uiType == ZH_VF_BLOB )
            errCode = zh_fptReadBlobBlock( pArea, pItem, pFile, ulBlock, 0 );
         else if( uiType == ZH_VF_BLOBCOMPRESS )
            errCode = zh_fptReadBlobBlock( pArea, pItem, pFile, ulBlock, BLOB_IMPORT_COMPRESS );
         else if( uiType == ZH_VF_BLOBENCRYPT )
            errCode = zh_fptReadBlobBlock( pArea, pItem, pFile, ulBlock, BLOB_IMPORT_ENCRYPT );
         else
            errCode = EDBF_DATATYPE;
      }
   }
   else if( pField->uiType == ZH_FT_MEMO ||
            pField->uiType == ZH_FT_IMAGE ||
            pField->uiType == ZH_FT_BLOB ||
            pField->uiType == ZH_FT_OLE )
   {
      errCode = zh_fptLockForRead( pArea, uiIndex, &fUnLock );
      if( errCode != ZH_SUCCESS )
         return errCode;

      errCode = zh_fptGetMemo( pArea, uiIndex, pItem, pFile, 0, 0, 0,
                               ( pField->uiFlags & ZH_FF_UNICODE ) != 0 ? FPT_TRANS_UNICODE :
                               ( ( pField->uiFlags & ZH_FF_BINARY ) == 0 &&
                                 zh_vmCodepage() != pArea->area.cdPage ? FPT_TRANS_CP : FPT_TRANS_NONE ) );
   }
   else if( pFile == NULL )
   {
      return SUPER_GETVALUE( &pArea->area, uiIndex, pItem );
   }
   else
   {
      return ZH_FAILURE;
   }

   if( fUnLock )
      zh_fptFileUnLockSh( pArea );

   return errCode;
}

static ZH_ERRCODE zh_fptGetVarFile( FPTAREAP pArea, ZH_ULONG ulBlock, const char * szFile, ZH_USHORT uiMode, int iTrans )
{
   ZH_ERRCODE errCode;
   PZH_FILE pFile;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptGetVarFile(%p, %lu, %s, %hu, %d)", ( void * ) pArea, ulBlock, szFile, uiMode, iTrans ) );

   pFile = zh_fileExtOpen( szFile, NULL, FO_WRITE | FO_EXCLUSIVE |
                           FXO_DEFAULTS | FXO_SHARELOCK |
                           ( uiMode == FILEGET_APPEND ?
                           FXO_APPEND : FXO_TRUNCATE ),
                           NULL, NULL );

   if( pFile == NULL )
   {
      errCode = uiMode != FILEGET_APPEND ? EDBF_CREATE : EDBF_OPEN_DBF;
   }
   else
   {
      zh_fileSeek( pFile, 0, FS_END );
      errCode = zh_fptGetMemo( pArea, 0, NULL, pFile, ulBlock, 0, 0, iTrans );
      zh_fileClose( pFile );
   }

   /* Exit if any error */
   if( errCode != ZH_SUCCESS )
   {
      if( errCode != ZH_FAILURE )
      {
         zh_memoErrorRT( pArea, 0, errCode,
                         errCode == EDBF_OPEN_DBF || errCode == EDBF_CREATE ||
                         errCode == EDBF_WRITE ? szFile :
                         pArea->szMemoFileName, 0, 0 );
      }
      return ZH_FAILURE;
   }
   return ZH_SUCCESS;
}

static ZH_ULONG zh_fptPutVarFile( FPTAREAP pArea, ZH_ULONG ulBlock, const char * szFile )
{
   ZH_ERRCODE errCode;
   PZH_FILE pFile;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptPutVarFile(%p, %lu, %s)", ( void * ) pArea, ulBlock, szFile ) );

   pFile = zh_fileExtOpen( szFile, NULL, FO_READ | FO_DENYNONE |
                           FXO_DEFAULTS | FXO_SHARELOCK, NULL, NULL );
   if( pFile == NULL )
   {
      errCode = EDBF_OPEN_DBF;
   }
   else
   {
      ZH_ULONG ulSize;
      ZH_FOFFSET size = zh_fileSize( pFile );
      zh_fileSeek( pFile, 0, FS_SET );
      if( ( ZH_FOFFSET ) ( size & 0xFFFFFFFFUL ) == size )
         ulSize = ZH_MIN( ( ZH_ULONG ) size, 0xFFFFFFFFUL - sizeof( FPTBLOCK ) );
      else
         ulSize = ( ZH_ULONG ) ZH_MIN( size, ( ZH_FOFFSET ) ( 0xFFFFFFFFUL - sizeof( FPTBLOCK ) ) );

      if( zh_fptFileLockEx( pArea, ZH_TRUE ) )
      {
         errCode = zh_fptWriteMemo( pArea, ulBlock, 0, NULL, pFile,
                                    0, ulSize, &ulBlock );
         zh_fptFileUnLockEx( pArea );
      }
      else
      {
         errCode = EDBF_LOCK;
      }
      zh_fileClose( pFile );
   }

   if( errCode != ZH_SUCCESS )
   {
      zh_memoErrorRT( pArea, 0, errCode,
                      errCode == EDBF_OPEN_DBF || errCode == EDBF_READ ?
                      szFile : pArea->szMemoFileName, 0, 0 );
      ulBlock = 0;
   }

   return ulBlock;
}

static ZH_ERRCODE zh_fptPutVarField( FPTAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   LPFIELD pField;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptPutVarField(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   pField = pArea->area.lpFields + uiIndex - 1;

   if( pField->uiType == ZH_FT_ANY ||
       pField->uiType == ZH_FT_MEMO ||
       pField->uiType == ZH_FT_IMAGE ||
       pField->uiType == ZH_FT_BLOB ||
       pField->uiType == ZH_FT_OLE )
   {
      ZH_BYTE * pFieldBuf = pArea->pRecord + pArea->pFieldOffset[ uiIndex - 1 ];
      ZH_ERRCODE errCode;
      ZH_BOOL bDeleted;

      /* update any pending relations and reread record if necessary */
      errCode = SELF_DELETED( &pArea->area, &bDeleted );
      if( errCode != ZH_SUCCESS )
         return errCode;

      if( ! pArea->fPositioned )
         return ZH_SUCCESS;

      /* Buffer is hot? */
      if( ! pArea->fRecordChanged )
      {
         errCode = SELF_GOHOT( &pArea->area );
         if( errCode != ZH_SUCCESS )
            return errCode;
      }

      if( pField->uiType != ZH_FT_ANY )
      {
         if( ! zh_fptFileLockEx( pArea, ZH_TRUE ) )
            return EDBF_LOCK;
         errCode = zh_fptPutMemo( pArea, uiIndex, pItem, NULL,
                                  ( pField->uiFlags & ZH_FF_UNICODE ) != 0 ? FPT_TRANS_UNICODE :
                                  ( ( pField->uiFlags & ZH_FF_BINARY ) == 0 &&
                                    zh_vmCodepage() != pArea->area.cdPage ? FPT_TRANS_CP : FPT_TRANS_NONE ) );
#if defined( ZH_MEMO_SAFELOCK )
         if( errCode == ZH_SUCCESS )
         {
            /* Force writer record to eliminate race condition */
            SELF_GOCOLD( &pArea->area );
         }
#endif
         zh_fptFileUnLockEx( pArea );
      }
      else if( pField->uiLen == 3 )
      {
         if( ! ZH_IS_DATETIME( pItem ) )
         {
            return EDBF_DATATYPE;
         }
         zh_sxDtoP( ( char * ) pFieldBuf, zh_itemGetDL( pItem ) );
      }
      else if( pField->uiLen == 4 )
      {
         ZH_MAXINT lVal;

         if( ! ZH_IS_NUMBER( pItem ) )
            return EDBF_DATATYPE;
         lVal = zh_itemGetNInt( pItem );
         if( ZH_IS_DOUBLE( pItem ) ?
             ! ZH_DBL_LIM_INT32( zh_itemGetND( pItem ) ) :
             ! ZH_LIM_INT32( lVal ) )
         {
            return EDBF_DATAWIDTH;
         }
         ZH_PUT_LE_UINT32( pFieldBuf, ( ZH_U32 ) lVal );
      }
      else if( pField->uiLen < 6 )
      {
         return EDBF_DATATYPE;
      }
      else
      {
         ZH_BYTE buffer[ 11 ], * pAlloc = NULL, *pbTmp;
         const ZH_BYTE * pBlock = NULL;
         ZH_ULONG ulOldBlock = 0, ulOldSize = 0, ulNewSize = 0;
         ZH_USHORT uiType = ZH_GET_LE_UINT16( pFieldBuf + pField->uiLen - 2 );

         if( ( uiType <= ZH_VF_CHAR && uiType > pField->uiLen - 2 ) ||
             ( uiType == ZH_VF_DNUM && pField->uiLen <= 12 ) ||
             uiType == ZH_VF_ARRAY || uiType == ZH_VF_BLOB ||
             uiType == ZH_VF_BLOBCOMPRESS || uiType == ZH_VF_BLOBENCRYPT )
         {
            ulOldBlock = ZH_GET_LE_UINT32( pFieldBuf + pField->uiLen - 6 );
            if( ulOldBlock )
            {
               if( uiType <= ZH_VF_CHAR )
                  ulOldSize = uiType - ( pField->uiLen - 6 );
               else if( uiType == ZH_VF_DNUM )
                  ulOldSize = 11;
               else if( uiType == ZH_VF_ARRAY )
               {
                  ZH_FOFFSET fOffset = FPT_BLOCK_OFFSET( ulOldBlock );
                  if( zh_fptCountSMTDataLength( pArea, &fOffset ) != ZH_SUCCESS )
                     ulOldSize = 0;
                  else
                     ulOldSize = ( ZH_ULONG ) ( fOffset - FPT_BLOCK_OFFSET( ulOldBlock ) );
               }
            }
         }

         if( ZH_IS_DATETIME( pItem ) )
         {
            zh_sxDtoP( ( char * ) pFieldBuf, zh_itemGetDL( pItem ) );
            uiType = ZH_VF_DATE;
         }
         else if( ZH_IS_LOGICAL( pItem ) )
         {
            pFieldBuf[ 0 ] = zh_itemGetL( pItem ) ? 1 : 0;
            uiType = ZH_VF_LOG;
         }
         else if( ZH_IS_NIL( pItem ) )
         {
            uiType = 0;
         }
         else if( ZH_IS_NUMBER( pItem ) )
         {
            ZH_MAXINT lVal = zh_itemGetNInt( pItem );

            if( ! ZH_IS_DOUBLE( pItem ) && ZH_LIM_INT32( lVal ) )
            {
               ZH_PUT_LE_UINT32( pFieldBuf, ( ZH_U32 ) lVal );
               uiType = ZH_VF_INT;
            }
            else
            {
               double dVal = zh_itemGetND( pItem );
               int iWidth, iDec;

               zh_itemGetNLen( pItem, &iWidth, &iDec );
               if( iDec )
                  iWidth += iDec + 1;
               buffer[ 0 ] = SMT_IT_DOUBLE;
               buffer[ 1 ] = ( ZH_BYTE ) iWidth;
               buffer[ 2 ] = ( ZH_BYTE ) iDec;
               ZH_PUT_LE_DOUBLE( &buffer[ 3 ], dVal );
               uiType = ZH_VF_DNUM;
               if( pField->uiLen > 12 )
                  memcpy( pFieldBuf, buffer + 1, 10 );
               else
               {
                  pBlock = buffer;
                  ulNewSize = 11;
               }
            }
         }
         else if( ZH_IS_STRING( pItem ) )
         {
            ZH_SIZE nLen = zh_itemGetCLen( pItem );

            pBlock = ( const ZH_BYTE * ) zh_itemGetCPtr( pItem );
            if( nLen > ZH_VF_CHAR )
               nLen = ZH_VF_CHAR;
            if( nLen > 0 && ( pField->uiFlags & ZH_FF_BINARY ) == 0 &&
                zh_vmCodepage() != pArea->area.cdPage )
            {
               pBlock = pAlloc = ( ZH_BYTE * )
                                 zh_cdpnDup( ( const char * ) pBlock, &nLen,
                                             zh_vmCodepage(), pArea->area.cdPage );
               if( nLen > ZH_VF_CHAR )
                  nLen = ZH_VF_CHAR;
            }
            uiType = ( ZH_USHORT ) nLen;
            if( uiType <= pField->uiLen - 2 )
            {
               memcpy( pFieldBuf, pBlock, uiType );
            }
            else
            {
               ulNewSize = uiType;
               if( pField->uiLen > 6 )
               {
                  memcpy( pFieldBuf, pBlock, pField->uiLen - 6 );
                  ulNewSize -= pField->uiLen - 6;
                  pBlock += pField->uiLen - 6;
               }
            }
         }
         else if( ZH_IS_ARRAY( pItem ) )
         {
            ZH_ULONG ulArrayCount = 0;
            int iTrans;

#if 0
            if( ( pField->uiFlags & ZH_FF_UNICODE ) != 0 )
               iTrans = FPT_TRANS_UNICODE;
            else
#endif
            if( ( pField->uiFlags & ZH_FF_BINARY ) == 0 &&
                zh_vmCodepage() != pArea->area.cdPage )
               iTrans = FPT_TRANS_CP;
            else
               iTrans = FPT_TRANS_NONE;

            ulNewSize = zh_fptCountSMTItemLength( pArea, pItem, &ulArrayCount, iTrans );
            pbTmp = pAlloc = ( ZH_BYTE * ) zh_xgrab( ulNewSize );
            zh_fptStoreSMTItem( pArea, pItem, &pbTmp, iTrans );
            pBlock = pAlloc;
            uiType = ZH_VF_ARRAY;
         }
         else
         {
            return EDBF_DATATYPE;
         }

         ZH_PUT_LE_UINT16( pFieldBuf + pField->uiLen - 2, uiType );
         if( ulNewSize )
            ZH_PUT_LE_UINT32( pFieldBuf + pField->uiLen - 6, 0 );
         if( ulOldBlock != 0 || ulNewSize != 0 )
         {
            if( ! zh_fptFileLockEx( pArea, ZH_TRUE ) )
            {
               errCode = EDBF_LOCK;
            }
            else
            {
               errCode = zh_fptWriteMemo( pArea, ulOldBlock, ulOldSize,
                                          pBlock, NULL,
                                          FPTIT_DUMMY, ulNewSize, &ulOldBlock );
               if( errCode == ZH_SUCCESS )
               {
                  if( ulNewSize )
                     ZH_PUT_LE_UINT32( pFieldBuf + pField->uiLen - 6, ulOldBlock );
#if defined( ZH_MEMO_SAFELOCK )
                  /* Force writer record to eliminate race condition */
                  SELF_GOCOLD( &pArea->area );
#endif
               }
               zh_fptFileUnLockEx( pArea );
            }
         }
         if( pAlloc )
            zh_xfree( pAlloc );
      }

      return errCode;
   }
   return SUPER_PUTVALUE( &pArea->area, uiIndex, pItem );
}


/* FPT METHODS */

/*
 * Open a data store in the WorkArea.
 * ( DBENTRYP_VO )    zh_fptOpen            : NULL
 */

/*
 * Retrieve the size of the WorkArea structure.
 * ( DBENTRYP_SP )    zh_fptStructSize
 */
static ZH_ERRCODE zh_fptStructSize( FPTAREAP pArea, ZH_USHORT * uiSize )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptStrucSize(%p, %p)", ( void * ) pArea, ( void * ) uiSize ) );
   ZH_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( FPTAREA );
   return ZH_SUCCESS;
}

/*
 * Obtain the length of a field value.
 * ( DBENTRYP_SVL )   zh_fptGetVarLen
 */
static ZH_ERRCODE zh_fptGetVarLen( FPTAREAP pArea, ZH_USHORT uiIndex, ZH_ULONG * pLength )
{

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptGetVarLen(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pLength ) );

   if( pArea->fHasMemo && pArea->pMemoFile &&
       ( pArea->area.lpFields[ uiIndex - 1 ].uiType == ZH_FT_MEMO ||
         pArea->area.lpFields[ uiIndex - 1 ].uiType == ZH_FT_IMAGE ||
         pArea->area.lpFields[ uiIndex - 1 ].uiType == ZH_FT_BLOB ||
         pArea->area.lpFields[ uiIndex - 1 ].uiType == ZH_FT_OLE ) )
   {
      ZH_ERRCODE errCode;
      ZH_BOOL fUnLock;

      errCode = zh_fptLockForRead( pArea, uiIndex, &fUnLock );
      if( errCode == ZH_SUCCESS )
         *pLength = zh_fptGetMemoLen( pArea, uiIndex );
      else
         *pLength = 0;

      if( fUnLock )
         zh_fptFileUnLockSh( pArea );

      return errCode;
   }

   return SUPER_GETVARLEN( &pArea->area, uiIndex, pLength );
}

/*
 * Obtain the current value of a field.
 * ( DBENTRYP_SI )    zh_fptGetValue
 */
static ZH_ERRCODE zh_fptGetValue( FPTAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptGetValue(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   if( ! uiIndex || uiIndex > pArea->area.uiFieldCount )
      return ZH_FAILURE;

   errCode = zh_fptGetVarField( pArea, uiIndex, pItem, NULL );

   if( errCode != ZH_SUCCESS )
   {
      if( errCode == ZH_FAILURE )
         return ZH_FAILURE;
      zh_memoErrorRT( pArea, 0, errCode, pArea->szMemoFileName, 0, 0 );
   }
   return ZH_SUCCESS;
}

/*
 * Assign a value to a field.
 * ( DBENTRYP_SI )    zh_fptPutValue
 */
static ZH_ERRCODE zh_fptPutValue( FPTAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   ZH_ERRCODE errCode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptPutValue(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   if( ! uiIndex || uiIndex > pArea->area.uiFieldCount )
      return ZH_FAILURE;

   errCode = zh_fptPutVarField( pArea, uiIndex, pItem );
   if( errCode != ZH_SUCCESS )
   {
      if( errCode == ZH_FAILURE )
         return ZH_FAILURE;
      zh_memoErrorRT( pArea, 0, errCode, pArea->szMemoFileName, 0, EF_CANDEFAULT );
   }
   return ZH_SUCCESS;
}

/* ( DBENTRYP_V )     zh_fptCloseMemFile    : NULL */

/*
 * Create a memo file in the WorkArea.
 * ( DBENTRYP_VO )    zh_fptCreateMemFile
 */
static ZH_ERRCODE zh_fptCreateMemFile( FPTAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   FPTHEADER fptHeader;
   ZH_ULONG ulNextBlock, ulSize, ulLen;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptCreateMemFile(%p, %p)", ( void * ) pArea, ( void * ) pCreateInfo ) );

   if( pCreateInfo )
   {
      char szFileName[ ZH_PATH_MAX ];
      PZH_FNAME pFileName;
      PZH_ITEM pError = NULL, pItem = NULL;
      ZH_BOOL bRetry;

      if( ! pArea->bMemoType )
      {
         pItem = zh_itemPutNil( pItem );
         if( SELF_INFO( &pArea->area, DBI_MEMOTYPE, pItem ) != ZH_SUCCESS )
         {
            zh_itemRelease( pItem );
            return ZH_FAILURE;
         }
         pArea->bMemoType = ( ZH_BYTE ) zh_itemGetNI( pItem );
#if 0
         if( ! pArea->bMemoType )
         {
            pArea->bMemoType = DB_MEMO_FPT;
            pArea->uiMemoVersion = DB_MEMOVER_FLEX;
         }
#endif
         if( pArea->bMemoType != DB_MEMO_DBT &&
             pArea->bMemoType != DB_MEMO_FPT &&
             pArea->bMemoType != DB_MEMO_SMT )
         {
            zh_memoErrorRT( pArea, EG_CREATE, EDBF_MEMOTYPE,
                            pCreateInfo->abName, 0, 0 );
            zh_itemRelease( pItem );
            return ZH_FAILURE;
         }
      }
      if( ! pArea->uiMemoVersion )
      {
         if( pArea->bMemoType == DB_MEMO_SMT )
            pArea->uiMemoVersion = DB_MEMOVER_SIX;
         else if( pArea->bMemoType == DB_MEMO_FPT )
         {
            pItem = zh_itemPutNil( pItem );
            if( SELF_INFO( &pArea->area, DBI_MEMOVERSION, pItem ) != ZH_SUCCESS )
            {
               zh_itemRelease( pItem );
               return ZH_FAILURE;
            }
            pArea->uiMemoVersion = ( ZH_USHORT ) zh_itemGetNI( pItem );
         }
         else
            pArea->uiMemoVersion = DB_MEMOVER_STD;
      }
      if( ! pArea->ulMemoBlockSize )
      {
         pItem = zh_itemPutNil( pItem );
         if( SELF_INFO( &pArea->area, DBI_MEMOBLOCKSIZE, pItem ) != ZH_SUCCESS )
         {
            zh_itemRelease( pItem );
            return ZH_FAILURE;
         }
         pArea->ulMemoBlockSize = zh_itemGetNL( pItem );
      }

      if( ! pArea->fTemporary )
      {
         /* create file name */
         pFileName = zh_fsFNameSplit( pCreateInfo->abName );
         if( ! pFileName->szExtension )
         {
            pItem = zh_itemPutNil( pItem );
            if( SELF_INFO( &pArea->area, DBI_MEMOEXT, pItem ) == ZH_SUCCESS )
            {
               pFileName->szExtension = zh_itemGetCPtr( pItem );
               zh_fsFNameMerge( szFileName, pFileName );
            }
         }
         else
         {
            zh_strncpy( szFileName, pCreateInfo->abName, sizeof( szFileName ) - 1 );
         }
         zh_xfree( pFileName );
      }

      if( pItem )
      {
         zh_itemRelease( pItem );
      }

      /* Try create */
      do
      {
         if( pArea->fTemporary )
            pArea->pMemoFile = zh_fileCreateTempEx( szFileName, NULL, NULL, NULL, FC_NORMAL );
         else
            pArea->pMemoFile = zh_fileExtOpen( szFileName, NULL,
                                               FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE |
                                               FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME |
                                               FXO_NOSEEKPOS,
                                               NULL, pError );
         if( ! pArea->pMemoFile )
         {
            if( ! pError )
            {
               pError = zh_errNew();
               zh_errPutGenCode( pError, EG_CREATE );
               zh_errPutSubCode( pError, EDBF_CREATE_MEMO );
               zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_CREATE ) );
               zh_errPutFileName( pError, szFileName );
               zh_errPutFlags( pError, EF_CANRETRY );
            }
            zh_errPutOsCode( pError, zh_fsError() );
            bRetry = ( SELF_ERROR( &pArea->area, pError ) == E_RETRY );
         }
         else
            bRetry = ZH_FALSE;
      }
      while( bRetry );

      if( pError )
         zh_itemRelease( pError );

      if( ! pArea->pMemoFile )
         return ZH_FAILURE;

      pArea->szMemoFileName = zh_strdup( szFileName );
   }
   /* else -> For zap file */

   memset( &fptHeader, 0, sizeof( fptHeader ) );
   ulSize = 512;
   if( pArea->uiMemoVersion == DB_MEMOVER_SIX )
   {
      memcpy( fptHeader.signature1, "SIxMemo", 8 );
   }
   else
   {
      memcpy( fptHeader.signature1, "Ziher", 8 );
      if( pArea->uiMemoVersion == DB_MEMOVER_FLEX ||
          pArea->uiMemoVersion == DB_MEMOVER_CLIP )
      {
         memcpy( fptHeader.signature2, "FlexFile3\003", 11 );
         ulSize = sizeof( FPTHEADER );
         if( pArea->area.rddID == s_uiRddIdBLOB )
         {
            ZH_PUT_LE_UINT16( fptHeader.flexSize, ( ZH_U16 ) pArea->ulMemoBlockSize );
         }
      }
   }
   ulNextBlock = ( ulSize + pArea->ulMemoBlockSize - 1 ) / pArea->ulMemoBlockSize;
   if( pArea->bMemoType == DB_MEMO_SMT || pArea->bMemoType == DB_MEMO_DBT )
   {
      ZH_PUT_LE_UINT32( fptHeader.nextBlock, ulNextBlock );
      ZH_PUT_LE_UINT32( fptHeader.blockSize, ( ZH_U32 ) pArea->ulMemoBlockSize );
   }
   else
   {
      ZH_PUT_BE_UINT32( fptHeader.nextBlock, ulNextBlock );
      ZH_PUT_BE_UINT32( fptHeader.blockSize, ( ZH_U32 ) pArea->ulMemoBlockSize );
   }
   if( zh_fileWriteAt( pArea->pMemoFile, &fptHeader, ulSize, 0 ) != ulSize )
      return ZH_FAILURE;

   ulLen = ulNextBlock * pArea->ulMemoBlockSize - ulSize;
   if( ulLen > ulSize )
   {
      memset( &fptHeader, 0, sizeof( fptHeader ) );
      do
      {
         ZH_ULONG ulWrite = ZH_MIN( ulLen - ulSize, sizeof( FPTHEADER ) );
         if( zh_fileWriteAt( pArea->pMemoFile, &fptHeader,
                             ulWrite, ulSize ) != ulWrite )
            return ZH_FAILURE;
         ulSize += ulWrite;
      }
      while( ulLen > ulSize );
   }
   /* trunc file */
   zh_fileTruncAt( pArea->pMemoFile, ulSize );
   pArea->fMemoFlush = ZH_TRUE;
   return ZH_SUCCESS;
}


/*
 * BLOB2FILE - retrieve memo contents into file
 * ( DBENTRYP_SCCS )  zh_fptGetValueFile
 */
static ZH_ERRCODE zh_fptGetValueFile( FPTAREAP pArea, ZH_USHORT uiIndex, const char * szFile, ZH_USHORT uiMode )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptGetValueFile(%p, %hu, %s, %hu)", ( void * ) pArea, uiIndex, szFile, uiMode ) );

   if( ! uiIndex || uiIndex > pArea->area.uiFieldCount )
      return ZH_FAILURE;

   if( pArea->fHasMemo && pArea->pMemoFile &&
       ( pArea->area.lpFields[ uiIndex - 1 ].uiType == ZH_FT_MEMO ||
         pArea->area.lpFields[ uiIndex - 1 ].uiType == ZH_FT_IMAGE ||
         pArea->area.lpFields[ uiIndex - 1 ].uiType == ZH_FT_BLOB ||
         pArea->area.lpFields[ uiIndex - 1 ].uiType == ZH_FT_OLE ||
         pArea->area.lpFields[ uiIndex - 1 ].uiType == ZH_FT_ANY ) )
   {
      ZH_ERRCODE errCode;
      PZH_FILE pFile;

      pFile = zh_fileExtOpen( szFile, NULL, FO_WRITE | FO_EXCLUSIVE |
                              FXO_DEFAULTS | FXO_SHARELOCK |
                              ( uiMode == FILEGET_APPEND ?
                              FXO_APPEND : FXO_TRUNCATE ),
                              NULL, NULL );

      if( pFile == NULL )
      {
         errCode = uiMode != FILEGET_APPEND ? EDBF_CREATE : EDBF_OPEN_DBF;
      }
      else
      {
         zh_fileSeek( pFile, 0, FS_END );
         errCode = zh_fptGetVarField( pArea, uiIndex, NULL, pFile );
         zh_fileClose( pFile );
      }

      /* Exit if any error */
      if( errCode != ZH_SUCCESS )
      {
         if( errCode != ZH_FAILURE )
         {
            zh_memoErrorRT( pArea, 0, errCode,
                            errCode == EDBF_OPEN_DBF || errCode == EDBF_CREATE ||
                            errCode == EDBF_WRITE ? szFile :
                            pArea->szMemoFileName, 0, 0 );
         }
         return ZH_FAILURE;
      }
      return ZH_SUCCESS;
   }
   return SUPER_GETVALUEFILE( &pArea->area, uiIndex, szFile, uiMode );
}

/*
 * Open a memo file in the specified WorkArea.
 * ( DBENTRYP_VO )    zh_fptOpenMemFile
 */
static ZH_ERRCODE zh_fptOpenMemFile( FPTAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   char szFileName[ ZH_PATH_MAX ];
   PZH_FNAME pFileName;
   PZH_ITEM pError;
   ZH_FATTR nFlags;
   ZH_BOOL bRetry;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptOpenMemFile(%p, %p)", ( void * ) pArea, ( void * ) pOpenInfo ) );

   if( pArea->area.rddID == s_uiRddIdBLOB )
   {
      pArea->bMemoType = DB_MEMO_FPT;
      pArea->uiMemoVersion = DB_MEMOVER_FLEX;
   }
   else if( pArea->bMemoType != DB_MEMO_DBT &&
            pArea->bMemoType != DB_MEMO_FPT &&
            pArea->bMemoType != DB_MEMO_SMT )
   {
      zh_memoErrorRT( pArea, EG_OPEN, EDBF_MEMOTYPE,
                      pOpenInfo->abName, 0, 0 );
      return ZH_FAILURE;
   }

   /* create file name */
   pFileName = zh_fsFNameSplit( pOpenInfo->abName );
   if( ! pFileName->szExtension )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      if( SELF_INFO( &pArea->area, DBI_MEMOEXT, pItem ) == ZH_SUCCESS )
      {
         pFileName->szExtension = zh_itemGetCPtr( pItem );
         zh_fsFNameMerge( szFileName, pFileName );
      }
      zh_itemRelease( pItem );
   }
   else
   {
      zh_strncpy( szFileName, pOpenInfo->abName, sizeof( szFileName ) - 1 );
   }
   zh_xfree( pFileName );

   nFlags = ( pOpenInfo->fReadonly ? FO_READ : FO_READWRITE ) |
            ( pOpenInfo->fShared ? FO_DENYNONE : FO_EXCLUSIVE ) |
            FXO_DEFAULTS | FXO_SHARELOCK | FXO_NOSEEKPOS;
   pError = NULL;

   /* Try open */
   do
   {
      pArea->pMemoFile = zh_fileExtOpen( szFileName, NULL, nFlags, NULL, pError );
      if( ! pArea->pMemoFile )
      {
         if( ! pError )
         {
            pError = zh_errNew();
            zh_errPutGenCode( pError, EG_OPEN );
            zh_errPutSubCode( pError, EDBF_OPEN_MEMO );
            zh_errPutDescription( pError, zh_langDGetErrorDesc( EG_OPEN ) );
            zh_errPutOsCode( pError, zh_fsError() );
            zh_errPutFileName( pError, szFileName );
            zh_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
         }
         bRetry = ( SELF_ERROR( &pArea->area, pError ) == E_RETRY );
      }
      else
         bRetry = ZH_FALSE;
   }
   while( bRetry );

   if( pError )
      zh_itemRelease( pError );

   if( ! pArea->pMemoFile )
      return ZH_FAILURE;

   pArea->szMemoFileName = zh_strdup( szFileName );

   if( pArea->bMemoType == DB_MEMO_DBT )
   {
      pArea->ulMemoBlockSize = DBT_DEFBLOCKSIZE;
   }
   else
   {
      FPTHEADER fptHeader;
      memset( &fptHeader, 0, sizeof( fptHeader ) );
      if( zh_fptFileLockSh( pArea, ZH_TRUE ) )
      {
         ZH_SIZE nRead = zh_fileReadAt( pArea->pMemoFile, &fptHeader,
                                        sizeof( FPTHEADER ), 0 );
         if( nRead >= 512 && nRead != ( ZH_SIZE ) FS_ERROR )
         {
            pArea->uiMemoVersion = DB_MEMOVER_STD;
            if( pArea->bMemoType == DB_MEMO_SMT )
               pArea->ulMemoBlockSize = ZH_GET_LE_UINT32( fptHeader.blockSize );
            else
               pArea->ulMemoBlockSize = ZH_GET_BE_UINT32( fptHeader.blockSize );
            /* hack for some buggy 3rd part memo code implementations */
            if( pArea->ulMemoBlockSize > 0x10000 &&
                ( pArea->ulMemoBlockSize & 0xFFFF ) != 0 )
            {
               pArea->ulMemoBlockSize &= 0xFFFF;
            }
            /* Check for compatibility with SIX memo headers */
            if( memcmp( fptHeader.signature1, "SIxMemo", 7 ) == 0 )
            {
               pArea->uiMemoVersion = DB_MEMOVER_SIX;
            }
            /* Check for compatibility with CLIP (www.itk.ru) memo headers */
            else if( memcmp( fptHeader.signature1, "Made by CLIP", 12 ) == 0 )
            {
               pArea->uiMemoVersion = DB_MEMOVER_CLIP;
            }
            if( pArea->uiMemoVersion != DB_MEMOVER_SIX &&
                memcmp( fptHeader.signature2, "FlexFile3\003", 10 ) == 0 )
            {
               ZH_USHORT usSize = ZH_GET_LE_UINT16( fptHeader.flexSize );
               pArea->uiMemoVersion = DB_MEMOVER_FLEX;
               if( usSize != 0 && ( pArea->ulMemoBlockSize == 0 ||
                                    pArea->area.rddID == s_uiRddIdBLOB ) )
               {
                  pArea->ulMemoBlockSize = usSize;
               }
            }
         }
         zh_fptFileUnLockSh( pArea );
      }
   }

   if( pArea->ulMemoBlockSize == 0 )
   {
      zh_memoErrorRT( pArea, EG_CORRUPTION, EDBF_CORRUPT,
                      pArea->szMemoFileName, 0, 0 );
      return ZH_FAILURE;
   }

   return ZH_SUCCESS;
}

/*
 * FILE2BLOB - store file contents in MEMO
 * ( DBENTRYP_SCCS )   zh_fptPutValueFile
 */
static ZH_ERRCODE zh_fptPutValueFile( FPTAREAP pArea, ZH_USHORT uiIndex, const char * szFile, ZH_USHORT uiMode )
{
   LPFIELD pField;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptPutValueFile(%p, %hu, %s, %hu)", ( void * ) pArea, uiIndex, szFile, uiMode ) );

   if( ! uiIndex || uiIndex > pArea->area.uiFieldCount )
      return ZH_FAILURE;

   pField = pArea->area.lpFields + uiIndex - 1;

   if( pArea->fHasMemo && pArea->pMemoFile &&
       ( pField->uiType == ZH_FT_MEMO ||
         pField->uiType == ZH_FT_IMAGE ||
         pField->uiType == ZH_FT_BLOB ||
         pField->uiType == ZH_FT_OLE ||
         ( pField->uiType == ZH_FT_ANY && pField->uiLen >= 6 ) ) )
   {
      ZH_ERRCODE errCode;
      ZH_BOOL bDeleted;
      PZH_FILE pFile;

      /* update any pending relations and reread record if necessary */
      errCode = SELF_DELETED( &pArea->area, &bDeleted );
      if( errCode != ZH_SUCCESS )
         return errCode;

      if( ! pArea->fPositioned )
         return ZH_FAILURE;

      /* Buffer is hot? */
      if( ! pArea->fRecordChanged && SELF_GOHOT( &pArea->area ) == ZH_FAILURE )
         return ZH_FAILURE;

      pFile = zh_fileExtOpen( szFile, NULL, FO_READ | FO_DENYNONE |
                              FXO_DEFAULTS | FXO_SHARELOCK, NULL, NULL );
      if( pFile == NULL )
      {
         errCode = EDBF_OPEN_DBF;
      }
      else if( pField->uiType == ZH_FT_ANY )
      {
         ZH_BYTE * pAlloc;
         ZH_ULONG ulSize;
         ZH_FOFFSET size = zh_fileSize( pFile );

         ulSize = ( ZH_ULONG ) ZH_MIN( size, ZH_VF_CHAR );
         pAlloc = ( ZH_BYTE * ) zh_xgrab( ulSize + 1 );
         if( zh_fileReadAt( pFile, pAlloc, ulSize, 0 ) != ulSize )
         {
            errCode = EDBF_READ;
            zh_xfree( pAlloc );
         }
         else
         {
            pAlloc[ ulSize ] = '\0';
         }
         zh_fileClose( pFile );
         if( errCode == ZH_SUCCESS )
         {
            PZH_ITEM pItem = zh_itemPutCLPtr( NULL, ( char * ) pAlloc, ulSize );
            errCode = zh_fptPutVarField( pArea, uiIndex, pItem );
            zh_itemRelease( pItem );
         }
      }
      else if( ! zh_fptFileLockEx( pArea, ZH_TRUE ) )
      {
         zh_fileClose( pFile );
         errCode = EDBF_LOCK;
      }
      else
      {
         ZH_ULONG ulSize, ulBlock, ulType, ulOldSize, ulOldType;
         ZH_FOFFSET size = zh_fileSize( pFile );

         zh_fileSeek( pFile, 0, FS_SET );
         if( ( ZH_FOFFSET ) ( size & 0xFFFFFFFFUL ) == size )
         {
            ulSize = ZH_MIN( ( ZH_ULONG ) size, 0xFFFFFFFFUL - sizeof( FPTBLOCK ) );
         }
         else
         {
            ulSize = ( ZH_ULONG ) ZH_MIN( size, ( ZH_FOFFSET ) ( 0xFFFFFFFFUL - sizeof( FPTBLOCK ) ) );
         }

         if( pArea->bMemoType == DB_MEMO_SMT )
            ulType = SMT_IT_CHAR;
         else
            ulType = FPTIT_BINARY;


         errCode = zh_dbfGetMemoData( ( DBFAREAP ) pArea, uiIndex - 1,
                                      &ulBlock, &ulOldSize, &ulOldType );
         if( errCode == ZH_SUCCESS )
            errCode = zh_fptWriteMemo( pArea, ulBlock, ulOldSize, NULL, pFile,
                                       ulType, ulSize, &ulBlock );
         if( errCode == ZH_SUCCESS )
            errCode = zh_dbfSetMemoData( ( DBFAREAP ) pArea, uiIndex - 1, ulBlock, ulSize, ulType );
#if defined( ZH_MEMO_SAFELOCK )
         if( errCode == ZH_SUCCESS )
         {
            /* Force writer record to eliminate race condition */
            SELF_GOCOLD( &pArea->area );
         }
#endif
         zh_fptFileUnLockEx( pArea );
         zh_fileClose( pFile );
      }
      /* Exit if any error */
      if( errCode != ZH_SUCCESS )
      {
         zh_memoErrorRT( pArea, 0, errCode,
                         errCode == EDBF_OPEN_DBF || errCode == EDBF_READ ?
                         szFile : pArea->szMemoFileName, 0, 0 );
         return ZH_FAILURE;
      }
      return ZH_SUCCESS;
   }

   return SUPER_PUTVALUEFILE( &pArea->area, uiIndex, szFile, uiMode );
}

static ZH_ERRCODE zh_fptDoPackRec( FPTAREAP pArea )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;
   ZH_ULONG ulBlock, ulSize, ulType;
   ZH_USHORT uiField;
   ZH_FOFFSET pos, from, size;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptDoPackRec(%p)", ( void * ) pArea ) );

   /* TODO: implement memo pack operation */
   for( uiField = 0; uiField < pArea->area.uiFieldCount; ++uiField )
   {
      LPFIELD pField = pArea->area.lpFields + uiField;

      if( pField->uiType == ZH_FT_MEMO ||
          pField->uiType == ZH_FT_IMAGE ||
          pField->uiType == ZH_FT_BLOB ||
          pField->uiType == ZH_FT_OLE )
      {
         errCode = zh_dbfGetMemoData( ( DBFAREAP ) pArea, uiField,
                                      &ulBlock, &ulSize, &ulType );
         if( errCode == ZH_SUCCESS && ulBlock != 0 )
         {
            /* Buffer is hot? */
            if( ! pArea->fRecordChanged )
               errCode = SELF_GOHOT( &pArea->area );
            if( ulSize == 0 && errCode == ZH_SUCCESS )
            {
               if( pArea->bMemoType == DB_MEMO_DBT )
               {
                  ulSize = zh_fptGetMemoLen( pArea, uiField + 1 );
                  if( ulSize )
                     ++ulSize;
               }
               else if( pArea->bMemoType == DB_MEMO_FPT )
               {
                  FPTBLOCK fptBlock;
                  if( zh_fileReadAt( pArea->pMemoFile, &fptBlock,
                                     sizeof( FPTBLOCK ), FPT_BLOCK_OFFSET( ulBlock ) ) ==
                      sizeof( FPTBLOCK ) )
                     ulSize = ZH_GET_BE_UINT32( fptBlock.size ) + sizeof( FPTBLOCK );
               }
            }
            if( ulSize && errCode == ZH_SUCCESS )
            {
               from = FPT_BLOCK_OFFSET( ulBlock );
               pos = zh_fileSize( pArea->pMemoTmpFile );
               ulBlock = ( ZH_ULONG ) ( ( pos + pArea->ulNewBlockSize - 1 ) /
                                     pArea->ulNewBlockSize );
               pos = ( ZH_FOFFSET ) ulBlock *
                     ( ZH_FOFFSET ) pArea->ulNewBlockSize;
               errCode = zh_fptCopyToFile( pArea->pMemoFile, from,
                                           pArea->pMemoTmpFile, pos, ulSize );
            }
            else
               ulBlock = ulType = 0;

            if( errCode == ZH_SUCCESS )
               errCode = zh_dbfSetMemoData( ( DBFAREAP ) pArea, uiField,
                                            ulBlock, ulSize, ulType );
         }
      }
      else if( pField->uiType == ZH_FT_ANY && pField->uiLen >= 6 )
      {
         ZH_BYTE * pFieldBuf = pArea->pRecord + pArea->pFieldOffset[ uiField ];
         ZH_BYTE buffer[ 4 ];

         ulBlock = ZH_GET_LE_UINT32( pFieldBuf + pField->uiLen - 6 );
         ulType = ZH_GET_LE_UINT16( pFieldBuf + pField->uiLen - 2 );
         size = 0;

         switch( ulType )
         {
            case ZH_VF_BLOB:
            case ZH_VF_BLOBCOMPRESS:
            case ZH_VF_BLOBENCRYPT:
               if( zh_fileReadAt( pArea->pMemoFile, buffer, 4,
                                  FPT_BLOCK_OFFSET( ulBlock ) ) != 4 )
                  errCode = EDBF_READ;
               else
                  size = ZH_GET_LE_UINT32( buffer ) + 4;
               break;
            case ZH_VF_ARRAY:
               from = FPT_BLOCK_OFFSET( ulBlock );
               errCode = zh_fptCountSMTDataLength( pArea, &from );
               size = from - FPT_BLOCK_OFFSET( ulBlock );
               break;
            case ZH_VF_DNUM:
               if( pField->uiLen <= 12 )
                  size = 11;
               break;
            default:
               if( ulType <= ZH_VF_CHAR && ( pField->uiLen - 2 ) < ( int ) ulType )
                  size = ulType - ( pField->uiLen - 6 );
               break;
         }
         if( errCode == ZH_SUCCESS && size )
         {
            /* Buffer is hot? */
            if( ! pArea->fRecordChanged )
               errCode = SELF_GOHOT( &pArea->area );
            if( errCode == ZH_SUCCESS )
            {
               from = FPT_BLOCK_OFFSET( ulBlock );
               pos = zh_fileSize( pArea->pMemoTmpFile );
               ulBlock = ( ZH_ULONG ) ( ( pos + pArea->ulNewBlockSize - 1 ) /
                                     pArea->ulNewBlockSize );
               pos = ( ZH_FOFFSET ) ulBlock *
                     ( ZH_FOFFSET ) pArea->ulNewBlockSize;
               errCode = zh_fptCopyToFile( pArea->pMemoFile, from,
                                           pArea->pMemoTmpFile, pos, size );
               if( errCode == ZH_SUCCESS )
                  ZH_PUT_LE_UINT32( pFieldBuf + pField->uiLen - 6, ulBlock );
            }
         }
      }
   }

   return errCode;
}

static ZH_ERRCODE zh_fptDoPack( FPTAREAP pArea, ZH_ULONG ulBlockSize,
                                PZH_ITEM pEvalBlock, ZH_LONG lEvalStep )
{
   ZH_ERRCODE errCode = ZH_SUCCESS;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptDoPack(%p,%lu,%p,%ld)", ( void * ) pArea, ulBlockSize, ( void * ) pEvalBlock, lEvalStep ) );

   if( pArea->fReadonly )
      errCode = EDBF_READONLY;
   else if( pArea->fShared )
      errCode = EDBF_SHARED;
   else if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
      return ZH_FAILURE;
   else if( pArea->fHasMemo && pArea->pMemoFile && pArea->pDataFile )
   {
      ZH_ULONG ulRecNo, ulRecords;
      ZH_LONG lStep = lEvalStep;

      if( pEvalBlock && ! ZH_IS_BLOCK( pEvalBlock ) )
         pEvalBlock = NULL;

      errCode = SELF_RECCOUNT( &pArea->area, &ulRecords );
      if( errCode == ZH_SUCCESS && ulRecords )
      {
         char szFile[ ZH_PATH_MAX ];
         pArea->ulNewBlockSize = ulBlockSize && pArea->bMemoType != DB_MEMO_DBT
                                 ? ulBlockSize : pArea->ulMemoBlockSize;
         pArea->pMemoTmpFile = zh_fileCreateTemp( NULL, NULL, FC_NORMAL, szFile );
         if( pArea->pMemoTmpFile )
         {
            ZH_ULONG ulMemoBlockSize = pArea->ulMemoBlockSize;
            PZH_FILE pFile = pArea->pMemoFile;

            pArea->ulMemoBlockSize = pArea->ulNewBlockSize;
            pArea->pMemoFile = pArea->pMemoTmpFile;
            errCode = SELF_CREATEMEMFILE( &pArea->area, NULL );
            pArea->pMemoFile = pFile;
            pArea->ulMemoBlockSize = ulMemoBlockSize;
            if( errCode == ZH_SUCCESS )
            {
               if( pEvalBlock )
               {
                  SELF_GOTO( &pArea->area, 0 );
                  pArea->area.fEof = ZH_FALSE;
                  zh_vmEvalBlock( pEvalBlock );
               }

               for( ulRecNo = 1; ulRecNo <= ulRecords; ++ulRecNo )
               {
                  ZH_BOOL fDeleted;

                  errCode = SELF_GOTO( &pArea->area, ulRecNo );
                  if( errCode != ZH_SUCCESS )
                     break;
                  if( pEvalBlock )
                  {
                     if( --lStep <= 0 )
                     {
                        zh_vmEvalBlock( pEvalBlock );
                        lStep = lEvalStep;
                     }
                  }

                  /* read record into bugger */
                  errCode = SELF_DELETED( &pArea->area, &fDeleted );
                  if( errCode != ZH_SUCCESS )
                     break;
                  errCode = zh_fptDoPackRec( pArea );
                  if( errCode != ZH_SUCCESS )
                     break;
                  errCode = SELF_GOCOLD( &pArea->area );
                  if( errCode != ZH_SUCCESS )
                     break;
               }

               if( errCode == ZH_SUCCESS && pEvalBlock )
               {
                  SELF_GOTO( &pArea->area, 0 );
                  pArea->area.fBof = ZH_FALSE;
                  zh_vmEvalBlock( pEvalBlock );
               }
            }
            if( errCode == ZH_SUCCESS )
            {
               ZH_FOFFSET size = zh_fileSize( pArea->pMemoTmpFile );
               ZH_ULONG ulNextBlock;
               ZH_BYTE buffer[ 4 ];

               ulNextBlock = ( ZH_ULONG ) ( ( size + pArea->ulNewBlockSize - 1 ) /
                                            pArea->ulNewBlockSize );
               if( pArea->bMemoType == DB_MEMO_SMT ||
                   pArea->bMemoType == DB_MEMO_DBT )
                  ZH_PUT_LE_UINT32( buffer, ulNextBlock );
               else
                  ZH_PUT_BE_UINT32( buffer, ulNextBlock );
               zh_fileWriteAt( pArea->pMemoTmpFile, buffer, sizeof( buffer ), 0 );
               errCode = zh_fptCopyToFile( pArea->pMemoTmpFile, 0,
                                           pArea->pMemoFile, 0, size );
               zh_fileTruncAt( pArea->pMemoFile, size );
               pArea->ulMemoBlockSize = pArea->ulNewBlockSize;
               if( errCode != ZH_SUCCESS )
               {
                  zh_memoErrorRT( pArea, 0, errCode, errCode == EDBF_READ ?
                                  szFile : pArea->szMemoFileName, 0, 0 );
                  errCode = ZH_FAILURE;
               }
            }
            zh_fileClose( pArea->pMemoTmpFile );
            zh_fileDelete( szFile );
            pArea->pMemoTmpFile = NULL;
         }
      }
   }

   if( errCode != ZH_SUCCESS && errCode != ZH_FAILURE )
   {
      zh_memoErrorRT( pArea, 0, errCode, pArea->szMemoFileName, 0, 0 );
      errCode = ZH_FAILURE;
   }

   return errCode;
}
/*
 * Pack helper function called for each packed record
 * ( DBENTRYP_LSP )   zh_fptPackRec,
 */
static ZH_ERRCODE zh_fptPackRec( FPTAREAP pArea, ZH_ULONG ulRecNo, ZH_BOOL * pfWritten )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptPackRec(%p, %lu, %p)", ( void * ) pArea, ulRecNo, ( void * ) pfWritten ) );

   if( pArea->fPackMemo )
   {
      ZH_ERRCODE errCode = SUPER_PACKREC( &pArea->area, ulRecNo, pfWritten );
      if( errCode == ZH_SUCCESS && *pfWritten )
      {
         errCode = zh_fptDoPackRec( pArea );
         if( errCode != ZH_SUCCESS && errCode != ZH_FAILURE )
         {
            zh_memoErrorRT( pArea, 0, errCode, pArea->szMemoFileName, 0, 0 );
            errCode = ZH_FAILURE;
         }
      }
      return errCode;
   }

   return SUPER_PACKREC( &pArea->area, ulRecNo, pfWritten );
}

/*
 * Remove records marked for deletion from a database.
 * ( DBENTRYP_V )     zh_fptPack,
 */
static ZH_ERRCODE zh_fptPack( FPTAREAP pArea )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptPack(%p)", ( void * ) pArea ) );

   if( ! pArea->fReadonly && ! pArea->fShared &&
       pArea->fHasMemo && pArea->pMemoFile && pArea->pDataFile )
   {
      char szFile[ ZH_PATH_MAX ];

      if( SELF_GOCOLD( &pArea->area ) != ZH_SUCCESS )
         return ZH_FAILURE;

      pArea->pMemoTmpFile = zh_fileCreateTemp( NULL, NULL, FC_NORMAL, szFile );
      if( pArea->pMemoTmpFile )
      {
         ZH_ERRCODE errCode;
         PZH_FILE pFile = pArea->pMemoFile;

         pArea->ulNewBlockSize = pArea->ulMemoBlockSize;

         pArea->pMemoFile = pArea->pMemoTmpFile;
         errCode = SELF_CREATEMEMFILE( &pArea->area, NULL );
         pArea->pMemoFile = pFile;

         if( errCode == ZH_SUCCESS )
         {
            pArea->fPackMemo = ZH_TRUE;
            errCode = SUPER_PACK( &pArea->area );
            pArea->fPackMemo = ZH_FALSE;
            if( errCode == ZH_SUCCESS )
            {
               ZH_FOFFSET size = zh_fileSize( pArea->pMemoTmpFile );
               ZH_ULONG ulNextBlock;
               ZH_BYTE buffer[ 4 ];

               ulNextBlock = ( ZH_ULONG ) ( ( size + pArea->ulNewBlockSize - 1 ) /
                                            pArea->ulNewBlockSize );
               if( pArea->bMemoType == DB_MEMO_SMT ||
                   pArea->bMemoType == DB_MEMO_DBT )
                  ZH_PUT_LE_UINT32( buffer, ulNextBlock );
               else
                  ZH_PUT_BE_UINT32( buffer, ulNextBlock );
               zh_fileWriteAt( pArea->pMemoTmpFile, buffer, sizeof( buffer ), 0 );

               errCode = zh_fptCopyToFile( pArea->pMemoTmpFile, 0,
                                           pArea->pMemoFile, 0, size );
               zh_fileTruncAt( pArea->pMemoFile, size );
               pArea->ulMemoBlockSize = pArea->ulNewBlockSize;
               if( errCode != ZH_SUCCESS )
               {
                  zh_memoErrorRT( pArea, 0, errCode, errCode == EDBF_READ ?
                                  szFile : pArea->szMemoFileName, 0, 0 );
                  errCode = ZH_FAILURE;
               }
            }
         }
         zh_fileClose( pArea->pMemoTmpFile );
         zh_fileDelete( szFile );
         pArea->pMemoTmpFile = NULL;
         return errCode;
      }
   }

   return SUPER_PACK( &pArea->area );
}

/*
 * Retrieve information about the current driver.
 * ( DBENTRYP_SI )    zh_fptInfo
 */
static ZH_ERRCODE zh_fptInfo( FPTAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptInfo(%p, %hu, %p)", ( void * ) pArea, uiIndex, ( void * ) pItem ) );

   switch( uiIndex )
   {
      case DBI_MEMOEXT:
         if( pArea->fHasMemo && pArea->pMemoFile )
         {
            PZH_FNAME pFileName;

            pFileName = zh_fsFNameSplit( pArea->szMemoFileName );
            zh_itemPutC( pItem, pFileName->szExtension );
            zh_xfree( pFileName );
         }
         else
         {
            LPDBFDATA pData = DBFAREA_DATA( pArea );
            const char * szExt;
            if( pData->szMemoExt[ 0 ] )
               zh_itemPutC( pItem, pData->szMemoExt );
            else if( pArea->bMemoType == DB_MEMO_FPT &&
                     ( szExt = zh_setGetMFileExt() ) != NULL && *szExt )
               zh_itemPutC( pItem, szExt );
            else
            {
               szExt = zh_memoDefaultFileExt( pArea->bMemoType, pArea->area.rddID );
               if( ! szExt )
                  szExt = zh_memoDefaultFileExt( zh_memoDefaultType( SELF_RDDNODE( &pArea->area ), 0 ),
                                                 pArea->area.rddID );
               zh_itemPutC( pItem, szExt );
            }
         }
         break;

      case DBI_MEMOBLOCKSIZE:
         if( pArea->fHasMemo && pArea->pMemoFile )
            zh_itemPutNL( pItem, pArea->ulMemoBlockSize );
         else if( pArea->bMemoType && pArea->ulMemoBlockSize )
            zh_itemPutNL( pItem, pArea->ulMemoBlockSize );
         else if( pArea->bMemoType == DB_MEMO_DBT )
            zh_itemPutNI( pItem, DBT_DEFBLOCKSIZE );
         else
         {
            zh_itemClear( pItem );
            return SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_MEMOBLOCKSIZE, 0, pItem );
         }
         break;

      case DBI_MEMOTYPE:
         if( pArea->fHasMemo && pArea->pMemoFile )
            zh_itemPutNI( pItem, pArea->bMemoType );
         else if( pArea->bMemoType )
            zh_itemPutNI( pItem, pArea->bMemoType );
         else
         {
            zh_itemClear( pItem );
            return SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_MEMOTYPE, 0, pItem );
         }
         break;

      case DBI_MEMOVERSION:
         if( pArea->fHasMemo && pArea->pMemoFile )
            zh_itemPutNI( pItem, pArea->uiMemoVersion );
         else if( pArea->bMemoType != DB_MEMO_NONE &&
                  pArea->uiMemoVersion != 0 )
            zh_itemPutNI( pItem, pArea->uiMemoVersion );
         else
         {
            zh_itemClear( pItem );
            return SELF_RDDINFO( SELF_RDDNODE( &pArea->area ), RDDI_MEMOVERSION, 0, pItem );
         }
         break;

      case DBI_MEMOPACK:
         return zh_fptDoPack( pArea, zh_arrayGetNL( pItem, 1 ),
                                     zh_arrayGetItemPtr( pItem, 2 ),
                                     zh_arrayGetNL( pItem, 3 ) );

      /* case DBI_RDD_VERSION */

      case DBI_BLOB_DIRECT_EXPORT:  /* BLOBDirectExport() { <nPointer>, <cTargetFile>, <kMOde> } */
      {
         ZH_ERRCODE errCode = ZH_FAILURE;

         if( ZH_IS_ARRAY( pItem ) )
         {
            ZH_ULONG ulBlock = zh_arrayGetNL( pItem, 1 );
            const char * szFile = zh_arrayGetCPtr( pItem, 2 );

            if( ulBlock && szFile && *szFile )
               errCode = zh_fptGetVarFile( pArea, ulBlock, szFile,
                                           ( ZH_USHORT ) zh_arrayGetNI( pItem, 3 ),
                                           FPT_DIRECT_TRANS( pArea ) );
         }
         zh_itemPutL( pItem, errCode == ZH_SUCCESS );
         break;
      }
      case DBI_BLOB_DIRECT_GET:     /* BLOBDirectGet() { <nPointer>, <nStart>, <nCount> } */
      {
         /* pItem := { <nPointer>, <nStart>, <nCount> } */
         ZH_ULONG ulBlock, ulStart, ulCount;
         ZH_ERRCODE errCode;

         if( ZH_IS_ARRAY( pItem ) )
         {
            ulBlock = zh_arrayGetNL( pItem, 1 );
            ulStart = zh_arrayGetNL( pItem, 2 );
            if( ulStart )
               --ulStart;
            ulCount = zh_arrayGetNL( pItem, 3 );
         }
         else
         {
            ulBlock = ulStart = ulCount = 0;
         }
         errCode = zh_fptGetMemo( pArea, 0, pItem, NULL, ulBlock, ulStart, ulCount, FPT_DIRECT_TRANS( pArea ) );
         if( errCode != ZH_SUCCESS )
         {
            if( errCode != ZH_FAILURE )
               zh_memoErrorRT( pArea, 0, errCode, pArea->szMemoFileName, 0, 0 );
            return ZH_FAILURE;
         }
         break;
      }
      case DBI_BLOB_DIRECT_IMPORT:  /* BLOBDirectImport() { <nOldPointer>, <cSourceFile> } */
         if( ZH_IS_ARRAY( pItem ) )
            zh_itemPutNInt( pItem, zh_fptPutVarFile( pArea,
                                                     zh_arrayGetNL( pItem, 1 ),
                                                     zh_arrayGetCPtr( pItem, 2 ) ) );
         else
            zh_itemPutNI( pItem, 0 );
         break;

      case DBI_BLOB_DIRECT_PUT:     /* BLOBDirectPut() { <nOldPointer>, <xBlob> } */
      {
         /* pItem := { <nOldPointer>, <xBlob> } */
         ZH_ERRCODE errCode = EDBF_UNSUPPORTED;
         ZH_ULONG ulBlock = 0;

         if( ZH_IS_ARRAY( pItem ) )
         {
            PZH_ITEM pValue = zh_arrayGetItemPtr( pItem, 2 );
            ulBlock = zh_arrayGetNL( pItem, 1 );
            if( pValue )
            {
               if( zh_fptFileLockEx( pArea, ZH_TRUE ) )
               {
                  errCode = zh_fptPutMemo( pArea, 0, pValue, &ulBlock, FPT_DIRECT_TRANS( pArea ) );
                  zh_fptFileUnLockEx( pArea );
               }
               else
                  errCode = EDBF_LOCK;
            }
         }
         zh_itemPutNInt( pItem, ulBlock );
         if( errCode != ZH_SUCCESS )
         {
            if( errCode != ZH_FAILURE )
               zh_memoErrorRT( pArea, 0, errCode, pArea->szMemoFileName, 0, 0 );
            return ZH_FAILURE;
         }
         break;
      }
      case DBI_BLOB_ROOT_GET:       /* BLOBRootGet() */
      {
         ZH_ULONG ulBlock;
         ZH_ERRCODE errCode;

         errCode = zh_fptGetRootBlock( pArea, &ulBlock );
         if( errCode == ZH_SUCCESS )
         {
            errCode = zh_fptGetMemo( pArea, 0, pItem, NULL, ulBlock, 0, 0, FPT_DIRECT_TRANS( pArea ) );
         }
         if( errCode != ZH_SUCCESS )
         {
            if( errCode != ZH_FAILURE )
               zh_memoErrorRT( pArea, 0, errCode, pArea->szMemoFileName, 0, 0 );
            zh_itemClear( pItem );
            return ZH_FAILURE;
         }
         break;
      }
      case DBI_BLOB_ROOT_PUT:       /* BLOBRootPut( <xBlob> ) */
      {
         ZH_ULONG ulBlock;
         ZH_ERRCODE errCode;

         errCode = zh_fptGetRootBlock( pArea, &ulBlock );
         if( errCode == ZH_SUCCESS )
         {
            if( zh_fptFileLockEx( pArea, ZH_TRUE ) )
            {
               errCode = zh_fptPutMemo( pArea, 0, pItem, &ulBlock, FPT_DIRECT_TRANS( pArea ) );
               zh_fptFileUnLockEx( pArea );
               if( errCode == ZH_SUCCESS )
                  errCode = zh_fptPutRootBlock( pArea, ulBlock );
            }
            else
               errCode = EDBF_LOCK;
         }
         if( errCode != ZH_SUCCESS )
         {
            if( errCode != ZH_FAILURE )
               zh_memoErrorRT( pArea, 0, errCode, pArea->szMemoFileName, 0, 0 );
            zh_itemPutL( pItem, ZH_FALSE );
            return ZH_FAILURE;
         }
         zh_itemPutL( pItem, ZH_TRUE );
         break;
      }
      case DBI_BLOB_ROOT_LOCK:      /* BLOBRootLock() */
         zh_itemPutL( pItem, zh_fptRootBlockLock( pArea ) );
         break;

      case DBI_BLOB_ROOT_UNLOCK:    /* BLOBRootUnlock() */
         zh_itemPutL( pItem, zh_fptRootBlockUnLock( pArea ) );
         break;

      case DBI_BLOB_DIRECT_LEN:
      case DBI_BLOB_DIRECT_TYPE:
      case DBI_BLOB_INTEGRITY:
      case DBI_BLOB_OFFSET:
      case DBI_BLOB_RECOVER:
         /* TODO: implement it */
         break;

      default:
         return SUPER_INFO( &pArea->area, uiIndex, pItem );
   }

   return ZH_SUCCESS;
}

/*
 * Retrieve information about a field.
 * ( DBENTRYP_SSI )   zh_fptFieldInfo
 */
static ZH_ERRCODE zh_fptFieldInfo( FPTAREAP pArea, ZH_USHORT uiIndex, ZH_USHORT uiType, PZH_ITEM pItem )
{
   LPFIELD pField;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptFieldInfo(%p, %hu, %hu, %p)", ( void * ) pArea, uiIndex, uiType, ( void * ) pItem ) );

   if( ! uiIndex || uiIndex > pArea->area.uiFieldCount )
      return ZH_FAILURE;

   pField = &pArea->area.lpFields[ uiIndex - 1 ];

   if( pArea->fHasMemo && pArea->pMemoFile &&
       ( pField->uiType == ZH_FT_MEMO ||
         pField->uiType == ZH_FT_IMAGE ||
         pField->uiType == ZH_FT_BLOB ||
         pField->uiType == ZH_FT_OLE ) )
   {
      ZH_ULONG ulBlock, ulSize, ulType;
      ZH_BOOL bDeleted;

      SELF_DELETED( &pArea->area, &bDeleted );
      switch( uiType )
      {
         case DBS_BLOB_GET:         /* BLOBGet() { <nStart>, <nCount> } */
         {
            /* pItem := { <nStart>, <nCount> } */
            ZH_ULONG ulStart, ulCount;
            ZH_ERRCODE errCode;
            int iTrans;

            if( ( pField->uiFlags & ZH_FF_UNICODE ) != 0 )
               iTrans = FPT_TRANS_UNICODE;
            else if( ( pField->uiFlags & ZH_FF_BINARY ) == 0 &&
                     zh_vmCodepage() != pArea->area.cdPage )
               iTrans = FPT_TRANS_CP;
            else
               iTrans = FPT_TRANS_NONE;

            if( ZH_IS_ARRAY( pItem ) )
            {
               ulStart = zh_arrayGetNL( pItem, 1 );
               if( ulStart )
                  --ulStart;
               ulCount = zh_arrayGetNL( pItem, 2 );
            }
            else
            {
               ulStart = ulCount = 0;
            }
            if( ulStart || ulCount )
               iTrans = FPT_TRANS_NONE;
            errCode = zh_fptGetMemo( pArea, uiIndex, pItem, NULL, 0, ulStart, ulCount, iTrans );
            if( errCode != ZH_SUCCESS )
            {
               if( errCode != ZH_FAILURE )
                  zh_memoErrorRT( pArea, 0, errCode, pArea->szMemoFileName, 0, 0 );
               return ZH_FAILURE;
            }
            return ZH_SUCCESS;
         }
         case DBS_BLOB_LEN:
            zh_itemPutNL( pItem, zh_fptGetMemoLen( pArea, uiIndex ) );
            return ZH_SUCCESS;
         case DBS_BLOB_OFFSET:
            zh_dbfGetMemoData( ( DBFAREAP ) pArea, uiIndex - 1,
                               &ulBlock, &ulSize, &ulType );
            zh_itemPutNInt( pItem, ( ZH_FOFFSET ) ulBlock * pArea->ulMemoBlockSize +
                                   ( pArea->bMemoType == DB_MEMO_FPT ? sizeof( FPTBLOCK ) : 0 ) );
            return ZH_SUCCESS;
         case DBS_BLOB_POINTER:
            zh_dbfGetMemoData( ( DBFAREAP ) pArea, uiIndex - 1,
                               &ulBlock, &ulSize, &ulType );
            zh_itemPutNL( pItem, ulBlock );
            return ZH_SUCCESS;
         case DBS_BLOB_TYPE:
            zh_itemPutC( pItem, zh_fptGetMemoType( pArea, uiIndex ) );
            return ZH_SUCCESS;
      }
   }
   return SUPER_FIELDINFO( &pArea->area, uiIndex, uiType, pItem );
}

/*
 * Retrieve (set) information about RDD
 * ( DBENTRYP_RSLV )   zh_fptRddInfo
 */
static ZH_ERRCODE zh_fptRddInfo( LPRDDNODE pRDD, ZH_USHORT uiIndex, ZH_ULONG ulConnect, PZH_ITEM pItem )
{
   LPDBFDATA pData;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_fptRddInfo(%p, %hu, %lu, %p)", ( void * ) pRDD, uiIndex, ulConnect, ( void * ) pItem ) );

   pData = DBFNODE_DATA( pRDD );

   switch( uiIndex )
   {
      case RDDI_MEMOEXT:
      {
         const char * szExt = zh_itemGetCPtr( pItem );
         char * szNewVal;

         szNewVal = szExt[ 0 ] == '.' && szExt[ 1 ] ? zh_strdup( szExt ) : NULL;

         if( pData->szMemoExt[ 0 ] )
            zh_itemPutC( pItem, pData->szMemoExt );
         else
         {
            int iType = zh_memoDefaultType( pRDD, ulConnect );

            if( iType == DB_MEMO_FPT && pRDD->rddID != s_uiRddIdBLOB &&
                ( szExt = zh_setGetMFileExt() ) != NULL && *szExt )
               zh_itemPutC( pItem, szExt );
            else
               zh_itemPutC( pItem, zh_memoDefaultFileExt( iType ?
                                   iType : DB_MEMO_FPT, pRDD->rddID ) );
         }
         if( szNewVal )
         {
            zh_strncpy( pData->szMemoExt, szNewVal, sizeof( pData->szMemoExt ) - 1 );
            zh_xfree( szNewVal );
         }
         break;
      }
      case RDDI_MEMOBLOCKSIZE:
      {
         int iSize = zh_itemGetNI( pItem ), iOldSize;

         if( pData->ulMemoBlockSize )
            zh_itemPutNL( pItem, pData->ulMemoBlockSize );
         else if( ( iOldSize = zh_setGetMBlockSize() ) > 0 &&
                  ( ( iOldSize <= 0x10000 ) || ( iOldSize & 0xFFFF ) == 0 ) )
            zh_itemPutNI( pItem, iOldSize );
         else
         {
            switch( zh_memoDefaultType( pRDD, ulConnect ) )
            {
               case DB_MEMO_DBT:
                  zh_itemPutNI( pItem, DBT_DEFBLOCKSIZE );
                  break;
               case DB_MEMO_SMT:
                  zh_itemPutNI( pItem, SMT_DEFBLOCKSIZE );
                  break;
               default:
                  zh_itemPutNI( pItem, FPT_DEFBLOCKSIZE );
                  break;
            }
         }
         if( iSize > 0 && ( iSize <= 0x10000 || ( iSize & 0xFFFF ) == 0 ) )
            pData->ulMemoBlockSize = iSize;
         break;
      }
      case RDDI_MEMOTYPE:
      {
         int iType = zh_itemGetNI( pItem );

         zh_itemPutNI( pItem, pData->bMemoType ? pData->bMemoType : DB_MEMO_FPT );

         if( pRDD->rddID != s_uiRddIdBLOB )
         {
            switch( iType )
            {
               case DB_MEMO_DBT:
               case DB_MEMO_FPT:
               case DB_MEMO_SMT:
                  pData->bMemoType = ( ZH_BYTE ) iType;
            }
         }
         break;
      }

      case RDDI_MEMOVERSION:
      {
         int iType = zh_itemGetNI( pItem );

         zh_itemPutNI( pItem, pData->bMemoExtType ? pData->bMemoExtType : DB_MEMOVER_FLEX );
         switch( iType )
         {
            case DB_MEMOVER_STD:
            case DB_MEMOVER_SIX:
            case DB_MEMOVER_FLEX:
            case DB_MEMOVER_CLIP:
               pData->bMemoExtType = ( ZH_BYTE ) iType;
         }
         break;
      }

      case RDDI_MEMOGCTYPE:
         zh_itemPutNI( pItem, 0 );
         break;

      case RDDI_MEMOREADLOCK:
#if defined( ZH_MEMO_SAFELOCK )
         zh_itemPutL( pItem, pRDD->rddID != s_uiRddIdBLOB );
#else
         zh_itemPutL( pItem, ZH_FALSE );
#endif
         break;
      case RDDI_MEMOREUSE:
         zh_itemPutL( pItem, ZH_TRUE );
         break;

      case RDDI_BLOB_SUPPORT:
         zh_itemPutL( pItem, pRDD->rddID == s_uiRddIdBLOB );
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );
   }

   return ZH_SUCCESS;
}


static const RDDFUNCS fptTable =
{

   /* Movement and positioning methods */

   ( DBENTRYP_BP )    NULL,   /* zh_fptBof */
   ( DBENTRYP_BP )    NULL,   /* zh_fptEof */
   ( DBENTRYP_BP )    NULL,   /* zh_fptFound */
   ( DBENTRYP_V )     NULL,   /* zh_fptGoBottom */
   ( DBENTRYP_UL )    NULL,   /* zh_fptGoTo */
   ( DBENTRYP_I )     NULL,   /* zh_fptGoToId */
   ( DBENTRYP_V )     NULL,   /* zh_fptGoTop */
   ( DBENTRYP_BIB )   NULL,   /* zh_fptSeek */
   ( DBENTRYP_L )     NULL,   /* zh_fptSkip */
   ( DBENTRYP_L )     NULL,   /* zh_fptSkipFilter */
   ( DBENTRYP_L )     NULL,   /* zh_fptSkipRaw */


   /* Data management */

   ( DBENTRYP_VF )    NULL,   /* zh_fptAddField */
   ( DBENTRYP_B )     NULL,   /* zh_fptAppend */
   ( DBENTRYP_I )     NULL,   /* zh_fptCreateFields */
   ( DBENTRYP_V )     NULL,   /* zh_fptDeleteRec */
   ( DBENTRYP_BP )    NULL,   /* zh_fptDeleted */
   ( DBENTRYP_SP )    NULL,   /* zh_fptFieldCount */
   ( DBENTRYP_VF )    NULL,   /* zh_fptFieldDisplay */
   ( DBENTRYP_SSI )   zh_fptFieldInfo,
   ( DBENTRYP_SCP )   NULL,   /* zh_fptFieldName */
   ( DBENTRYP_V )     NULL,   /* zh_fptFlush */
   ( DBENTRYP_PP )    NULL,   /* zh_fptGetRec */
   ( DBENTRYP_SI )    zh_fptGetValue,
   ( DBENTRYP_SVL )   zh_fptGetVarLen,
   ( DBENTRYP_V )     NULL,   /* zh_fptGoCold */
   ( DBENTRYP_V )     NULL,   /* zh_fptGoHot */
   ( DBENTRYP_P )     NULL,   /* zh_fptPutRec */
   ( DBENTRYP_SI )    zh_fptPutValue,
   ( DBENTRYP_V )     NULL,   /* zh_fptRecall */
   ( DBENTRYP_ULP )   NULL,   /* zh_fptRecCount */
   ( DBENTRYP_ISI )   NULL,   /* zh_fptRecInfo */
   ( DBENTRYP_ULP )   NULL,   /* zh_fptRecNo */
   ( DBENTRYP_I )     NULL,   /* zh_fptRecId */
   ( DBENTRYP_S )     NULL,   /* zh_fptSetFieldExtent */


   /* WorkArea/Database management */

   ( DBENTRYP_CP )    NULL,   /* zh_fptAlias */
   ( DBENTRYP_V )     NULL,   /* zh_fptClose */
   ( DBENTRYP_VO )    NULL,   /* zh_fptCreate */
   ( DBENTRYP_SI )    zh_fptInfo,
   ( DBENTRYP_V )     NULL,   /* zh_fptNewArea */
   ( DBENTRYP_VO )    NULL,   /* zh_fptOpen */
   ( DBENTRYP_V )     NULL,   /* zh_fptRelease */
   ( DBENTRYP_SP )    zh_fptStructSize,
   ( DBENTRYP_CP )    NULL,   /* zh_fptSysName */
   ( DBENTRYP_VEI )   NULL,   /* zh_fptEval */
   ( DBENTRYP_V )     zh_fptPack,
   ( DBENTRYP_LSP )   zh_fptPackRec,
   ( DBENTRYP_VS )    NULL,   /* zh_fptSort */
   ( DBENTRYP_VT )    NULL,   /* zh_fptTrans */
   ( DBENTRYP_VT )    NULL,   /* zh_fptTransRec */
   ( DBENTRYP_V )     NULL,   /* zh_fptZap */


   /* Relational Methods */

   ( DBENTRYP_VR )    NULL,   /* zh_fptChildEnd */
   ( DBENTRYP_VR )    NULL,   /* zh_fptChildStart */
   ( DBENTRYP_VR )    NULL,   /* zh_fptChildSync */
   ( DBENTRYP_V )     NULL,   /* zh_fptSyncChildren */
   ( DBENTRYP_V )     NULL,   /* zh_fptClearRel */
   ( DBENTRYP_V )     NULL,   /* zh_fptForceRel */
   ( DBENTRYP_SSP )   NULL,   /* zh_fptRelArea */
   ( DBENTRYP_VR )    NULL,   /* zh_fptRelEval */
   ( DBENTRYP_SI )    NULL,   /* zh_fptRelText */
   ( DBENTRYP_VR )    NULL,   /* zh_fptSetRel */


   /* Order Management */

   ( DBENTRYP_VOI )   NULL,   /* zh_fptOrderListAdd */
   ( DBENTRYP_V )     NULL,   /* zh_fptOrderListClear */
   ( DBENTRYP_VOI )   NULL,   /* zh_fptOrderListDelete */
   ( DBENTRYP_VOI )   NULL,   /* zh_fptOrderListFocus */
   ( DBENTRYP_V )     NULL,   /* zh_fptOrderListRebuild */
   ( DBENTRYP_VOO )   NULL,   /* zh_fptOrderCondition */
   ( DBENTRYP_VOC )   NULL,   /* zh_fptOrderCreate */
   ( DBENTRYP_VOI )   NULL,   /* zh_fptOrderDestroy */
   ( DBENTRYP_SVOI )  NULL,   /* zh_fptOrderInfo */


   /* Filters and Scope Settings */

   ( DBENTRYP_V )     NULL,   /* zh_fptClearFilter */
   ( DBENTRYP_V )     NULL,   /* zh_fptClearLocate */
   ( DBENTRYP_V )     NULL,   /* zh_fptClearScope */
   ( DBENTRYP_VPLP )  NULL,   /* zh_fptCountScope */
   ( DBENTRYP_I )     NULL,   /* zh_fptFilterText */
   ( DBENTRYP_SI )    NULL,   /* zh_fptScopeInfo */
   ( DBENTRYP_VFI )   NULL,   /* zh_fptSetFilter */
   ( DBENTRYP_VLO )   NULL,   /* zh_fptSetLocate */
   ( DBENTRYP_VOS )   NULL,   /* zh_fptSetScope */
   ( DBENTRYP_VPL )   NULL,   /* zh_fptSkipScope */
   ( DBENTRYP_B )     NULL,   /* zh_fptLocate */


   /* Miscellaneous */

   ( DBENTRYP_CC )    NULL,   /* zh_fptCompile */
   ( DBENTRYP_I )     NULL,   /* zh_fptError */
   ( DBENTRYP_I )     NULL,   /* zh_fptEvalBlock */


   /* Network operations */

   ( DBENTRYP_VSP )   NULL,   /* zh_fptRawLock */
   ( DBENTRYP_VL )    NULL,   /* zh_fptLock */
   ( DBENTRYP_I )     NULL,   /* zh_fptUnLock */


   /* Memofile functions */

   ( DBENTRYP_V )     NULL,   /* zh_fptCloseMemFile */
   ( DBENTRYP_VO )    zh_fptCreateMemFile,
   ( DBENTRYP_SCCS )  zh_fptGetValueFile,
   ( DBENTRYP_VO )    zh_fptOpenMemFile,
   ( DBENTRYP_SCCS )  zh_fptPutValueFile,


   /* Database file header handling */

   ( DBENTRYP_V )     NULL,   /* zh_fptReadDBHeader */
   ( DBENTRYP_V )     NULL,   /* zh_fptWriteDBHeader */


   /* non WorkArea functions       */

   ( DBENTRYP_R )     NULL,   /* zh_fptInit */
   ( DBENTRYP_R )     NULL,   /* zh_fptExit */
   ( DBENTRYP_RVVL )  NULL,   /* zh_fptDrop */
   ( DBENTRYP_RVVL )  NULL,   /* zh_fptExists */
   ( DBENTRYP_RVVVL ) NULL,   /* zh_fptRename */
   ( DBENTRYP_RSLV )  zh_fptRddInfo,


   /* Special and reserved methods */

   ( DBENTRYP_SVP )   NULL    /* zh_fptWhoCares */
};

ZH_FUNC_TRANSLATE( DBFFPT, _DBF )
ZH_FUNC( DBFDBT ) { ; }
ZH_FUNC( DBFSMT ) { ; }
ZH_FUNC( DBFBLOB ) { ; }

static void zh_dbffptRegisterRDD( ZH_USHORT * pusRddId )
{
   RDDFUNCS * pTable;
   ZH_USHORT * puiCount, uiRddId, * puiSuperRddId;

   puiCount = ( ZH_USHORT * ) zh_parptr( 1 );
   pTable = ( RDDFUNCS * ) zh_parptr( 2 );
   uiRddId = ( ZH_USHORT ) zh_parni( 4 );
   puiSuperRddId = ( ZH_USHORT * ) zh_parptr( 5 );

   ZH_TRACE( ZH_TR_DEBUG, ( "DBFFPT_GETFUNCTABLE(%p, %p)", ( void * ) puiCount, ( void * ) pTable ) );

   if( pTable )
   {
      ZH_ERRCODE errCode;

      if( puiCount )
         *puiCount = RDDFUNCSCOUNT;

      errCode = zh_rddInheritEx( pTable, &fptTable, &fptSuper, "DBF", puiSuperRddId );
      if( errCode == ZH_SUCCESS )
         *pusRddId = uiRddId;
      zh_retni( errCode );
   }
   else
      zh_retni( ZH_FAILURE );
}

ZH_FUNC_STATIC( DBFFPT_GETFUNCTABLE )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "DBFFPT_GETFUNCTABLE()" ) );

   zh_dbffptRegisterRDD( &s_uiRddIdFPT );
}

ZH_FUNC_STATIC( DBFBLOB_GETFUNCTABLE )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "DBFBLOB_GETFUNCTABLE()" ) );

   zh_dbffptRegisterRDD( &s_uiRddIdBLOB );
}

static void zh_dbffptRddInit( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( zh_rddRegister( "DBF",     RDD_REGISTER_TYPE_FULL ) > 1 ||
       zh_rddRegister( "DBFFPT",  RDD_REGISTER_TYPE_FULL ) > 1 ||
       zh_rddRegister( "DBFBLOB", RDD_REGISTER_TYPE_FULL ) > 1 )
   {
      zh_errInternal( ZH_EI_RDDINVALID, NULL, NULL, NULL );
   }
}

ZH_INIT_SYMBOLS_BEGIN( dbffpt1__InitSymbols )
{ "DBFFPT",               {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( DBFFPT )},               NULL },
{ "DBFFPT_GETFUNCTABLE",  {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( DBFFPT_GETFUNCTABLE )},  NULL },
{ "DBFBLOB",              {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( DBFBLOB )},              NULL },
{ "DBFBLOB_GETFUNCTABLE", {ZH_FS_PUBLIC|ZH_FS_LOCAL}, {ZH_FUNCNAME( DBFBLOB_GETFUNCTABLE )}, NULL }
ZH_INIT_SYMBOLS_END( dbffpt1__InitSymbols )

ZH_CALL_ON_STARTUP_BEGIN( _zh_dbffpt_rdd_init_ )
   zh_vmAtInit( zh_dbffptRddInit, NULL );
ZH_CALL_ON_STARTUP_END( _zh_dbffpt_rdd_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup dbffpt1__InitSymbols
   #pragma startup _zh_dbffpt_rdd_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    ZH_DATASEG_FUNC( dbffpt1__InitSymbols ) \
                              ZH_DATASEG_FUNC( _zh_dbffpt_rdd_init_ )
   #include "zh_ini_seg.h"
#endif
