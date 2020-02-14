/*
 * Ziher simple hash table implementation
 *
 * Copyright 1999-2002 Ryszard Glab <rglab@imid.med.pl>
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
#include "zh_hash.h"

static PZH_HASH_ITEM zh_hashItemNew( ZH_SIZE nKey, const void * pKey, const void * pValue )
{
   PZH_HASH_ITEM pItem = ( PZH_HASH_ITEM ) zh_xgrab( sizeof( ZH_HASH_ITEM ) );

   pItem->key = nKey;
   pItem->KeyPtr = pKey;
   pItem->ValPtr = pValue;
   pItem->next = NULL;

   return pItem;
}

static void zh_hashItemDelete( PZH_HASH_TABLE pTable, PZH_HASH_ITEM pItem )
{
   if( pTable->pDeleteItemFunc )
      ( pTable->pDeleteItemFunc )( pTable, pItem->KeyPtr, pItem->ValPtr );
   zh_xfree( pItem );
}

/* create a new  hash table
 * nSize = initial number of items in the table
 * pHashTable = a function that calculates a hash key value
 *       (first parameter is a value to add)
 * pDelete = a function that clears item's value before item's releasing
 *       (first parameter is a value to clear)
 * pComp = a function for comparing a values
 *       (first and second are values to compare, function have to return
 *        zero if values match or nonzero if they don't match)
 */
PZH_HASH_TABLE zh_hashTableCreate( ZH_SIZE nSize,
                                   PZH_HASH_FUNC pHashFunc,
                                   PZH_HASH_FUNC pDelete,
                                   PZH_HASH_FUNC pComp )
{
   PZH_HASH_TABLE pTable = ( PZH_HASH_TABLE ) zh_xgrab( sizeof( ZH_HASH_TABLE ) );

   pTable->nTableSize = nSize;
   pTable->pKeyFunc = pHashFunc;
   pTable->pDeleteItemFunc = pDelete;
   pTable->pCompFunc = pComp;
   pTable->nCount = pTable->nUsed = 0;

   pTable->pItems = ( PZH_HASH_ITEM * ) zh_xgrabz( sizeof( PZH_HASH_ITEM ) * nSize );

   return pTable;
}

/* Delete all items in the hash table and next delete the table
 */
void zh_hashTableKill( PZH_HASH_TABLE pTable )
{
   ZH_SIZE nSize = 0;

   while( nSize < pTable->nTableSize )
   {
      if( pTable->pItems[ nSize ] )
      {
         PZH_HASH_ITEM pItem = pTable->pItems[ nSize ];
         while( pItem )
         {
            PZH_HASH_ITEM pFree = pItem;
            pItem = pItem->next;
            zh_hashItemDelete( pTable, pFree );
         }
      }
      ++nSize;
   }
   zh_xfree( pTable->pItems );
   zh_xfree( pTable );
}

/* resize table */
PZH_HASH_TABLE zh_hashTableResize( PZH_HASH_TABLE pTable, ZH_SIZE nNewSize )
{
   PZH_HASH_TABLE pNew;
   ZH_SIZE nSize = 0;

   if( nNewSize == 0 )
      nNewSize = 2 * pTable->nTableSize + 1;
   pNew = zh_hashTableCreate( nNewSize,
                              pTable->pKeyFunc,
                              pTable->pDeleteItemFunc,
                              pTable->pCompFunc );

   while( nSize < pTable->nTableSize )
   {
      if( pTable->pItems[ nSize ] )
      {
         PZH_HASH_ITEM pItem;

         pItem = pTable->pItems[ nSize ];
         while( pItem )
         {
            ZH_SIZE nKey;
            PZH_HASH_ITEM pNewItem, pNext;

            pNext = pItem->next;
            nKey = ( pTable->pKeyFunc )( pNew, pItem->KeyPtr, pItem->ValPtr );
            pNewItem = pNew->pItems[ nKey ];
            if( pNewItem )
            {
               while( pNewItem->next )
                  pNewItem = pNewItem->next;
               pNewItem->next = pItem;
            }
            else
            {
               pNew->pItems[ nKey ] = pItem;
               ++pNew->nUsed;
            }
            pItem->key = nKey;
            pItem->next = NULL;
            ++pNew->nCount;
            pItem = pNext;
         }
      }
      ++nSize;
   }
   zh_xfree( pTable->pItems );
   zh_xfree( pTable );

   return pNew;
}

/* add a new value into the hash table */
ZH_BOOL zh_hashTableAdd( PZH_HASH_TABLE pTable, const void * pKey, const void * pValue )
{
   ZH_SIZE nKey;
   PZH_HASH_ITEM pItem;

   nKey = ( pTable->pKeyFunc )( pTable, pKey, pValue );
   pItem = pTable->pItems[ nKey ];
   if( pItem )
   {
      while( pItem->next )
         pItem = pItem->next;
      pItem->next = zh_hashItemNew( nKey, pKey, pValue );
   }
   else
   {
      pTable->pItems[ nKey ] = zh_hashItemNew( nKey, pKey, pValue );
      ++pTable->nUsed;
   }
   ++pTable->nCount;

   return ZH_TRUE;
}

/* return the pointer to item's value or NULL if not found
 */
const void * zh_hashTableFind( PZH_HASH_TABLE pTable, const void * pKey )
{
   ZH_SIZE nKey;
   PZH_HASH_ITEM pItem;
   const void * pFound = NULL;

   nKey = ( pTable->pKeyFunc )( pTable, pKey, NULL );
   pItem = pTable->pItems[ nKey ];
   if( pItem )
   {
      while( pItem && ( ( pTable->pCompFunc )( pTable, pItem->KeyPtr, pKey ) != 0 ) )
         pItem = pItem->next;

      if( pItem )
         pFound = pItem->ValPtr;
   }

   return pFound;
}

/* Delete an item from the table
 * Returns ZH_TRUE if item was found and returns ZH_FALSE when passed item
 * is not stored in the table
 */
ZH_BOOL zh_hashTableDel( PZH_HASH_TABLE pTable, const void * pKey )
{
   ZH_SIZE nKey;
   PZH_HASH_ITEM pItem;
   PZH_HASH_ITEM pPrev = NULL;
   ZH_BOOL bFound = ZH_FALSE;

   nKey = ( pTable->pKeyFunc )( pTable, pKey, NULL );
   if( nKey > pTable->nTableSize )
      return ZH_FALSE;

   pItem = pTable->pItems[ nKey ];
   while( pItem && ! bFound )
   {
      if( ( pTable->pCompFunc )( pTable, pItem->KeyPtr, pKey ) == 0 )
      {
         if( pPrev )
         {
            pPrev->next = pItem->next;
         }
         else
         {
            pTable->pItems[ nKey ] = pItem->next;
            if( ! pItem->next )
            {
               --pTable->nUsed;
               pTable->pItems[ nKey ] = NULL;
            }
         }
         --pTable->nCount;
         zh_hashItemDelete( pTable, pItem );
         bFound = ZH_TRUE;
      }
      else
      {
         pPrev = pItem;
         pItem = pItem->next;
      }
   }

   return bFound;
}

/* return the hash table size */
ZH_SIZE zh_hashTableSize( PZH_HASH_TABLE pTable )
{
   return pTable->nTableSize;
}
