/*
 * Ziher common hash table implementation
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

#ifndef ZH_HASH_H_
#define ZH_HASH_H_

#include "zh_api.h"

ZH_EXTERN_BEGIN

struct ZH_HASH_TABLE_;

#define ZH_HASH_FUNC( zhfunc )   ZH_SIZE zhfunc( struct ZH_HASH_TABLE_ * HashPtr, const void * Value, const void * Cargo )
typedef ZH_HASH_FUNC( ( * PZH_HASH_FUNC ) );

typedef struct ZH_HASH_ITEM_
{
   const void * ValPtr;          /* value stored in the hash table */
   const void * KeyPtr;
   ZH_SIZE key;
   struct ZH_HASH_ITEM_ *next;
} ZH_HASH_ITEM, * PZH_HASH_ITEM;

typedef struct ZH_HASH_TABLE_
{
   PZH_HASH_ITEM * pItems;        /* pointer to items */
   ZH_SIZE nTableSize;            /* the table size - number of slots */
   ZH_SIZE nCount;                /* number of items stored in the table */
   ZH_SIZE nUsed;                 /* number of used slots */
   PZH_HASH_FUNC pKeyFunc;        /* pointer to func that returns key value */
   PZH_HASH_FUNC pDeleteItemFunc; /* ptr to func that deletes value stored in the table */
   PZH_HASH_FUNC pCompFunc;       /* ptr to func that compares two items */
} ZH_HASH_TABLE, * PZH_HASH_TABLE;


extern ZH_EXPORT_INT PZH_HASH_TABLE zh_hashTableCreate( ZH_SIZE nSize,
                                                        PZH_HASH_FUNC pHashFunc,
                                                        PZH_HASH_FUNC pDelete,
                                                        PZH_HASH_FUNC pComp );
extern ZH_EXPORT_INT void zh_hashTableKill( PZH_HASH_TABLE pTable ); /* release all items and the hash table */
extern ZH_EXPORT_INT ZH_BOOL zh_hashTableAdd( PZH_HASH_TABLE pTable, const void * pKey, const void * pValue ); /* add a new item into the table */
extern ZH_EXPORT_INT ZH_BOOL zh_hashTableDel( PZH_HASH_TABLE pTable, const void * pKey ); /* delete an item from the table */
extern ZH_EXPORT_INT const void * zh_hashTableFind( PZH_HASH_TABLE pTable, const void * pKey ); /* return the pointer to item's value or NULL if not found */
extern ZH_EXPORT_INT PZH_HASH_TABLE zh_hashTableResize( PZH_HASH_TABLE pTable, ZH_SIZE nNewSize ); /* resize the hash table */
extern ZH_EXPORT_INT ZH_SIZE zh_hashTableSize( PZH_HASH_TABLE pTable ); /* return the hash table size */

ZH_EXTERN_END

#endif /* ZH_HASH_H_ */
