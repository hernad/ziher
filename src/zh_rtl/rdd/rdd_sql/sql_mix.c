/*
 * SQL MIX (Memory Index) Database Driver
 *
 * Copyright 2007 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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
#include "zh_init.h"
#include "zh_item_api.h"
#include "zh_rdd_api.h"
#include "zh_api_error.h"
#include "zh_lang_api.h"
#include "zh_date.h"
#include "zh_set.h"
#include "zh_vm.h"
#include "zh_rdd_sql.h"
#include "../rdd_sys.zhh"
#include "../dbf_cdx/zh_dbf_error.h"

#define SUPERTABLE  ( &sqlmixSuper )


#define MIX_KEY( tag, node, index ) \
   ( ( PMIXKEY ) &( ( ZH_BYTE * ) ( node ) )[ ( ( node )->Leaf ? sizeof( MIXNODELEAF ) : sizeof( MIXNODE ) ) + ( index ) * ( tag )->uiTotalLen ] )

#define MIX_COPY_KEYS_INTERNAL( tag, node, dst, src, count ) \
   memmove( ( ( ZH_BYTE * ) ( node ) ) + ( ( node )->Leaf ? sizeof( MIXNODELEAF ) : sizeof( MIXNODE ) ) + ( dst ) * ( tag )->uiTotalLen, \
            ( ( ZH_BYTE * ) ( node ) ) + ( ( node )->Leaf ? sizeof( MIXNODELEAF ) : sizeof( MIXNODE ) ) + ( src ) * ( tag )->uiTotalLen, \
            ( count ) * ( tag )->uiTotalLen )

#define MIX_COPY_KEYS_EXTERNAL( tag, ndst, dst, nsrc, src, count ) \
   memmove( ( ( ZH_BYTE * ) ( ndst ) ) + ( ( ndst )->Leaf ? sizeof( MIXNODELEAF ) : sizeof( MIXNODE ) ) + ( dst ) * ( tag )->uiTotalLen, \
            ( ( ZH_BYTE * ) ( nsrc ) ) + ( ( nsrc )->Leaf ? sizeof( MIXNODELEAF ) : sizeof( MIXNODE ) ) + ( src ) * ( tag )->uiTotalLen, \
            ( count ) * ( tag )->uiTotalLen )

#define MIX_ASSIGN_KEY( tag, node, dst, src ) \
   memmove( ( ( ZH_BYTE * ) ( node ) ) + ( ( node )->Leaf ? sizeof( MIXNODELEAF ) : sizeof( MIXNODE ) ) + ( dst ) * ( tag )->uiTotalLen, \
            ( src ), ( tag )->uiTotalLen )

#define MIX_COPY_CHILDS_INTERNAL( tag, node, dst, src, count ) \
   memmove( &( ( node )->Child[ ( dst ) ] ), &( ( node )->Child[ ( src ) ] ), ( count ) * sizeof( void * ) )

#define MIX_COPY_CHILDS_EXTERNAL( tag, ndst, dst, nsrc, src, count ) \
   memmove( &( ( ndst )->Child[ ( dst ) ] ), &( ( nsrc )->Child[ ( src ) ] ), ( count ) * sizeof( void * ) )


static ZH_USHORT s_uiRddIdSQLMIX = ( ZH_USHORT ) -1;

static RDDFUNCS sqlmixSuper;


/* --- Misc functions --- */


static ZH_ERRCODE sqlmixErrorRT( SQLMIXAREAP pArea, ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, char * filename, ZH_ERRCODE errOsCode, ZH_USHORT uiFlags )
{
   ZH_ERRCODE iRet = ZH_FAILURE;

   if( zh_vmRequestQuery() == 0 )
   {
      PZH_ITEM pError = zh_errNew();
      zh_errPutGenCode( pError, errGenCode );
      zh_errPutSubCode( pError, errSubCode );
      zh_errPutOsCode( pError, errOsCode );
      zh_errPutDescription( pError, zh_langDGetErrorDesc( errGenCode ) );
      if( filename )
         zh_errPutFileName( pError, filename );
      if( uiFlags )
         zh_errPutFlags( pError, uiFlags );
      iRet = SELF_ERROR( &pArea->sqlarea.area, pError );
      zh_errRelease( pError );
   }
   return iRet;
}


/* --- Memory Index --- */

/* --- Key management --- */

/* zh_mixKey*() */

static PMIXKEY zh_mixKeyNew( PMIXTAG pTag )
{
   return ( PMIXKEY ) zh_xgrab( pTag->uiTotalLen );
}


static PMIXKEY zh_mixKeyPutItem( PMIXKEY pKey, PZH_ITEM pItem, ZH_ULONG ulRecNo, PMIXTAG pTag )
{
   double  dbl;
   ZH_BYTE buf[ 8 ];

   if( ! pKey )
      pKey = zh_mixKeyNew( pTag );

   pKey->rec    = ulRecNo;
   pKey->notnul = 1;

   /* TODO: check valtype */
   switch( pTag->bType )
   {
      case 'C':
      {
         ZH_SIZE nLen = zh_itemGetCLen( pItem );

         if( nLen > ( ZH_SIZE ) pTag->uiKeyLen )
            nLen = pTag->uiKeyLen;

         memcpy( pKey->val, zh_itemGetCPtr( pItem ), nLen );

         if( nLen < ( ZH_SIZE ) pTag->uiKeyLen )
            memset( pKey->val + nLen, ' ', ( ZH_SIZE ) pTag->uiKeyLen - nLen );

         break;
      }
      case 'N':
         dbl = zh_itemGetND( pItem );
         ZH_DBL2ORD( &dbl, buf );
         memcpy( pKey->val, buf, 8 );
         break;

      case 'D':
         dbl = ( double ) zh_itemGetDL( pItem );
         ZH_DBL2ORD( &dbl, buf );
         memcpy( pKey->val, buf, 8 );
         break;

      case 'L':
         pKey->val[ 0 ] = ( ZH_BYTE ) ( zh_itemGetL( pItem ) ? 'T' : 'F' );
         break;

      default:
         pKey->notnul = 0;
         memset( pKey->val, ' ', pTag->uiKeyLen );
   }
   return pKey;
}


static PMIXKEY zh_mixKeyEval( PMIXKEY pKey, PMIXTAG pTag )
{
   PZH_ITEM     pItem;
   SQLMIXAREAP  pArea     = pTag->pArea;
   int          iCurrArea = zh_rddGetCurrentWorkAreaNumber();
   PZH_CODEPAGE pCodepage = zh_cdpSelect( pArea->sqlarea.area.cdPage );

   if( iCurrArea != pArea->sqlarea.area.uiArea )
      zh_rddSelectWorkAreaNumber( pArea->sqlarea.area.uiArea );
   else
      iCurrArea = 0;

   pItem = zh_vmEvalBlockOrMacro( pTag->pKeyItem );

   pKey = zh_mixKeyPutItem( pKey, pItem, pArea->sqlarea.ulRecNo, pTag );

   if( iCurrArea )
      zh_rddSelectWorkAreaNumber( iCurrArea );

   zh_cdpSelect( pCodepage );

   return pKey;
}


static ZH_BOOL zh_mixEvalCond( SQLMIXAREAP pArea, PZH_ITEM pCondItem )
{
   int     iCurrArea = 0;
   ZH_BOOL fRet;

   if( pArea )
   {
      iCurrArea = zh_rddGetCurrentWorkAreaNumber();

      if( iCurrArea != pArea->sqlarea.area.uiArea )
         zh_rddSelectWorkAreaNumber( pArea->sqlarea.area.uiArea );
      else
         iCurrArea = 0;
   }

   fRet = zh_itemGetL( zh_vmEvalBlockOrMacro( pCondItem ) );

   if( iCurrArea )
      zh_rddSelectWorkAreaNumber( iCurrArea );

   return fRet;
}


static void zh_mixKeyFree( PMIXKEY pKey )
{
   zh_xfree( pKey );
}


static int zh_mixKeyCompare( PMIXTAG pTag, PMIXKEY pKey1, PMIXKEY pKey2, unsigned int uiLen )
{
   unsigned int uiSize;
   int          i;
   PZH_CODEPAGE cdp = zh_cdpFindExt( zh_cdpID() );

   if( ! pKey1->notnul || ! pKey2->notnul )
      return ( int ) pKey1->notnul - ( int ) pKey2->notnul;

   i      = 0;
   uiSize = pTag->uiKeyLen > uiLen ? uiLen : pTag->uiKeyLen;

   //if( pTag->pCodepage )
      //i = zh_cdpcmp( ( const char * ) pKey1->val, ( ZH_SIZE ) uiSize, ( const char * ) pKey2->val, ( ZH_SIZE ) uiSize, pTag->pCodepage, 0 );

   if( cdp )
      i = zh_cdpcmp( ( const char * ) pKey1->val, ( ZH_SIZE ) uiSize, ( const char * ) pKey2->val, ( ZH_SIZE ) uiSize, cdp, 0 );
   else if( uiSize > 0 )
      i = memcmp( pKey1->val, pKey2->val, uiSize );

   if( i == 0 )
   {
      if( pKey2->rec == ( ZH_ULONG ) -1 )
      {
         /* This condition seems inverted, but it's ok for seek last */
         if( pTag->uiKeyLen > uiLen )
            i = -1;
      }
      else
      {
         if( pTag->uiKeyLen > uiLen )
            i = 1;
         else if( pTag->uiKeyLen < uiLen )
            i = -1;
      }
   }


   if( i != 0 )
   {
      if( i < 0 )
         return -2;

      return 2;
   }

   if( pKey1->rec < pKey2->rec )
      return -1;
   else if( pKey1->rec > pKey2->rec )
      return 1;

   return 0;
}


/* --- Tag management --- */

/* zh_mixTag*() */

/* This function is used for debugging purposes. Uncomment it, if you need it. */
#if 0
static void zh_mixTagPrintNode( PMIXTAG pTag, PMIXNODE pNode, int iLevel )
{
   unsigned int i;

   if( ! pNode )
      return;

   if( pNode->KeyCount < MIX_NODE_ORDER / 2 && pNode->Parent )
      printf( "!!! Too few keys\n" );

   for( i = 0; i < pNode->KeyCount; i++ )
   {
      if( ! pNode->Leaf )
      {
         if( pNode->Child[ i ]->Parent != pNode )
            printf( "!!! Invalid parent\n" );

         zh_mixTagPrintNode( pTag, pNode->Child[ i ], iLevel + 1 );
      }
      printf( "%*ld %*s\n", iLevel * 10 + 5, MIX_KEY( pTag, pNode, i )->rec, pTag->uiKeyLen,
              MIX_KEY( pTag, pNode, i )->notnul ? ( char * ) MIX_KEY( pTag, pNode, i )->val : "NULL" );
   }

   if( ! pNode->Leaf )
   {
      if( pNode->Child[ pNode->KeyCount ]->Parent != pNode )
         printf( "!!! Invalid parent\n" );

      zh_mixTagPrintNode( pTag, pNode->Child[ pNode->KeyCount ], iLevel + 1 );
   }
}
#endif

static PMIXNODE zh_mixTagCreateNode( PMIXTAG pTag, ZH_BOOL fLeaf )
{
   ZH_SIZE  nSize = ( fLeaf ? sizeof( MIXNODELEAF ) : sizeof( MIXNODE ) ) + MIX_NODE_ORDER * pTag->uiTotalLen;
   PMIXNODE pNode = ( PMIXNODE ) zh_xgrabz( nSize );

   pNode->Leaf = fLeaf ? 1 : 0;

   return pNode;
}


static unsigned int zh_mixTagNodeParentIndex( PMIXNODE pNode )
{
   PMIXNODE     pParent = pNode->Parent;
   unsigned int ui;

   /* Find position in the parent node */
   ui = pParent->KeyCount;
   do
   {
      if( pParent->Child[ ui ] == pNode )
         return ui;
   }
   while( ui-- );

   return ( unsigned int ) -1;
}


static int zh_mixTagFindKey( PMIXTAG pTag, PMIXKEY pKey, unsigned int uiLen, PMIXNODE * ppNode, unsigned int * puiPos, ZH_BOOL fValidKey )
{
   PMIXNODE     pNode;
   unsigned int ui;
   int          i;

   pNode = pTag->Root;

   for( ;; )
   {
      i = -2;

      /* TODO: binary search */
      for( ui = 0; ui < pNode->KeyCount; ui++ )
      {
         i = zh_mixKeyCompare( pTag, MIX_KEY( pTag, pNode, ui ), pKey, uiLen );

         if( i >= 0 )
            break;
      }

      if( i == 0 || pNode->Leaf )
         break;
      else
         pNode = pNode->Child[ ui ];
   }

   if( fValidKey && ui >= pNode->KeyCount )
   {
      /* unsuccessful find always finds position in leaf */

      while( pNode->Parent && pNode->Parent->Child[ pNode->Parent->KeyCount ] == pNode )
         pNode = pNode->Parent;

      if( pNode->Parent )
      {
         for( ui = 0; ui < pNode->Parent->KeyCount; ui++ )
         {
            if( pNode->Parent->Child[ ui ] == pNode )
            {
               pNode = pNode->Parent;
               break;
            }
         }
      }
      else
         ui = pNode->KeyCount + 1;   /* EOF */
   }

   *ppNode = pNode;
   *puiPos = ui;
   return i;
}


static void zh_mixTagSetCurrent( PMIXTAG pTag, PMIXNODE pNode, unsigned int uiPos )
{
   if( uiPos < pNode->KeyCount )
   {
      pTag->CurNode = pNode;
      pTag->CurPos  = uiPos;
      pTag->CurKey  = MIX_KEY( pTag, pNode, uiPos );
      pTag->fEof    = ZH_FALSE;
   }
   else
      pTag->fEof = ZH_TRUE;
}


static ZH_BOOL zh_mixTagRefreshKey( PMIXTAG pTag )
{
   SQLMIXAREAP pArea;

   pArea = pTag->pArea;

   if( pArea->sqlarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->sqlarea.area );

   if( ! pArea->sqlarea.fPositioned )
   {
      pTag->fEof = ZH_TRUE;
      return ZH_FALSE;
   }
   else if( pTag->fEof || pTag->CurKey->rec != pArea->sqlarea.ulRecNo )
   {
      PMIXKEY      pKey;
      PMIXNODE     pNode;
      unsigned int ui;

      pKey = zh_mixKeyEval( NULL, pTag );

      zh_mixTagFindKey( pTag, pKey, pTag->uiKeyLen, &pNode, &ui, ZH_FALSE );
      zh_mixTagSetCurrent( pTag, pNode, ui );

      zh_mixKeyFree( pKey );
      return ! pTag->fEof && pTag->CurKey->rec == pArea->sqlarea.ulRecNo;
   }
   pTag->fBof = pTag->fEof = ZH_FALSE;
   return ZH_TRUE;
}


static void zh_mixTagAddKeyNode( PMIXTAG pTag, PMIXNODE pNode, unsigned int uiPos, PMIXKEY pKey, PMIXNODE pChildLeft, PMIXNODE pChildRight )
{
   MIX_COPY_KEYS_INTERNAL( pTag, pNode, uiPos + 1, uiPos, pNode->KeyCount - uiPos );
   if( ! pNode->Leaf )
   {
      MIX_COPY_CHILDS_INTERNAL( pTag, pNode, uiPos + 2, uiPos + 1, pNode->KeyCount - uiPos );
      pNode->Child[ uiPos ]     = pChildLeft;
      pNode->Child[ uiPos + 1 ] = pChildRight;
      pChildLeft->Parent        = pNode;
      pChildRight->Parent       = pNode;
   }
   MIX_ASSIGN_KEY( pTag, pNode, uiPos, pKey );
   pNode->KeyCount++;
}


static void zh_mixTagAddKeyPos( PMIXTAG pTag, PMIXNODE pNode, unsigned int uiPos, PMIXKEY pKey, PMIXNODE pChildLeft, PMIXNODE pChildRight )
{
   PMIXNODE     pNewNode;
   unsigned int j, k;

   if( pNode->KeyCount < MIX_NODE_ORDER )
   {
      zh_mixTagAddKeyNode( pTag, pNode, uiPos, pKey, pChildLeft, pChildRight );
      return;
   }

#ifdef USE_SIBLINGS
   /* Try use siblings, if leaf node is full */

   if( pNode->Leaf && pNode->Parent )
   {
      j = zh_mixTagNodeParentIndex( pNode );

      if( j > 0 && pNode->Parent->Child[ j - 1 ]->KeyCount < MIX_NODE_ORDER )
      {
         MIX_COPY_KEYS_EXTERNAL( pTag, pNode->Parent->Child[ j - 1 ], pNode->Parent->Child[ j - 1 ]->KeyCount, pNode->Parent, j - 1, 1 );
         pNode->Parent->Child[ j - 1 ]->KeyCount++;

         if( uiPos == 0 )
         {
            MIX_ASSIGN_KEY( pTag, pNode->Parent, j - 1, pKey );
            pNode->Parent->Key[ j - 1 ] = pKey;
         }
         else
         {
            MIX_COPY_KEYS_EXTERNAL( pTag, pNode->Parent, j - 1, pNode, 0, 1 );
            uiPos--;
            MIX_COPY_KEYS_INTERNAL( pTag, pNode, 0, 1, uiPos );
            MIX_ASSIGN_KEY( pTag, pNode, uiPos, pKey );
         }

         return;
      }
      else if( j < pNode->Parent->KeyCount && pNode->Parent->Child[ j + 1 ]->KeyCount < MIX_NODE_ORDER )
      {
         MIX_COPY_KEYS_INTERNAL( pTag, pNode->Parent->Child[ j + 1 ], 1, 0, pNode->Parent->Child[ j + 1 ]->KeyCount );
         MIX_COPY_KEYS_EXTERNAL( pTag, pNode->Parent->Child[ j + 1 ], 0, pNode->Parent, j, 1 );
         pNode->Parent->Child[ j + 1 ]->KeyCount++;

         if( uiPos == MIX_NODE_ORDER )
            MIX_ASSIGN_KEY( pTag, pNode->Parent, j, pKey );
         else
         {
            MIX_COPY_KEYS_EXTERNAL( pTag, pNode->Parent, j, pNode, MIX_NODE_ORDER - 1, 1 );
            MIX_COPY_KEYS_INTERNAL( pTag, pNode, uiPos + 1, uiPos, MIX_NODE_ORDER - uiPos - 1 );
            MIX_ASSIGN_KEY( pTag, pNode, uiPos, pKey );
         }
         return;
      }
   }
#endif /* USE_SIBLINGS */


   /* Create new node */
   pNewNode = zh_mixTagCreateNode( pTag, pNode->Leaf );

   /* Move half of items to new node */
   k = MIX_NODE_ORDER / 2 + ( ( uiPos <= MIX_NODE_ORDER / 2 ) ? 0 : 1 );
   MIX_COPY_KEYS_EXTERNAL( pTag, pNewNode, 0, pNode, k, MIX_NODE_ORDER - k );
   if( ! pNode->Leaf )
   {
      MIX_COPY_CHILDS_EXTERNAL( pTag, pNewNode, 1, pNode, k + 1, MIX_NODE_ORDER - k );
      for( j = 1; j <= MIX_NODE_ORDER - k; j++ )
         pNewNode->Child[ j ]->Parent = pNewNode;  /* Do NOT forget to re-parent */
   }
   pNode->KeyCount    = k;
   pNewNode->KeyCount = MIX_NODE_ORDER - k;


   /* Insert new item to the left node or right node */
   if( uiPos <= MIX_NODE_ORDER / 2 )
      zh_mixTagAddKeyNode( pTag, pNode, uiPos, pKey, pChildLeft, pChildRight );
   else
      zh_mixTagAddKeyNode( pTag, pNewNode, uiPos - MIX_NODE_ORDER / 2 - 1, pKey, pChildLeft, pChildRight );


   /* Assign the leftmost child of the new node */
   if( ! pNode->Leaf )
   {
      pNewNode->Child[ 0 ]         = pNode->Child[ pNode->KeyCount ];
      pNewNode->Child[ 0 ]->Parent = pNewNode;
   }

   pNode->KeyCount--;

   /* Move middle (last+1 in first node) item up */
   if( pNode->Parent )
      zh_mixTagAddKeyPos( pTag, pNode->Parent, zh_mixTagNodeParentIndex( pNode ),
                          MIX_KEY( pTag, pNode, pNode->KeyCount ), pNode, pNewNode );
   else
   {
      pTag->Root = zh_mixTagCreateNode( pTag, 0 );
      zh_mixTagAddKeyNode( pTag, pTag->Root, 0, MIX_KEY( pTag, pNode, pNode->KeyCount ), pNode, pNewNode );
   }

}


static ZH_BOOL zh_mixTagAddKey( PMIXTAG pTag, PMIXKEY pKey )
{
   PMIXNODE     pNode;
   unsigned int ui;
   int          i;

   i = zh_mixTagFindKey( pTag, pKey, pTag->uiKeyLen, &pNode, &ui, ZH_FALSE );

   /* Key cannot be duplicated */
   if( ! i )
      return ZH_FALSE;

   zh_mixTagAddKeyPos( pTag, pNode, ui, pKey, NULL, NULL );
   return ZH_TRUE;
}


static void zh_mixTagDelKeyNode( PMIXTAG pTag, PMIXNODE pNode, unsigned int uiPos )
{
   MIX_COPY_KEYS_INTERNAL( pTag, pNode, uiPos, uiPos + 1, pNode->KeyCount - uiPos - 1 );
   if( ! pNode->Leaf )
   {
      MIX_COPY_CHILDS_INTERNAL( pTag, pNode, uiPos, uiPos + 1, pNode->KeyCount - uiPos );
   }
   pNode->KeyCount--;
}


static void zh_mixTagNodeAdjust( PMIXTAG pTag, PMIXNODE pNode )
{
   unsigned int i, j;
   PMIXNODE     pParent, pSibling;

   for( ;; )
   {
      if( pNode->KeyCount >= MIX_NODE_ORDER / 2 )
         return;

      /* Check siblings */

      if( pNode->Parent )
      {
         pParent = pNode->Parent;
         j       = zh_mixTagNodeParentIndex( pNode );

         if( j > 0 && pParent->Child[ j - 1 ]->KeyCount > MIX_NODE_ORDER / 2 )
         {
            /* Borrow from left */

            pSibling = pParent->Child[ j - 1 ];

            /* It could not be pNode->Child[ 0 ] if it is not Leaf!!! */
            zh_mixTagAddKeyNode( pTag, pNode, 0, MIX_KEY( pTag, pParent, j - 1 ), pSibling->Child[ pSibling->KeyCount ], pNode->Child[ 0 ] );
            MIX_COPY_KEYS_EXTERNAL( pTag, pParent, j - 1, pSibling, pSibling->KeyCount - 1, 1 );
            pSibling->KeyCount--;
            return;
         }
         else if( j < pParent->KeyCount && pParent->Child[ j + 1 ]->KeyCount > MIX_NODE_ORDER / 2 )
         {
            /* Borrow from right */

            pSibling = pParent->Child[ j + 1 ];
            zh_mixTagAddKeyNode( pTag, pNode, pNode->KeyCount, MIX_KEY( pTag, pParent, j ), pNode->Child[ pNode->KeyCount ], pSibling->Child[ 0 ] );
            MIX_COPY_KEYS_EXTERNAL( pTag, pParent, j, pSibling, 0, 1 );
            zh_mixTagDelKeyNode( pTag, pSibling, 0 );
            return;
         }
         else if( j > 0 )
         {
            /* Join with left */

            pSibling = pParent->Child[ j - 1 ];
            MIX_COPY_KEYS_EXTERNAL( pTag, pSibling, pSibling->KeyCount, pParent, j - 1, 1 );
            pSibling->KeyCount++;
            MIX_COPY_KEYS_EXTERNAL( pTag, pSibling, pSibling->KeyCount, pNode, 0, pNode->KeyCount );

            if( pNode->Leaf )
               pSibling->KeyCount += pNode->KeyCount;
            else
            {
               MIX_COPY_CHILDS_EXTERNAL( pTag, pSibling, pSibling->KeyCount, pNode, 0, pNode->KeyCount );
               for( i = 0; i < pNode->KeyCount; i++ )
                  pSibling->Child[ pSibling->KeyCount++ ]->Parent = pSibling;

               pSibling->Child[ pSibling->KeyCount ]         = pNode->Child[ i ];
               pSibling->Child[ pSibling->KeyCount ]->Parent = pSibling;
            }
            zh_xfree( pNode );
            pParent->Child[ j ] = pSibling;
            zh_mixTagDelKeyNode( pTag, pParent, j - 1 );
            pNode = pParent;
         }
         else if( j < pParent->KeyCount )
         {
            /* Join with right */

            pSibling = pParent->Child[ j + 1 ];
            MIX_COPY_KEYS_EXTERNAL( pTag, pNode, pNode->KeyCount, pParent, j, 1 );
            pNode->KeyCount++;
            MIX_COPY_KEYS_EXTERNAL( pTag, pNode, pNode->KeyCount, pSibling, 0, pSibling->KeyCount );
            if( pNode->Leaf )
               pNode->KeyCount += pSibling->KeyCount;
            else
            {
               MIX_COPY_CHILDS_EXTERNAL( pTag, pNode, pNode->KeyCount, pSibling, 0, pSibling->KeyCount );
               for( i = 0; i < pSibling->KeyCount; i++ )
                  pNode->Child[ pNode->KeyCount++ ]->Parent = pNode;

               pNode->Child[ pNode->KeyCount ]         = pSibling->Child[ i ];
               pNode->Child[ pNode->KeyCount ]->Parent = pNode;
            }
            zh_xfree( pSibling );
            pParent->Child[ j + 1 ] = pNode;
            zh_mixTagDelKeyNode( pTag, pParent, j );
            pNode = pParent;
         }
      }
      else
      {
         /* Adjust root */

         if( ! pNode->KeyCount && ! pNode->Leaf )
         {
            pTag->Root         = pNode->Child[ 0 ];
            pTag->Root->Parent = NULL;
            zh_xfree( pNode );
         }
         return;
      }
   }
}


static void zh_mixTagDelKeyPos( PMIXTAG pTag, PMIXNODE pNode, unsigned int uiPos )
{
   if( pNode->Leaf )
   {
      zh_mixTagDelKeyNode( pTag, pNode, uiPos );
      zh_mixTagNodeAdjust( pTag, pNode );
   }
   else
   {
      PMIXNODE pLeaf;

      pLeaf = pNode->Child[ uiPos + 1 ];
      while( ! pLeaf->Leaf )
         pLeaf = pLeaf->Child[ 0 ];

      MIX_COPY_KEYS_EXTERNAL( pTag, pNode, uiPos, pLeaf, 0, 1 );
      zh_mixTagDelKeyNode( pTag, pLeaf, 0 );
      zh_mixTagNodeAdjust( pTag, pLeaf );
   }
}


static ZH_BOOL zh_mixTagDelKey( PMIXTAG pTag, PMIXKEY pKey )
{
   PMIXNODE     pNode;
   unsigned int ui;
   int          i;

   i = zh_mixTagFindKey( pTag, pKey, pTag->uiKeyLen, &pNode, &ui, ZH_FALSE );

   if( i )
      return ZH_FALSE;

   zh_mixTagDelKeyPos( pTag, pNode, ui );
   return ZH_TRUE;
}


static PMIXTAG zh_mixTagCreate( const char * szTagName, PZH_ITEM pKeyExpr, PZH_ITEM pKeyItem, PZH_ITEM pForItem, PZH_ITEM pWhileItem, ZH_BYTE bType, unsigned int uiKeyLen, SQLMIXAREAP pArea )
{
   PMIXTAG pTag;
   PMIXKEY pKey = NULL;
   LPDBORDERCONDINFO pOrdCondInfo = pArea->sqlarea.area.lpdbOrdCondInfo;
   ZH_ULONG          ulStartRec, ulNextCount = 0;
   ZH_LONG  lStep = 0;
   PZH_ITEM pItem, pEvalItem = NULL;

   pTag = ( PMIXTAG ) zh_xgrabz( sizeof( MIXTAG ) );

   pTag->pArea = pArea;

   pTag->szName = ( char * ) zh_xgrab( MIX_MAXTAGNAMELEN + 1 );
   zh_strncpyUpperTrim( pTag->szName, szTagName, MIX_MAXTAGNAMELEN );

   pTag->szKeyExpr = ( char * ) zh_xgrab( zh_itemGetCLen( pKeyExpr ) + 1 );
   zh_strncpyTrim( pTag->szKeyExpr, zh_itemGetCPtr( pKeyExpr ), zh_itemGetCLen( pKeyExpr ) );

   /* TODO: FOR expression */
   pTag->szForExpr = NULL;

   pTag->pKeyItem = pKeyItem;
   pTag->pForItem = pForItem;
   pTag->bType    = bType;
   pTag->uiKeyLen = uiKeyLen;

   pTag->uiTotalLen = sizeof( MIXKEY ) + pTag->uiKeyLen;

   /* Use national support */
   if( bType == 'C' )
   {
      if( pArea->sqlarea.area.cdPage &&
          ! ZH_CODEPAGE_ISBINSORT( pArea->sqlarea.area.cdPage ) )
         pTag->pCodepage = pArea->sqlarea.area.cdPage;
   }

   pTag->Root = zh_mixTagCreateNode( pTag, ZH_TRUE );

   ulStartRec = 0;

   if( pOrdCondInfo )
   {
      pEvalItem = pOrdCondInfo->itmCobEval;
      lStep     = pOrdCondInfo->lStep;
   }

   if( ! pOrdCondInfo || pOrdCondInfo->fAll )
      pArea->pTag = NULL;
   else
   {
      if( pOrdCondInfo->itmRecID )
         ulStartRec = zh_itemGetNL( pOrdCondInfo->itmRecID );

      if( ulStartRec )
         ulNextCount = 1;
      else if( pOrdCondInfo->fRest || pOrdCondInfo->lNextCount > 0 )
      {
         if( pOrdCondInfo->itmStartRecID )
            ulStartRec = zh_itemGetNL( pOrdCondInfo->itmStartRecID );

         if( ! ulStartRec )
            ulStartRec = pArea->sqlarea.ulRecNo;

         if( pArea->sqlarea.area.lpdbOrdCondInfo->lNextCount > 0 )
            ulNextCount = pOrdCondInfo->lNextCount;
      }
      else if( ! pOrdCondInfo->fUseCurrent )
         pArea->pTag = NULL;
   }

   if( ulStartRec == 0 && pArea->pTag == NULL )
      ulStartRec = 1;

   if( ulStartRec )
      SELF_GOTO( &pArea->sqlarea.area, ulStartRec );
   else
      SELF_GOTOP( &pArea->sqlarea.area );

   while( ! pArea->sqlarea.area.fEof )
   {
      if( pEvalItem )
      {
         if( lStep >= pOrdCondInfo->lStep )
         {
            lStep = 0;
            if( ! zh_mixEvalCond( NULL, pEvalItem ) )
               break;
         }
         ++lStep;
      }

      if( pWhileItem && ! zh_mixEvalCond( NULL, pWhileItem ) )
         break;

      if( pForItem == NULL || zh_mixEvalCond( NULL, pForItem ) )
      {
         pItem = zh_vmEvalBlockOrMacro( pKeyItem );

         pKey = zh_mixKeyPutItem( pKey, pItem, pArea->sqlarea.ulRecNo, pTag );
         zh_mixTagAddKey( pTag, pKey );
      }

      if( ulNextCount )
      {
         ulNextCount--;
         if( ! ulNextCount )
            break;
      }
      if( SELF_SKIPRAW( &pArea->sqlarea.area, 1 ) == ZH_FAILURE )
         break;
   }
   if( pKey )
      zh_mixKeyFree( pKey );

   return pTag;
}


static void zh_mixTagDestroyNode( PMIXNODE pNode )
{
   if( ! pNode->Leaf )
   {
      unsigned int ui;

      for( ui = 0; ui <= pNode->KeyCount; ui++ )
         zh_mixTagDestroyNode( pNode->Child[ ui ] );
   }
   zh_xfree( pNode );
}


static void zh_mixTagDestroy( PMIXTAG pTag )
{
   if( pTag->szName )
      zh_xfree( pTag->szName );

   if( pTag->szKeyExpr )
      zh_xfree( pTag->szKeyExpr );

   if( pTag->szForExpr )
      zh_xfree( pTag->szForExpr );

   if( pTag->pKeyItem )
      zh_vmDestroyBlockOrMacro( pTag->pKeyItem );

   if( pTag->pForItem )
      zh_vmDestroyBlockOrMacro( pTag->pForItem );

   if( pTag->Root )
      zh_mixTagDestroyNode( pTag->Root );

   if( pTag->HotKey )
      zh_mixKeyFree( pTag->HotKey );

   zh_xfree( pTag );
}


static void zh_mixTagGoTop( PMIXTAG pTag )
{
   PMIXNODE pNode;

   pNode = pTag->Root;
   while( ! pNode->Leaf )
      pNode = pNode->Child[ 0 ];

   if( ! pNode->KeyCount )
   {
      pTag->fEof = ZH_TRUE;
      return;
   }
   pTag->fEof    = ZH_FALSE;
   pTag->CurNode = pNode;
   pTag->CurPos  = 0;
   pTag->CurKey  = MIX_KEY( pTag, pTag->CurNode, 0 );
}


static void zh_mixTagGoBottom( PMIXTAG pTag )
{
   PMIXNODE pNode;

   pNode = pTag->Root;
   while( ! pNode->Leaf )
      pNode = pNode->Child[ pNode->KeyCount ];

   if( ! pNode->KeyCount )
   {
      pTag->fEof = ZH_TRUE;
      return;
   }
   pTag->fEof    = ZH_FALSE;
   pTag->CurNode = pNode;
   pTag->CurPos  = pNode->KeyCount - 1;
   pTag->CurKey  = MIX_KEY( pTag, pTag->CurNode, pTag->CurPos );
}


static void zh_mixTagSkip( PMIXTAG pTag, ZH_LONG lSkip )
{
   PMIXNODE     pNode, pNode2;
   unsigned int uiPos, uiPos2;

   pNode = pTag->CurNode;
   uiPos = pTag->CurPos;

   #if 0
   printf( "zh_mixTagSkip: CurNode=%p, CurPos=%d lSkip=%d\n", pNode, uiPos, lSkip );
   #endif

   if( lSkip > 0 )
   {
      pTag->fBof = ZH_FALSE;
      while( ! pTag->fEof && lSkip > 0 )
      {
         if( pNode->Leaf )
         {
            if( ( ZH_LONG ) ( pNode->KeyCount - 1 - uiPos ) >= lSkip )
            {
               uiPos += lSkip;
               lSkip  = 0;
            }
            else if( pNode->KeyCount - 1 > uiPos )
            {
               lSkip -= ( ZH_LONG ) ( pNode->KeyCount - 1 - uiPos );
               uiPos  = pNode->KeyCount - 1;
            }
            if( lSkip )
            {
               do
               {
                  if( pNode->Parent )
                     uiPos = zh_mixTagNodeParentIndex( pNode );
                  pNode = pNode->Parent;
               }
               while( pNode && uiPos == pNode->KeyCount );

               if( pNode )
                  lSkip--;
               else
                  pTag->fEof = ZH_TRUE;
            }
         }
         else
         {
            pNode = pNode->Child[ uiPos + 1 ];
            while( ! pNode->Leaf )
               pNode = pNode->Child[ 0 ];

            uiPos = 0;
            lSkip--;
         }
      }
   }
   else if( lSkip < 0 )
   {
      lSkip = -lSkip;

      /* This is not needed. skip(-1) from Eof is processed inside sqlmixSkipRaw */
#if 0
      if( pTag->fEof )
      {
         zh_mixTagGoBottom( pTag );
         lSkip--;
         pTag->fBof = pTag->fEof;
      }
#endif
      pTag->fBof = pTag->fEof;

      while( ! pTag->fBof && lSkip > 0 )
      {
         if( pNode->Leaf )
         {
            if( ( ZH_LONG ) uiPos >= lSkip )
            {
               uiPos -= lSkip;
               lSkip  = 0;
            }
            else if( uiPos )
            {
               lSkip -= uiPos;
               uiPos  = 0;
            }
            if( lSkip )
            {
               pNode2 = pNode;
               uiPos2 = uiPos;
               do
               {
                  if( pNode->Parent )
                     uiPos = zh_mixTagNodeParentIndex( pNode );
                  pNode = pNode->Parent;
               }
               while( pNode && uiPos == 0 );

               if( pNode )
               {
                  uiPos--;
                  lSkip--;
               }
               else
               {
                  pNode      = pNode2;
                  uiPos      = uiPos2;
                  pTag->fBof = ZH_TRUE;
               }
            }
         }
         else
         {
            do
            {
               pNode = pNode->Child[ uiPos ];
               uiPos = pNode->KeyCount;
            }
            while( ! pNode->Leaf );
            uiPos--;
            lSkip--;
         }
      }
   }
   if( ! pTag->fEof )
   {
      pTag->CurNode = pNode;
      pTag->CurPos  = uiPos;
      pTag->CurKey  = MIX_KEY( pTag, pNode, uiPos );
   }
}

/* --- Misc functions --- */

/* zh_mix*() */

static PMIXTAG zh_mixFindTag( SQLMIXAREAP pArea, PZH_ITEM pOrder )
{
   PMIXTAG pTag;

   if( ZH_IS_NUMBER( pOrder ) )
   {
      int iOrder, iCurr = 0;

      iOrder = zh_itemGetNI( pOrder );

      pTag = pArea->pTagList;
      while( pTag && iOrder != ++iCurr )
         pTag = pTag->pNext;
   }
   else
   {
      char szTag[ MIX_MAXTAGNAMELEN + 1 ];

      zh_strncpyUpperTrim( szTag, zh_itemGetCPtr( pOrder ), MIX_MAXTAGNAMELEN );
      pTag = pArea->pTagList;
      while( pTag && zh_stricmp( szTag, pTag->szName ) )
         pTag = pTag->pNext;
   }
   return pTag;
}


/* --- */


static ZH_ULONG zh_mixTagNodeKeyCount( PMIXNODE pNode )
{
   ZH_ULONG ulKeyCount = pNode->KeyCount;

   if( ! pNode->Leaf )
   {
      unsigned int ui;
      for( ui = 0; ui <= pNode->KeyCount; ui++ )
         ulKeyCount += zh_mixTagNodeKeyCount( pNode->Child[ ui ] );
   }

   return ulKeyCount;
}


static ZH_BOOL zh_mixCheckRecordFilter( SQLMIXAREAP pArea, ZH_ULONG ulRecNo )
{
   ZH_BOOL lResult = ZH_FALSE;

   if( pArea->sqlarea.area.dbfi.itmCobExpr || zh_setGetDeleted() )
   {
      if( pArea->sqlarea.ulRecNo != ulRecNo || pArea->sqlarea.lpdbPendingRel )
         SELF_GOTO( &pArea->sqlarea.area, ulRecNo );

      if( zh_setGetDeleted() )
         SUPER_DELETED( &pArea->sqlarea.area, &lResult );

      if( ! lResult && pArea->sqlarea.area.dbfi.itmCobExpr )
      {
         PZH_ITEM pResult = zh_vmEvalBlock( pArea->sqlarea.area.dbfi.itmCobExpr );
         lResult = ZH_IS_LOGICAL( pResult ) && ! zh_itemGetL( pResult );
      }
   }
   return ! lResult;
}


static ZH_ULONG zh_mixDBOIKeyCount( PMIXTAG pTag, ZH_BOOL fFilter )
{
   ZH_ULONG ulKeyCount;

   if( ! pTag )
      return 0;

   if( fFilter && pTag->pArea->sqlarea.area.dbfi.fFilter )
   {
      PMIXNODE     pNode   = pTag->CurNode;
      unsigned int uiPos   = pTag->CurPos;
      ZH_ULONG     ulRecNo = pTag->pArea->sqlarea.ulRecNo;

      ulKeyCount = 0;

      zh_mixTagGoTop( pTag );
      while( ! pTag->fEof )
      {
         if( zh_mixCheckRecordFilter( pTag->pArea, pTag->CurKey->rec ) )
            ulKeyCount++;
         zh_mixTagSkip( pTag, 1 );
      }
      zh_mixTagSetCurrent( pTag, pNode, uiPos );
      SELF_GOTO( &pTag->pArea->sqlarea.area, ulRecNo );

   }
   else
      ulKeyCount = zh_mixTagNodeKeyCount( pTag->Root );

   return ulKeyCount;
}


static ZH_ULONG zh_mixDBOIKeyNo( PMIXTAG pTag, ZH_BOOL fFilter )
{
   ZH_ULONG ulKeyCount;

   if( ! pTag )
      return 0;

   if( fFilter )
      ulKeyCount = 0;
   else
   {
      PMIXNODE     pNode = pTag->CurNode;
      unsigned int ui, uiPos = pTag->CurPos;

      ulKeyCount = 1;

      while( pNode )
      {
         ulKeyCount += uiPos;
         if( ! pNode->Leaf )
         {
            for( ui = 0; ui < uiPos; ui++ )
               ulKeyCount += zh_mixTagNodeKeyCount( pNode->Child[ ui ] );
         }
         pNode = pNode->Parent;
         if( pNode )
            uiPos = zh_mixTagNodeParentIndex( pNode );
      }
   }
   return ulKeyCount;
}


/* --- SQLMIX RDD METHODS --- */

static ZH_ERRCODE sqlmixGoBottom( SQLMIXAREAP pArea )
{
   ZH_ERRCODE retval;

   if( SELF_GOCOLD( &pArea->sqlarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->pTag )
      return SUPER_GOBOTTOM( &pArea->sqlarea.area );

   if( pArea->sqlarea.lpdbPendingRel && pArea->sqlarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->sqlarea.area );

   zh_mixTagGoBottom( pArea->pTag );

   pArea->sqlarea.area.fTop    = ZH_FALSE;
   pArea->sqlarea.area.fBottom = ZH_TRUE;

   retval = SELF_GOTO( &pArea->sqlarea.area, pArea->pTag->CurKey ? pArea->pTag->CurKey->rec : 0 );
   if( retval != ZH_FAILURE && pArea->sqlarea.fPositioned )
      retval = SELF_SKIPFILTER( &pArea->sqlarea.area, -1 );

   return retval;
}


static ZH_ERRCODE sqlmixGoTop( SQLMIXAREAP pArea )
{
   ZH_ERRCODE retval;

   if( SELF_GOCOLD( &pArea->sqlarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->pTag )
      return SUPER_GOTOP( &pArea->sqlarea.area );

   if( pArea->sqlarea.lpdbPendingRel && pArea->sqlarea.lpdbPendingRel->isScoped )
      SELF_FORCEREL( &pArea->sqlarea.area );

   zh_mixTagGoTop( pArea->pTag );

   pArea->sqlarea.area.fTop    = ZH_TRUE;
   pArea->sqlarea.area.fBottom = ZH_FALSE;

   retval = SELF_GOTO( &pArea->sqlarea.area, pArea->pTag->CurKey ? pArea->pTag->CurKey->rec : 0 );
   if( retval != ZH_FAILURE && pArea->sqlarea.fPositioned )
      retval = SELF_SKIPFILTER( &pArea->sqlarea.area, 1 );

   return retval;
}


static ZH_ERRCODE sqlmixSeek( SQLMIXAREAP pArea, ZH_BOOL fSoftSeek, PZH_ITEM pItem, ZH_BOOL fFindLast )
{
   if( SELF_GOCOLD( &pArea->sqlarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pArea->pTag )
   {
      sqlmixErrorRT( pArea, EG_NOORDER, 1201, NULL, 0, EF_CANDEFAULT );
      return ZH_FAILURE;
   }
   else
   {
      PMIXKEY      pKey;
      ZH_ERRCODE   errCode = ZH_SUCCESS;
      ZH_BOOL      fEOF;
      PMIXTAG      pTag = pArea->pTag;
      PMIXNODE     pNode;
      unsigned int uiKeyLen, ui;

      if( pArea->sqlarea.lpdbPendingRel && pArea->sqlarea.lpdbPendingRel->isScoped )
         SELF_FORCEREL( &pArea->sqlarea.area );

      pArea->sqlarea.area.fTop = pArea->sqlarea.area.fBottom = ZH_FALSE;
      pArea->sqlarea.area.fEof = ZH_FALSE;

      pKey = zh_mixKeyPutItem( NULL, pItem, fFindLast ? ( ZH_ULONG ) -1 : 0, pTag );

      uiKeyLen = pTag->uiKeyLen;
      if( pTag->bType == 'C' )
      {
         uiKeyLen = ( unsigned int ) zh_itemGetCLen( pItem );
         if( uiKeyLen > pTag->uiKeyLen )
            uiKeyLen = pTag->uiKeyLen;
      }

      zh_mixTagFindKey( pTag, pKey, uiKeyLen, &pNode, &ui, ZH_TRUE );
      zh_mixTagSetCurrent( pTag, pNode, ui );

      if( fFindLast )
      {
         if( pTag->fEof )
            zh_mixTagGoBottom( pTag );
         else
            zh_mixTagSkip( pTag, -1 );

         pArea->sqlarea.area.fFound = ! pTag->fEof && ( uiKeyLen == 0 || memcmp( pTag->CurKey->val, pKey->val, ( ZH_ULONG ) uiKeyLen ) == 0 );

         if( ! pArea->sqlarea.area.fFound )
            zh_mixTagSetCurrent( pTag, pNode, ui );
      }
      else
         pArea->sqlarea.area.fFound = ! pTag->fEof && ( uiKeyLen == 0 || memcmp( pTag->CurKey->val, pKey->val, ( ZH_ULONG ) uiKeyLen ) == 0 );

      fEOF = pTag->fEof;

      if( ! fEOF )
      {
         errCode = SELF_GOTO( &pArea->sqlarea.area, pTag->CurKey->rec );
         if( errCode != ZH_FAILURE && pArea->sqlarea.fPositioned )
         {
            errCode = SELF_SKIPFILTER( &pArea->sqlarea.area, fFindLast ? -1 : 1 );
            if( errCode != ZH_FAILURE && pArea->sqlarea.fPositioned )
            {
               pArea->sqlarea.area.fFound = ( uiKeyLen == 0 || memcmp( pTag->CurKey->val, pKey->val, ( ZH_ULONG ) uiKeyLen ) == 0 );
               if( ! pArea->sqlarea.area.fFound && ! fSoftSeek )
                  fEOF = ZH_TRUE;
            }
         }
      }

      if( errCode != ZH_FAILURE && fEOF )
         errCode = SELF_GOTO( &pArea->sqlarea.area, 0 );

      pArea->sqlarea.area.fBof = ZH_FALSE;

      zh_mixKeyFree( pKey );
      return errCode;
   }
}


static ZH_ERRCODE sqlmixSkipRaw( SQLMIXAREAP pArea, ZH_LONG lToSkip )
{
   PMIXTAG pTag = pArea->pTag;
   ZH_BOOL fOut = ZH_FALSE;

   if( SELF_GOCOLD( &pArea->sqlarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( ! pTag || lToSkip == 0 )
      return SUPER_SKIPRAW( &pArea->sqlarea.area, lToSkip );

   if( pArea->sqlarea.lpdbPendingRel )
      SELF_FORCEREL( &pArea->sqlarea.area );

   if( ! zh_mixTagRefreshKey( pTag ) )
   {
      if( lToSkip > 0 || pArea->sqlarea.fPositioned )
         fOut = ZH_TRUE;
      else
      {
         zh_mixTagGoBottom( pTag );
         fOut = pTag->fEof;
         lToSkip++;
      }
   }

   if( ! fOut )
      zh_mixTagSkip( pTag, lToSkip );

   if( SELF_GOTO( &pArea->sqlarea.area, ( pTag->fEof || fOut ) ? 0 : pTag->CurKey->rec ) != ZH_SUCCESS )
      return ZH_FAILURE;
   pArea->sqlarea.area.fEof = pTag->fEof;
   pArea->sqlarea.area.fBof = pTag->fBof;
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlmixGoCold( SQLMIXAREAP pArea )
{
   ZH_BOOL fRecordChanged = pArea->sqlarea.fRecordChanged;
   ZH_BOOL fAppend        = pArea->sqlarea.fAppend;

   if( SUPER_GOCOLD( &pArea->sqlarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( fRecordChanged && pArea->pTagList )
   {
      PMIXTAG     pTag;
      ZH_BOOL     fAdd, fDel;
      LPDBRELINFO lpdbPendingRel;

      lpdbPendingRel = pArea->sqlarea.lpdbPendingRel;
      pArea->sqlarea.lpdbPendingRel = NULL;

      pTag = pArea->pTagList;
      while( pTag )
      {
         if( ! pTag->fCustom )
         {
            PMIXKEY pKey = zh_mixKeyEval( NULL, pTag );

            if( pTag->pForItem != NULL )
               fAdd = zh_mixEvalCond( pArea, pTag->pForItem );
            else
               fAdd = ZH_TRUE;

            if( fAppend )
               fDel = ZH_FALSE;
            else
            {
               if( zh_mixKeyCompare( pTag, pKey, pTag->HotKey, pTag->uiKeyLen ) == 0 )
               {
                  fDel = ! fAdd && pTag->HotFor;
                  fAdd = fAdd && ! pTag->HotFor;
               }
               else
                  fDel = pTag->HotFor;
            }

            if( fDel )
               zh_mixTagDelKey( pTag, pTag->HotKey );

            if( fAdd )
               zh_mixTagAddKey( pTag, pKey );

            zh_mixKeyFree( pKey );
         }
         pTag = pTag->pNext;
      }
      pArea->sqlarea.lpdbPendingRel = lpdbPendingRel;
   }

   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlmixGoHot( SQLMIXAREAP pArea )
{
   PMIXTAG pTag;

#if 0
   if( pArea->fRecordChanged )
      printf( "sqlmixGoHot: multiple marking buffer as hot." );
#endif

   if( SUPER_GOHOT( &pArea->sqlarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   pTag = pArea->pTagList;
   while( pTag )
   {
      if( ! pTag->fCustom )
      {
         pTag->HotKey = zh_mixKeyEval( pTag->HotKey, pTag );
         pTag->HotFor = pTag->pForItem == NULL || zh_mixEvalCond( pArea, pTag->pForItem );
      }
      pTag = pTag->pNext;
   }
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlmixClose( SQLMIXAREAP pArea )
{
   if( SELF_GOCOLD( &pArea->sqlarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( SUPER_CLOSE( &pArea->sqlarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( SELF_ORDLSTCLEAR( &pArea->sqlarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlmixStructSize( SQLMIXAREAP pArea, ZH_USHORT * StructSize )
{
   ZH_SYMBOL_UNUSED( pArea );

   *StructSize = sizeof( SQLMIXAREA );
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlmixOrderListClear( SQLMIXAREAP pArea )
{
   PMIXTAG pTag;

   while( pArea->pTagList )
   {
      pTag = pArea->pTagList;
      pArea->pTagList = pTag->pNext;
      zh_mixTagDestroy( pTag );
   }
   pArea->pTag = NULL;
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlmixOrderListFocus( SQLMIXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   if( pArea->pTag )
      pOrderInfo->itmResult = zh_itemPutC( pOrderInfo->itmResult, pArea->pTag->szName );

   if( pOrderInfo->itmOrder )
      pArea->pTag = zh_mixFindTag( pArea, pOrderInfo->itmOrder );

   return pArea->pTag ? ZH_SUCCESS : ZH_FAILURE;
}


static ZH_ERRCODE sqlmixOrderCreate( SQLMIXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   PMIXTAG   pTagNew, pTag;
   PZH_ITEM  pKeyItem, pForItem = NULL, pWhileItem = NULL, pResult;
   ZH_ULONG  ulRecNo;
   ZH_USHORT uiLen;
   ZH_BYTE   bType;

   /* Obtain key codeblock */
   if( pOrderInfo->itmCobExpr )
      pKeyItem = zh_itemNew( pOrderInfo->itmCobExpr );
   else
   {
      if( SELF_COMPILE( &pArea->sqlarea.area, zh_itemGetCPtr( pOrderInfo->abExpr ) ) == ZH_FAILURE )
         return ZH_FAILURE;
      pKeyItem = pArea->sqlarea.area.valResult;
      pArea->sqlarea.area.valResult = NULL;
   }

   /* Test key codeblock on EOF */
   ulRecNo = pArea->sqlarea.ulRecNo;
   SELF_GOTO( &pArea->sqlarea.area, 0 );
   if( SELF_EVALBLOCK( &pArea->sqlarea.area, pKeyItem ) == ZH_FAILURE )
   {
      zh_vmDestroyBlockOrMacro( pKeyItem );
      SELF_GOTO( &pArea->sqlarea.area, ulRecNo );
      return ZH_FAILURE;
   }

   pResult = pArea->sqlarea.area.valResult;
   pArea->sqlarea.area.valResult = NULL;

   switch( zh_itemType( pResult ) )
   {
      case ZH_IT_STRING:
      case ZH_IT_STRING | ZH_IT_MEMO:
         bType = 'C';
         uiLen = ( ZH_USHORT ) zh_itemGetCLen( pResult );
         if( uiLen > MIX_MAXKEYLEN )
            uiLen = MIX_MAXKEYLEN;
         break;

      case ZH_IT_INTEGER:
      case ZH_IT_LONG:
      case ZH_IT_DOUBLE:
         bType = 'N';
         uiLen = 8;
         break;

      case ZH_IT_DATE:
         bType = 'D';
         uiLen = 8;
         break;

      case ZH_IT_LOGICAL:
         bType = 'L';
         uiLen = 1;
         break;

      default:
         bType = 'U';
         uiLen = 0;
   }
   zh_itemRelease( pResult );

   if( bType == 'U' || uiLen == 0 )
   {
      zh_vmDestroyBlockOrMacro( pKeyItem );
      SELF_GOTO( &pArea->sqlarea.area, ulRecNo );
      sqlmixErrorRT( pArea, bType == 'U' ? EG_DATATYPE : EG_DATAWIDTH, 1026, NULL, 0, 0 );
      return ZH_FAILURE;
   }

   if( pArea->sqlarea.area.lpdbOrdCondInfo )
   {
      /* Obtain FOR codeblock */
      if( pArea->sqlarea.area.lpdbOrdCondInfo->itmCobFor )
         pForItem = zh_itemNew( pArea->sqlarea.area.lpdbOrdCondInfo->itmCobFor );
      else if( pArea->sqlarea.area.lpdbOrdCondInfo->abFor )
      {
         if( SELF_COMPILE( &pArea->sqlarea.area, pArea->sqlarea.area.lpdbOrdCondInfo->abFor ) == ZH_FAILURE )
         {
            zh_vmDestroyBlockOrMacro( pKeyItem );
            SELF_GOTO( &pArea->sqlarea.area, ulRecNo );
            return ZH_FAILURE;
         }
         pForItem = pArea->sqlarea.area.valResult;
         pArea->sqlarea.area.valResult = NULL;
      }

      /* Obtain WHILE codeblock */
      if( pArea->sqlarea.area.lpdbOrdCondInfo->itmCobWhile )
         pWhileItem = zh_itemNew( pArea->sqlarea.area.lpdbOrdCondInfo->itmCobWhile );
      else if( pArea->sqlarea.area.lpdbOrdCondInfo->abWhile )
      {
         if( SELF_COMPILE( &pArea->sqlarea.area, pArea->sqlarea.area.lpdbOrdCondInfo->abWhile ) == ZH_FAILURE )
         {
            zh_vmDestroyBlockOrMacro( pKeyItem );
            if( pForItem )
               zh_vmDestroyBlockOrMacro( pForItem );
            SELF_GOTO( &pArea->sqlarea.area, ulRecNo );
            return ZH_FAILURE;
         }
         pWhileItem = pArea->sqlarea.area.valResult;
         pArea->sqlarea.area.valResult = NULL;
      }
   }

   /* Test FOR codeblock on EOF */
   if( pForItem )
   {
      if( SELF_EVALBLOCK( &pArea->sqlarea.area, pForItem ) == ZH_FAILURE )
      {
         zh_vmDestroyBlockOrMacro( pKeyItem );
         zh_vmDestroyBlockOrMacro( pForItem );
         if( pWhileItem )
            zh_vmDestroyBlockOrMacro( pWhileItem );
         SELF_GOTO( &pArea->sqlarea.area, ulRecNo );
         return ZH_FAILURE;
      }
      if( zh_itemType( pArea->sqlarea.area.valResult ) != ZH_IT_LOGICAL )
      {
         zh_itemRelease( pArea->sqlarea.area.valResult );
         pArea->sqlarea.area.valResult = 0;
         zh_vmDestroyBlockOrMacro( pKeyItem );
         zh_vmDestroyBlockOrMacro( pForItem );
         if( pWhileItem )
            zh_vmDestroyBlockOrMacro( pWhileItem );
         SELF_GOTO( &pArea->sqlarea.area, ulRecNo );
         sqlmixErrorRT( pArea, EG_DATATYPE, EDBF_INVALIDFOR, NULL, 0, 0 );
         return ZH_FAILURE;
      }
      zh_itemRelease( pArea->sqlarea.area.valResult );
      pArea->sqlarea.area.valResult = 0;
   }

   SELF_GOTO( &pArea->sqlarea.area, ulRecNo );

   pTagNew = zh_mixTagCreate( pOrderInfo->atomBagName, pOrderInfo->abExpr, pKeyItem, pForItem, pWhileItem, bType, uiLen, pArea );

   if( pWhileItem )
      zh_vmDestroyBlockOrMacro( pWhileItem );

   /* Append the tag to the end of list */
   if( pArea->pTagList )
   {
      pTag = pArea->pTagList;
      while( pTag->pNext )
         pTag = pTag->pNext;

      pTag->pNext = pTagNew;
   }
   else
      pArea->pTagList = pTagNew;

   pArea->pTag = pTagNew;
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlmixOrderInfo( SQLMIXAREAP pArea, ZH_USHORT uiIndex, LPDBORDERINFO pOrderInfo )
{
   PMIXTAG   pTag;
   ZH_USHORT uiTag = 0;

   switch( uiIndex )
   {
      case DBOI_EVALSTEP:
         pOrderInfo->itmResult = zh_itemPutNL( pOrderInfo->itmResult,
                                               pArea->sqlarea.area.lpdbOrdCondInfo ? pArea->sqlarea.area.lpdbOrdCondInfo->lStep : 0 );
         return ZH_SUCCESS;

      case DBOI_ORDERCOUNT:
         pTag = pArea->pTagList;
         while( pTag )
         {
            pTag = pTag->pNext;
            uiTag++;
         }
         pOrderInfo->itmResult = zh_itemPutNI( pOrderInfo->itmResult, uiTag );
         return ZH_SUCCESS;
   }

   if( SELF_GOCOLD( &pArea->sqlarea.area ) == ZH_FAILURE )
      return ZH_FAILURE;

   if( pOrderInfo->itmOrder )
      pTag = zh_mixFindTag( pArea, pOrderInfo->itmOrder );
   else
      pTag = pArea->pTag;

   switch( uiIndex )
   {
      case DBOI_CONDITION:
         pOrderInfo->itmResult = zh_itemPutC( pOrderInfo->itmResult, ( pTag ? pTag->szForExpr : NULL ) );
         if( pTag && pOrderInfo->itmNewVal && ZH_IS_STRING( pOrderInfo->itmNewVal ) )
         {
            if( pTag->szForExpr != NULL )
            {
               zh_xfree( pTag->szForExpr );
               pTag->szForExpr = NULL;
            }
            if( pTag->pForItem != NULL )
            {
               zh_vmDestroyBlockOrMacro( pTag->pForItem );
               pTag->pForItem = NULL;
            }
            if( zh_itemGetCLen( pOrderInfo->itmNewVal ) > 0 )
            {
               const char * pForExpr = zh_itemGetCPtr( pOrderInfo->itmNewVal );

               if( SELF_COMPILE( &pArea->sqlarea.area, pForExpr ) == ZH_SUCCESS )
               {
                  PZH_ITEM pForItem = pArea->sqlarea.area.valResult;

                  pArea->sqlarea.area.valResult = NULL;
                  if( SELF_EVALBLOCK( &pArea->sqlarea.area, pForItem ) == ZH_SUCCESS )
                  {
                     if( zh_itemType( pArea->sqlarea.area.valResult ) == ZH_IT_LOGICAL )
                     {
                        pTag->szForExpr = zh_strdup( pForExpr );
                        pTag->pForItem  = pForItem;
                        pForItem        = NULL;
                     }
                     zh_itemRelease( pArea->sqlarea.area.valResult );
                     pArea->sqlarea.area.valResult = NULL;
                  }
                  if( pForItem )
                     zh_vmDestroyBlockOrMacro( pForItem );
               }
            }
         }
         break;

      case DBOI_EXPRESSION:
         pOrderInfo->itmResult = zh_itemPutC( pOrderInfo->itmResult, pTag ? pTag->szKeyExpr : NULL );
         break;

      case DBOI_POSITION:
      case DBOI_KEYNORAW:
         if( pOrderInfo->itmNewVal && ZH_IS_NUMERIC( pOrderInfo->itmNewVal ) )
         {
            /* TODO */
#if 0
            pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult,
                                                 zh_cdxDBOIKeyGoto( pArea, pTag,
                                                                    zh_itemGetNL( pOrderInfo->itmNewVal ), uiIndex == DBOI_POSITION ) == ZH_SUCCESS );
#endif
         }
         else
            pOrderInfo->itmResult = zh_itemPutNL( pOrderInfo->itmResult,
                                                  zh_mixDBOIKeyNo( pTag, uiIndex == DBOI_POSITION ) );
         break;

      case DBOI_KEYCOUNT:
      case DBOI_KEYCOUNTRAW:
         pOrderInfo->itmResult = zh_itemPutNL( pOrderInfo->itmResult,
                                               zh_mixDBOIKeyCount( pTag, uiIndex == DBOI_KEYCOUNT ) );
         break;

         /* TODO */
#if 0
      case DBOI_FINDREC:
         pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult,
                                              zh_cdxDBOIFindRec( pArea, pTag,
                                                                 zh_itemGetNL( pOrderInfo->itmNewVal ), ZH_FALSE ) );
         break;

      case DBOI_FINDRECCONT:
         pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult,
                                              zh_cdxDBOIFindRec( pArea, pTag,
                                                                 zh_itemGetNL( pOrderInfo->itmNewVal ), ZH_TRUE ) );
         break;
#endif
      case DBOI_NAME:
         pOrderInfo->itmResult = zh_itemPutC( pOrderInfo->itmResult, pTag ? pTag->szName : NULL );
         break;

      case DBOI_NUMBER:
         pOrderInfo->itmResult = zh_itemPutNI( pOrderInfo->itmResult, uiTag );  /* otherwise */
         break;

      case DBOI_ISCOND:
         pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult, pTag && pTag->szForExpr != NULL );
         break;

      case DBOI_ISDESC:
      case DBOI_UNIQUE:
         pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult, ZH_FALSE );
         break;

      case DBOI_SCOPETOP:
      case DBOI_SCOPEBOTTOM:
         if( pOrderInfo->itmResult )
            zh_itemClear( pOrderInfo->itmResult );
         break;

      case DBOI_KEYTYPE:
         if( pTag )
         {
            char szType[ 2 ];

            szType[ 0 ] = ( char ) pTag->bType;
            szType[ 1 ] = 0;
            pOrderInfo->itmResult = zh_itemPutC( pOrderInfo->itmResult, szType );
         }
         else
            pOrderInfo->itmResult = zh_itemPutC( pOrderInfo->itmResult, NULL );
         break;

      case DBOI_KEYSIZE:
         pOrderInfo->itmResult = zh_itemPutNI( pOrderInfo->itmResult, pTag ? pTag->uiKeyLen : 0 );
         break;

      case DBOI_KEYDEC:
         pOrderInfo->itmResult = zh_itemPutNI( pOrderInfo->itmResult, 0 );
         break;

         /* TODO */
#if 0
      case DBOI_KEYVAL:
         zh_itemClear( pOrderInfo->itmResult );
         if( pArea->sqlarea.lpdbPendingRel )
            SELF_FORCEREL( &pArea->sqlarea.area );
         if( pTag && pArea->sqlarea.fPositioned )
         {
            if( pTag->CurKey->rec != pArea->sqlarea.ulRecNo )
            {
               zh_cdxIndexLockRead( pTag->pIndex );
               zh_cdxCurKeyRefresh( pArea, pTag );
               zh_cdxIndexUnLockRead( pTag->pIndex );
            }
            if( pTag->CurKey->rec == pArea->sqlarea.ulRecNo )
               pOrderInfo->itmResult = zh_cdxKeyGetItem( pTag->CurKey,
                                                         pOrderInfo->itmResult, pTag, ZH_TRUE );
         }
         break;

      case DBOI_CUSTOM:
         pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult, ( pTag ? pTag->fCustom : ZH_FALSE ) );
         if( pOrderInfo->itmNewVal && ZH_IS_LOGICAL( pOrderInfo->itmNewVal )
             && zh_itemGetL( pOrderInfo->itmNewVal ) )
            pTag->fCustom = ZH_TRUE;
         break;

      case DBOI_KEYADD:
         if( ! pTag )
            pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult, ZH_FALSE );
         else
         {
            if( pTag->fCustom )
            {
               if( pArea->sqlarea.lpdbPendingRel )
                  SELF_FORCEREL( &pArea->sqlarea.area );

               if( ! pArea->sqlarea.fPositioned ||
                   ( pTag->pForItem &&
                     ! zh_cdxEvalCond( pArea, pTag->pForItem, ZH_TRUE ) ) )
                  pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult, ZH_FALSE );
               else
               {
                  LPCDXKEY pKey;
                  zh_cdxIndexLockWrite( pTag->pIndex );
                  if( pOrderInfo->itmNewVal && ! ZH_IS_NIL( pOrderInfo->itmNewVal ) )
                     pKey = zh_cdxKeyPutItem( NULL, pOrderInfo->itmNewVal, pArea->sqlarea.ulRecNo, pTag, ZH_TRUE, ZH_TRUE );
                  else
                     pKey = zh_cdxKeyEval( NULL, pTag );
                  pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult,
                                                       zh_cdxTagKeyAdd( pTag, pKey ) );
                  zh_cdxIndexUnLockWrite( pTag->pIndex );
                  zh_cdxKeyFree( pKey );
               }
            }
            else
               sqlmixErrorRT( pArea, 0, 1052, NULL, 0, 0 );
         }
         break;

      case DBOI_KEYDELETE:
         if( ! pTag )
            pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult, ZH_FALSE );
         else
         {
            if( pTag->Custom )
            {
               if( pArea->sqlarea.lpdbPendingRel )
                  SELF_FORCEREL( &pArea->sqlarea.area );

               if( ! pArea->sqlarea.fPositioned ||
                   ( pTag->pForItem &&
                     ! zh_cdxEvalCond( pArea, pTag->pForItem, ZH_TRUE ) ) )
                  pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult, ZH_FALSE );
               else
               {
                  LPCDXKEY pKey;
                  zh_cdxIndexLockWrite( pTag->pIndex );
                  if( pOrderInfo->itmNewVal && ! ZH_IS_NIL( pOrderInfo->itmNewVal ) )
                     pKey = zh_cdxKeyPutItem( NULL, pOrderInfo->itmNewVal, pArea->sqlarea.ulRecNo, pTag, ZH_TRUE, ZH_TRUE );
                  else
                     pKey = zh_cdxKeyEval( NULL, pTag );
                  pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult,
                                                       zh_cdxTagKeyDel( pTag, pKey ) );
                  zh_cdxIndexUnLockWrite( pTag->pIndex );
                  zh_cdxKeyFree( pKey );
               }
            }
            else
               sqlmixErrorRT( pArea, 0, 1052, NULL, 0, 0 );
         }
         break;
#endif

      case DBOI_SHARED:
      case DBOI_ISREADONLY:
         pOrderInfo->itmResult = zh_itemPutL( pOrderInfo->itmResult, ZH_FALSE );
         break;

      default:
         return SUPER_ORDINFO( &pArea->sqlarea.area, uiIndex, pOrderInfo );

   }
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlmixInit( LPRDDNODE pRDD )
{
   /* This empty method is used to avoid duplicated sqlbase init call */
   ZH_SYMBOL_UNUSED( pRDD );
   return ZH_SUCCESS;
}


static ZH_ERRCODE sqlmixExit( LPRDDNODE pRDD )
{
   /* This empty method is used to avoid duplicated sqlbase exit call */
   ZH_SYMBOL_UNUSED( pRDD );
   return ZH_SUCCESS;
}


static RDDFUNCS sqlmixTable =
{ ( DBENTRYP_BP ) NULL,                 /* sqlmixBof */
  ( DBENTRYP_BP ) NULL,                 /* sqlmixEof */
  ( DBENTRYP_BP ) NULL,                 /* sqlmixFound */
  ( DBENTRYP_V ) sqlmixGoBottom,
  ( DBENTRYP_UL ) NULL,                 /* sqlmixGoTo */
  ( DBENTRYP_I ) NULL,                  /* sqlmixGoToId */
  ( DBENTRYP_V ) sqlmixGoTop,
  ( DBENTRYP_BIB ) sqlmixSeek,
  ( DBENTRYP_L ) NULL,                  /* sqlmixSkip */
  ( DBENTRYP_L ) NULL,                  /* sqlmixSkipFilter */
  ( DBENTRYP_L ) sqlmixSkipRaw,
  ( DBENTRYP_VF ) NULL,                 /* sqlmixAddField */
  ( DBENTRYP_B ) NULL,                  /* sqlmixAppend */
  ( DBENTRYP_I ) NULL,                  /* sqlmixCreateFields */
  ( DBENTRYP_V ) NULL,                  /* sqlmixDeleteRec */
  ( DBENTRYP_BP ) NULL,                 /* sqlmixDeleted */
  ( DBENTRYP_SP ) NULL,                 /* sqlmixFieldCount */
  ( DBENTRYP_VF ) NULL,                 /* sqlmixFieldDisplay */
  ( DBENTRYP_SSI ) NULL,                /* sqlmixFieldInfo */
  ( DBENTRYP_SCP ) NULL,                /* sqlmixFieldName */
  ( DBENTRYP_V ) NULL,                  /* sqlmixFlush */
  ( DBENTRYP_PP ) NULL,                 /* sqlmixGetRec */
  ( DBENTRYP_SI ) NULL,                 /* sqlmixGetValue */
  ( DBENTRYP_SVL ) NULL,                /* sqlmixGetVarLen */
  ( DBENTRYP_V ) sqlmixGoCold,
  ( DBENTRYP_V ) sqlmixGoHot,
  ( DBENTRYP_P ) NULL,                  /* sqlmixPutRec */
  ( DBENTRYP_SI ) NULL,                 /* sqlmixPutValue */
  ( DBENTRYP_V ) NULL,                  /* sqlmixRecall */
  ( DBENTRYP_ULP ) NULL,                /* sqlmixRecCount */
  ( DBENTRYP_ISI ) NULL,                /* sqlmixRecInfo */
  ( DBENTRYP_ULP ) NULL,                /* sqlmixRecNo */
  ( DBENTRYP_I ) NULL,                  /* sqlmixRecId */
  ( DBENTRYP_S ) NULL,                  /* sqlmixSetFieldExtent */
  ( DBENTRYP_CP ) NULL,                 /* sqlmixAlias */
  ( DBENTRYP_V ) sqlmixClose,
  ( DBENTRYP_VO ) NULL,                 /* sqlmixCreate */
  ( DBENTRYP_SI ) NULL,                 /* sqlmixInfo */
  ( DBENTRYP_V ) NULL,                  /* sqlmixNewArea */
  ( DBENTRYP_VO ) NULL,                 /* sqlmixOpen */
  ( DBENTRYP_V ) NULL,                  /* sqlmixRelease */
  ( DBENTRYP_SP ) sqlmixStructSize,
  ( DBENTRYP_CP ) NULL,                 /* sqlmixSysName */
  ( DBENTRYP_VEI ) NULL,                /* sqlmixEval */
  ( DBENTRYP_V ) NULL,                  /* sqlmixPack */
  ( DBENTRYP_LSP ) NULL,                /* sqlmixPackRec */
  ( DBENTRYP_VS ) NULL,                 /* sqlmixSort */
  ( DBENTRYP_VT ) NULL,                 /* sqlmixTrans */
  ( DBENTRYP_VT ) NULL,                 /* sqlmixTransRec */
  ( DBENTRYP_V ) NULL,                  /* sqlmixZap */
  ( DBENTRYP_VR ) NULL,                 /* sqlmixChildEnd */
  ( DBENTRYP_VR ) NULL,                 /* sqlmixChildStart */
  ( DBENTRYP_VR ) NULL,                 /* sqlmixChildSync */
  ( DBENTRYP_V ) NULL,                  /* sqlmixSyncChildren */
  ( DBENTRYP_V ) NULL,                  /* sqlmixClearRel */
  ( DBENTRYP_V ) NULL,                  /* sqlmixForceRel */
  ( DBENTRYP_SSP ) NULL,                /* sqlmixRelArea */
  ( DBENTRYP_VR ) NULL,                 /* sqlmixRelEval */
  ( DBENTRYP_SI ) NULL,                 /* sqlmixRelText */
  ( DBENTRYP_VR ) NULL,                 /* sqlmixSetRel */
  ( DBENTRYP_VOI ) NULL,                /* sqlmixOrderListAdd */
  ( DBENTRYP_V ) sqlmixOrderListClear,
  ( DBENTRYP_VOI ) NULL,                /* sqlmixOrderListDelete */
  ( DBENTRYP_VOI ) sqlmixOrderListFocus,
  ( DBENTRYP_V ) NULL,                  /* sqlmixOrderListRebuild */
  ( DBENTRYP_VOO ) NULL,                /* sqlmixOrderCondition */
  ( DBENTRYP_VOC ) sqlmixOrderCreate,
  ( DBENTRYP_VOI ) NULL,                /* sqlmixOrderDestroy */
  ( DBENTRYP_SVOI ) sqlmixOrderInfo,
  ( DBENTRYP_V ) NULL,                  /* sqlmixClearFilter */
  ( DBENTRYP_V ) NULL,                  /* sqlmixClearLocate */
  ( DBENTRYP_V ) NULL,                  /* sqlmixClearScope */
  ( DBENTRYP_VPLP ) NULL,               /* sqlmixCountScope */
  ( DBENTRYP_I ) NULL,                  /* sqlmixFilterText */
  ( DBENTRYP_SI ) NULL,                 /* sqlmixScopeInfo */
  ( DBENTRYP_VFI ) NULL,                /* sqlmixSetFilter */
  ( DBENTRYP_VLO ) NULL,                /* sqlmixSetLocate */
  ( DBENTRYP_VOS ) NULL,                /* sqlmixSetScope */
  ( DBENTRYP_VPL ) NULL,                /* sqlmixSkipScope */
  ( DBENTRYP_B ) NULL,                  /* sqlmixLocate */
  ( DBENTRYP_CC ) NULL,                 /* sqlmixCompile */
  ( DBENTRYP_I ) NULL,                  /* sqlmixError */
  ( DBENTRYP_I ) NULL,                  /* sqlmixEvalBlock */
  ( DBENTRYP_VSP ) NULL,                /* sqlmixRawLock */
  ( DBENTRYP_VL ) NULL,                 /* sqlmixLock */
  ( DBENTRYP_I ) NULL,                  /* sqlmixUnLock */
  ( DBENTRYP_V ) NULL,                  /* sqlmixCloseMemFile */
  ( DBENTRYP_VO ) NULL,                 /* sqlmixCreateMemFile */
  ( DBENTRYP_SCCS ) NULL,               /* sqlmixGetValueFile */
  ( DBENTRYP_VO ) NULL,                 /* sqlmixOpenMemFile */
  ( DBENTRYP_SCCS ) NULL,               /* sqlmixPutValueFile */
  ( DBENTRYP_V ) NULL,                  /* sqlmixReadDBHeader */
  ( DBENTRYP_V ) NULL,                  /* sqlmixWriteDBHeader */
  ( DBENTRYP_R ) sqlmixInit,
  ( DBENTRYP_R ) sqlmixExit,
  ( DBENTRYP_RVVL ) NULL,               /* sqlmixDrop */
  ( DBENTRYP_RVVL ) NULL,               /* sqlmixExists */
  ( DBENTRYP_RVVVL ) NULL,              /* sqlmixRename */
  ( DBENTRYP_RSLV ) NULL,               /* sqlmixRddInfo */
  ( DBENTRYP_SVP ) NULL,                /* sqlmixWhoCares */
};

/* force SQLBASE linking */
ZH_FUNC_TRANSLATE( SQLMIX, SQLBASE )

ZH_FUNC_STATIC( SQLMIX_GETFUNCTABLE )
{
   RDDFUNCS *  pTable;
   ZH_USHORT * puiCount, * puiSuperRddId, uiRddId;

   puiCount      = ( ZH_USHORT * ) zh_parptr( 1 );
   pTable        = ( RDDFUNCS * ) zh_parptr( 2 );
   uiRddId       = ( ZH_USHORT ) zh_parni( 4 );
   puiSuperRddId = ( ZH_USHORT * ) zh_parptr( 5 );

   if( pTable )
   {
      ZH_ERRCODE errCode;

      if( puiCount )
         *puiCount = RDDFUNCSCOUNT;

      errCode = zh_rddInheritEx( pTable, &sqlmixTable, &sqlmixSuper, "SQLBASE", puiSuperRddId );
      if( errCode == ZH_SUCCESS )
         s_uiRddIdSQLMIX = uiRddId;
      zh_retni( errCode );
   }
   else
      zh_retni( ZH_FAILURE );
}

static void zh_sqlmixRddInit( void * cargo )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( zh_rddRegister( "SQLBASE", RDT_FULL ) > 1 ||
       zh_rddRegister( "SQLMIX", RDT_FULL ) > 1 )
      zh_errInternal( ZH_EI_RDDINVALID, NULL, NULL, NULL );
}

ZH_INIT_SYMBOLS_BEGIN( sqlmix__InitSymbols )
{
   "SQLMIX", { ZH_FS_PUBLIC | ZH_FS_LOCAL }, { ZH_FUNCNAME( SQLMIX ) }, NULL
},
{ "SQLMIX_GETFUNCTABLE", { ZH_FS_PUBLIC | ZH_FS_LOCAL }, { ZH_FUNCNAME( SQLMIX_GETFUNCTABLE ) }, NULL }
ZH_INIT_SYMBOLS_END( sqlmix__InitSymbols )

ZH_CALL_ON_STARTUP_BEGIN( _zh_sqlmix_rdd_init_ )
zh_vmAtInit( zh_sqlmixRddInit, NULL );
ZH_CALL_ON_STARTUP_END( _zh_sqlmix_rdd_init_ )

#if defined( ZH_PRAGMA_STARTUP )
   #pragma startup sqlmix__InitSymbols
   #pragma startup _zh_sqlmix_rdd_init_
#elif defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY  ZH_DATASEG_FUNC( sqlmix__InitSymbols ) \
   ZH_DATASEG_FUNC( _zh_sqlmix_rdd_init_ )
   #include "hbiniseg.h"
#endif
