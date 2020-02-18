/*
 * DBFCDX RDD (ver.2)
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
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

#ifndef ZH_RDDCDX_H_
#define ZH_RDDCDX_H_

#include "zh_rdd_dbf.h"

ZH_EXTERN_BEGIN

/* CDX constants and defaults */
#define CDX_INDEXEXT                              ".cdx"
#define CDX_MAXKEY                                  240
#define CDX_MAXEXP                                  255
#define CDX_MAXTAGNAMELEN                            10
#define CDX_PAGELEN_BITS                              9
#define CDX_PAGELEN               (1<<CDX_PAGELEN_BITS)
#define CDX_PAGELEN_MAX                          0x2000
#define CDX_HEADERLEN                              1024
#define CDX_HEADEREXPLEN          (CDX_HEADERLEN - 512)
#define CDX_INT_HEADSIZE                             12
#define CDX_EXT_HEADSIZE                             24

#define CDX_ZIHER_SIGNATURE               0x52434842L /* Ziher index signature: RCHB */

#define CDX_STACKSIZE                                64
#define CDX_PAGECACHESIZE                             8
#define CDX_NODE_BRANCH                               0
#define CDX_NODE_ROOT                                 1
#define CDX_NODE_LEAF                                 2
#define CDX_NODE_UNUSED                            0xFF
#define CDX_IGNORE_REC_NUM                         0x0L
#define CDX_MAX_REC_NUM                     0xFFFFFFFFL
#define CDX_DUMMYNODE                       0xFFFFFFFFL
#define CDX_BALANCE_LEAFPAGES                         3
#define CDX_BALANCE_INTPAGES                          3

#define CDX_CURKEY_UNDEF                        (1<< 0)
#define CDX_CURKEY_REC                          (1<< 1)
#define CDX_CURKEY_VAL                          (1<< 2)
#define CDX_CURKEY_INPAGE                       (1<< 3)
#define CDX_CURKEY_INSTACK                      (1<< 4)
#define CDX_CURKEY_NOTEXIST                     (1<< 5)
#define CDX_CURKEY_RAWCNT                       (1<< 6)
#define CDX_CURKEY_RAWPOS                       (1<< 7)
#define CDX_CURKEY_LOGCNT                       (1<< 8)
#define CDX_CURKEY_LOGPOS                       (1<< 9)

#define TOP_RECORD                                    1
#define BTTM_RECORD                                   2
#define PREV_RECORD                                   3
#define NEXT_RECORD                                   4
#define PRVU_RECORD                                   6
#define NXTU_RECORD                                   5

#define NODE_NEWLASTKEY                               1
#define NODE_SPLIT                                    2
#define NODE_JOIN                                     4
#define NODE_BALANCE                                  8
#define NODE_EAT                                     16

#define CURKEY_RAWCNT(pTag)   (((pTag)->curKeyState & CDX_CURKEY_RAWCNT) != 0)
#define CURKEY_LOGCNT(pTag)   (((pTag)->curKeyState & CDX_CURKEY_LOGCNT) != 0)

#define CURKEY_RAWPOS(pTag)   ( ((pTag)->curKeyState & CDX_CURKEY_RAWPOS) != 0 && \
                                 (pTag)->rawKeyRec == (pTag)->CurKey->rec )
#define CURKEY_SETRAWPOS(pTag) do { (pTag)->curKeyState |= CDX_CURKEY_RAWPOS; \
                                    (pTag)->rawKeyRec = (pTag)->CurKey->rec; } while(0)

#define CURKEY_LOGPOS(pTag)   ( ((pTag)->curKeyState & CDX_CURKEY_LOGPOS) != 0 && \
                                 (pTag)->logKeyRec == (pTag)->pIndex->pArea->dbfarea.ulRecNo )
#define CURKEY_SETLOGPOS(pTag) do { (pTag)->curKeyState |= CDX_CURKEY_LOGPOS; \
                                    (pTag)->logKeyRec = (pTag)->pIndex->pArea->dbfarea.ulRecNo; } while(0)

/*
#define CURKEY_UNDEF(pTag)    (((pTag)->curKeyState & CDX_CURKEY_UNDEF) != 0)
#define CURKEY_NOTEXIST(pTag) (((pTag)->curKeyState & CDX_CURKEY_NOTEXIST) != 0)
#define CURKEY_ISSET(pTag)    (((pTag)->curKeyState & (CDX_CURKEY_NOTEXIST | CDX_CURKEY_UNDEF)) == 0)
#define CURKEY_REC(pTag)      ((((pTag)->curKeyState & CDX_CURKEY_REC) != 0) ? (pTag)->curKey->rec : 0)
#define CURKEY_VAL(pTag)      ((((pTag)->curKeyState & CDX_CURKEY_VAL) != 0) ? (pTag)->curKey->val : NULL)
#define CURKEY_REFRESH(pTag)
*/

#define ZH_CDXBITMASK( x )    ( ( ZH_LONG ) ( ( 1L << ( x ) ) - 1 ) )

/* #define FAST_GOCOLD( A )      ((A)->dbfarea.fRecordChanged || (A)->fCdxAppend ? (SELF_GOCOLD((AREAP)(A))) : ZH_SUCCESS) */
#define FAST_GOCOLD( A )      SELF_GOCOLD(A)


#define CDX_TYPE_UNIQUE        0x01    /* unique index */
#define CDX_TYPE_PARTIAL       0x02    /* temporary index */
#define CDX_TYPE_CUSTOM        0x04    /* custom index */
#define CDX_TYPE_FORFILTER     0x08    /* for expression present */
#define CDX_TYPE_BITVECTOR     0x10    /* SoftC? */
#define CDX_TYPE_COMPACT       0x20    /* FoxPro */
#define CDX_TYPE_COMPOUND      0x40    /* FoxPro */
#define CDX_TYPE_STRUCTURE     0x80    /* FoxPro */

#define CDX_CMP_EXACT          0x00    /* exact comparison */
#define CDX_CMP_PREFIX         0x01    /* prefix comparison */
#define CDX_CMP_DATE           0x02    /* date comparison */

/*
 SIx3 order temperature flags:
   switch ( indexOpt & ( CDX_TYPE_PARTIAL | CDX_TYPE_CUSTOM ) )
      case CDX_TYPE_PARTIAL:
         PARTIAL_RYO
      case CDX_TYPE_CUSTOM:
         PARTIAL_RYO | CHGONLY_RYO
      case CDX_TYPE_PARTIAL | CDX_TYPE_CUSTOM:
         PARTIAL_RYO | NOUPDATE_RYO
         if index key begin with:
            'sxChar(' or 'sxNum(' or 'sxDate(' or 'sxLog('
         then
            | TEMPLATE_RYO

   sx_Chill()  if ( ! NOUPDATE_RYO ) then set ( CHGONLY_RYO | PARTIAL_RYO )
                  if ( indexOpt & ( CDX_TYPE_PARTIAL | CDX_TYPE_CUSTOM ) !=
                        CDX_TYPE_PARTIAL | CDX_TYPE_CUSTOM )
                  {
                     indexOpt &= ~CDX_TYPE_CUSTOM;
                     indexOpt |= CDX_TYPE_PARTIAL
                  }

   sx_Warm()   if ( ! NOUPDATE_RYO ) then clear CHGONLY_RYO
                  if ( indexOpt & ( CDX_TYPE_PARTIAL | CDX_TYPE_CUSTOM ) !=
                        CDX_TYPE_PARTIAL | CDX_TYPE_CUSTOM )
                  {
                     indexOpt |= CDX_TYPE_CUSTOM;
                     indexOpt &= ~CDX_TYPE_PARTIAL
                  }

   sx_Freeze() set NOUPDATE_RYO
                  indexOpt |= CDX_TYPE_PARTIAL | CDX_TYPE_CUSTOM;
*/
/*
 indexSig:
   0x01     - CLIP like, ignoreCase significant
   0x20     - in index header: ADI file, in ADI tag header: ASCII
   0x00     - in ADI tag header: UNICODE
*/
/* CDX index node structures */
/* Compact Index Header Record */
typedef struct _CDXTAGHEADER
{
   ZH_BYTE     rootPtr  [ 4 ];   /* offset of the root node */
   ZH_BYTE     freePtr  [ 4 ];   /* offset of list of free pages or -1 */
   ZH_BYTE     counter  [ 4 ];   /* update counter (in root node) */
   ZH_BYTE     keySize  [ 2 ];   /* key length */
   ZH_BYTE     indexOpt;         /* index options see CDX_TYPE_* */
   ZH_BYTE     indexSig;         /* index signature */
   ZH_BYTE     headerLen[ 2 ];   /* ADI header length ???: 0x400 */
   ZH_BYTE     pageLen  [ 2 ];   /* ADI (idx) page length */
   ZH_BYTE     signature[ 4 ];   /* ADI (idx) collation signature */
   ZH_BYTE     reserved2[ 68 ];
   ZH_BYTE     lang     [ 26 ];  /* ADI (tag) ex. lt_LT, zh_Hant_TW_ADS_CI */
   ZH_BYTE     collatVer[ 4 ];   /* ADI (tag) collation signature */
   ZH_BYTE     reserved3[ 372 ];
   ZH_BYTE     codepage [ 5 ];   /* VFP codepage */
   ZH_BYTE     ignoreCase;       /* 1 = ignore case, key converted to upper */
   ZH_BYTE     reserved4[ 2 ];
   ZH_BYTE     ascendFlg[ 2 ];   /* 0 = ascending  1 = descending */
   ZH_BYTE     forExpPos[ 2 ];   /* offset of filter expression */
   ZH_BYTE     forExpLen[ 2 ];   /* length of filter expression */
   ZH_BYTE     keyExpPos[ 2 ];   /* offset of key expression */
   ZH_BYTE     keyExpLen[ 2 ];   /* length of key expression */
   ZH_BYTE     keyExpPool[ CDX_HEADEREXPLEN ];
} CDXTAGHEADER, * LPCDXTAGHEADER;

/* Compact Index Interior Node Record */
typedef struct _CDXINTNODE
{
   ZH_BYTE     attr    [ 2 ];    /* node type see CDX_NODE_* */
   ZH_BYTE     nKeys   [ 2 ];    /* number of keys */
   ZH_BYTE     leftPtr [ 4 ];    /* offset of left node or -1 */
   ZH_BYTE     rightPtr[ 4 ];    /* offset of right node or -1 */
} CDXINTNODE;

/* Compact Index Exterior Node Record */
typedef struct _CDXEXTNODE
{
   ZH_BYTE     attr    [ 2 ];    /* node type see CDX_NODE_* */
   ZH_BYTE     nKeys   [ 2 ];    /* number of keys */
   ZH_BYTE     leftPtr [ 4 ];    /* offset of left node or -1 */
   ZH_BYTE     rightPtr[ 4 ];    /* offset of right node or -1 */
   ZH_BYTE     freeSpc [ 2 ];    /* free space available in a page */
   ZH_BYTE     recMask [ 4 ];    /* record number mask */
   ZH_BYTE     dupMask;          /* duplicate bytes count mask */
   ZH_BYTE     trlMask;          /* trailing bytes count mask */
   ZH_BYTE     recBits;          /* number of bits for record number */
   ZH_BYTE     dupBits;          /* number of bits for duplicate count */
   ZH_BYTE     trlBits;          /* number of bits for trailing count */
   ZH_BYTE     keyBytes;         /* total number of bytes for recno/dup/trail info */
} CDXEXTNODE;



/* CDX internal memory structures */

struct _CDXAREA;  /* forward declaration */
struct _CDXINDEX; /* forward declaration */
struct _CDXTAG;   /* forward declaration */

typedef struct _CDXKEY
{
   ZH_ULONG  rec;
   ZH_USHORT mode;
   ZH_USHORT len;
   ZH_BYTE   val[ 1 ];
} CDXKEY, * LPCDXKEY;

typedef struct _CDXPAGE
{
   ZH_ULONG  Page;
   ZH_ULONG  Left;
   ZH_ULONG  Right;

   int       iKeys;
   int       iCurKey;

   ZH_ULONG  RNMask;
   ZH_USHORT DCMask;
   ZH_USHORT TCMask;
   ZH_BYTE   RNBits;
   ZH_BYTE   DCBits;
   ZH_BYTE   TCBits;
   ZH_BYTE   ReqByte;

   ZH_BYTE   PageType;
   ZH_BYTE   bUsed;
   ZH_BOOL   fChanged;
   ZH_BOOL   fBufChanged;

   ZH_SHORT  bufKeyNum;                    /* do not change these vars' order */
   ZH_SHORT  bufKeyPos;                    /* they have to be just after the node */
   ZH_SHORT  bufKeyLen;                    /* and maybe temporary overwritten when adding */
   ZH_SHORT  iFree;                        /* new key to interior node record. */

   ZH_BYTE * pKeyBuf;                      /* pointer to uncompressed leaf page key pool */

   struct _CDXPAGE * Owner;
   struct _CDXPAGE * Child;
   struct _CDXTAG  * TagParent;
   struct _CDXPAGE * pPoolPrev;
   struct _CDXPAGE * pPoolNext;

   union
   {
      CDXEXTNODE extNode;
      CDXINTNODE intNode;
   } node;
} CDXPAGE, * LPCDXPAGE;

typedef struct _CDXSTACK
{
   LPCDXPAGE Page;
   int       iKey;
} CDXSTACK, * LPCDXSTACK;

typedef struct _CDXLIST
{
   ZH_ULONG nextPage;
   ZH_BOOL  fStat;
   struct _CDXLIST * pNext;
} CDXLIST, * LPCDXLIST;

typedef struct _CDXTAG
{
   char *    szName;          /* Name of tag */
   char *    KeyExpr;         /* a tag key expression as text */
   char *    ForExpr;         /* a tag for expression as text */
   PZH_ITEM  pKeyItem;        /* item with a macro pcode for a tag key expression */
   PZH_ITEM  pForItem;        /* item with a macro pcode for a tag for expression */
   ZH_USHORT uiType;          /* a type of key expression value */
   ZH_USHORT uiLen;           /* length of the key expression value */
   ZH_USHORT nField;          /* Field number for simple (one field) key expression */
   ZH_BYTE   bTrail;          /* trailing character for shorter key value */
   ZH_BYTE   OptFlags;        /* index options flag */
   ZH_BOOL   AscendKey;       /* ascending/descending order flag */
   ZH_BOOL   UniqueKey;       /* unique order flag */
   ZH_BOOL   Custom;          /* custom order flag */
   ZH_BOOL   Template;        /* user key data in ordKeyAdd()/ordKeyDel() accepted */
   ZH_BOOL   MultiKey;        /* repeated key values in custom indexes accepted */
   ZH_BOOL   Partial;         /* order is updated only partially - missing keys possible */
   ZH_BOOL   ChgOnly;         /* only existing key modifications are updated, no new key added */
   ZH_BOOL   UsrAscend;       /* user settable ascending/descending order flag */
   ZH_BOOL   UsrUnique;       /* user settable unique order flag */
   ZH_BOOL   IgnoreCase;      /* ignore case (upper keys) */

   ZH_BOOL   TagChanged;
   ZH_BOOL   TagBOF;
   ZH_BOOL   TagEOF;

   ZH_BOOL   fRePos;
   int       curKeyState;     /* see: CDX_CURKEY_* */
   ZH_ULONG  rawKeyCount;
   ZH_ULONG  rawKeyPos;
   ZH_ULONG  rawKeyRec;
   ZH_ULONG  logKeyCount;
   ZH_ULONG  logKeyPos;
   ZH_ULONG  logKeyRec;

   ZH_ULONG  TagBlock;        /* a page offset where a tag header is stored */
   ZH_ULONG  RootBlock;       /* a page offset with the root of keys tree */
   ZH_USHORT MaxKeys;         /* maximum number of keys in Interior node */

   struct _CDXINDEX * pIndex; /* a parent index info */
   struct _CDXTAG   * pNext;  /* pointer to next tag in index */

   #if 0
   CDXSTACK   PageStack[ CDX_STACKSIZE ];  /* stack with page path to current key */
   #endif
   LPCDXPAGE  RootPage;       /* pointer to root of keys tree in memory */
   LPCDXKEY   CurKey;         /* current value of key expression */
   LPCDXKEY   HotKey;         /* value of hot key expression */
   ZH_BOOL    HotFor;         /* index FOR condition for HotKey */

   PZH_ITEM   topScope;       /* Top scope ZH_ITEM */
   LPCDXKEY   topScopeKey;    /* Top scope index key */
   PZH_ITEM   bottomScope;    /* Bottom scope ZH_ITEM */
   LPCDXKEY   bottomScopeKey; /* Bottom index key */

   LPCDXPAGE  pagePool;       /* page buffer in memory */
} CDXTAG, * LPCDXTAG;

typedef struct _CDXINDEX
{
   char *     szFileName;     /* Name of index file */
   char *     szRealName;     /* Real name of index file */
   PZH_FILE   pFile;          /* Index file handle */
   struct _CDXAREA  * pArea;  /* Parent WorkArea */
   struct _CDXINDEX * pNext;  /* The next index in the list */
   LPCDXTAG   pCompound;      /* Compound tag */
   LPCDXTAG   TagList;        /* List of tags in index file */
   ZH_BOOL    fShared;        /* Shared file */
   ZH_BOOL    fReadonly;      /* Read only file */
   ZH_BOOL    fDelete;        /* delete on close flag */
   ZH_BOOL    fLargeFile;     /* page numbers instead of page offsets in index file */
   ZH_USHORT  uiHeaderLen;    /* length of tag header */
   ZH_USHORT  uiPageLen;      /* length of index page */
   ZH_USHORT  uiPageBits;     /* length of index page in bits */
   ZH_USHORT  uiMaxKeyLen;    /* maximum index key length */
   ZH_ULONG   nextAvail;      /* offset to next free page in the end of index file */
   ZH_ULONG   freePage;       /* offset to next free page inside index file */
   LPCDXLIST  freeLst;        /* list of free pages in index file */
   int        lockWrite;      /* number of write lock set */
   int        lockRead;       /* number of read lock set */
   ZH_DBFLOCKDATA lockData;   /* index lock data */
#ifdef ZH_CDX_DBGCODE
   ZH_BOOL    RdLck;
   ZH_BOOL    WrLck;
#endif
   ZH_BOOL    fChanged;       /* changes written to index, need to update ulVersion */
   ZH_BOOL    fFlush;         /* changes written to index, need to update ulVersion */
   ZH_ULONG   ulVersion;      /* network version/update flag */
} CDXINDEX, * LPCDXINDEX;

/* for index creation */
typedef struct
{
   ZH_FOFFSET nOffset;         /* offset in temporary file */
   ZH_ULONG   ulKeys;          /* number of keys in page */
   ZH_ULONG   ulKeyBuf;        /* number of keys in memory buffer */
   ZH_ULONG   ulCurKey;        /* current key in memory buffer */
   ZH_BYTE *  pKeyPool;        /* memory buffer */
} CDXSWAPPAGE, * LPCDXSWAPPAGE;

typedef struct
{
   LPCDXTAG   pTag;           /* current Tag */
   PZH_FILE   pTempFile;      /* handle to temporary file */
   char *     szTempFileName; /* temporary file name */
   int        keyLen;         /* key length */
   ZH_BYTE    bTrl;           /* filler char for shorter keys */
   ZH_BOOL    fUnique;        /* ZH_TRUE if index is unique */
   ZH_BOOL    fReindex;       /* ZH_TRUE if reindexing is in process */
   ZH_ULONG   ulMaxRec;       /* the highest record number */
   ZH_ULONG   ulTotKeys;      /* total number of keys indexed */
   ZH_ULONG   ulKeys;         /* keys in currently created page */
   ZH_ULONG   ulPages;        /* number of pages */
   ZH_ULONG   ulCurPage;      /* current page */
   ZH_ULONG   ulPgKeys;       /* maximum number of key in page memory buffer */
   ZH_ULONG   ulMaxKey;       /* maximum number of keys in single page */
   ZH_BYTE *  pKeyPool;       /* memory buffer for current page then for pages */
   ZH_BYTE *  pStartKey;      /* beginning of key pool after sorting */
   LPCDXSWAPPAGE pSwapPage;   /* list of pages */
   LPCDXPAGE  NodeList[ CDX_STACKSIZE ];  /* Stack of pages */
   ZH_ULONG   ulFirst;
   ZH_ULONG * pSortedPages;
   ZH_BYTE *  pLastKey;       /* last key val */
   ZH_ULONG   ulLastRec;
   ZH_BYTE *  pRecBuff;
#ifndef ZH_CDX_PACKTRAIL
   int        iLastTrl;       /* last key trailing spaces */
#endif
} CDXSORTINFO, * LPCDXSORTINFO;



/*
 *  DBF WORKAREA
 *  ------------
 *  The Workarea Structure of DBFCDX RDD
 *
 */

typedef struct _CDXAREA
{
   DBFAREA dbfarea;

   /*
    * CDX's additions to the workarea structure
    *
    * Warning: The above section MUST match DBFAREA exactly! Any
    * additions to the structure MUST be added below, as in this
    * example.
    */

   LPCDXSORTINFO  pSort;         /* Index build structure */
   LPCDXINDEX     lpIndexes;     /* Pointer to indexes array */
   const ZH_UCHAR * sortTab;     /* Table with sorted characters */
   ZH_BOOL        fCdxAppend;    /* Appended record.zhhanged */
   ZH_BOOL        fSortCDP;      /* Use CDP functions for sorting */
   ZH_USHORT      uiTag;         /* current tag focus */

} CDXAREA, * LPCDXAREA;

#ifndef CDXAREAP
#define CDXAREAP LPCDXAREA
#endif

#undef  SUPERTABLE
#define SUPERTABLE                         ( &cdxSuper )

ZH_EXTERN_END

#endif /* ZH_RDDCDX_H_ */
