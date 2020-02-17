/*
 * DBFNTX RDD
 *
 * Copyright 2000 Alexander Kresin <alex@belacy.belgorod.su>
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

#ifndef ZH_RDDNTX_H_
#define ZH_RDDNTX_H_

#include "zh_rdd_dbf.h"

ZH_EXTERN_BEGIN

/* DBFNTX default extensions */
#define NTX_INDEXEXT                             ".ntx"

/* DBFNTX constants declarations */

#define NTX_IGNORE_REC_NUM                        0x0UL
#define NTX_MAX_REC_NUM                    0xFFFFFFFFUL

#define NTX_DUMMYNODE                      0xFFFFFFFFUL

#define NTX_FLAG_DEFALUT         0x0006
#define NTX_FLAG_OLDDEFALUT      0x0003
#define NTX_FLAG_FORITEM         0x0001
#define NTX_FLAG_PARTIAL         0x0008
#define NTX_FLAG_EXTLOCK         0x0010
#define NTX_FLAG_CUSTOM          0x0020
#define NTX_FLAG_CHGONLY         0x0040
#define NTX_FLAG_TEMPLATE        0x0080
#define NTX_FLAG_SORTRECNO       0x0100
#define NTX_FLAG_LARGEFILE       0x0200
#define NTX_FLAG_MULTIKEY        0x0400
#define NTX_FLAG_COMPOUND        0x8000
#define NTX_FLAG_MASK            0x87FF

#define CTX_MAX_TAGS                 63

#define NTX_MAX_KEY                     256     /* Max length of key */
#define NTX_MAX_EXP                     256     /* Max length of KEY/FOR expression */
#define NTXBLOCKBITS                     10     /* Size of NTX block in bits */
#define NTXBLOCKSIZE      (1<<NTXBLOCKBITS)     /* Size of block in NTX file */
#define NTX_MAX_TAGNAME                  10     /* Max length of tag name */
#define NTX_HDR_UNUSED                  473     /* the unused part of header */
#define NTX_PAGES_PER_TAG                 8
#define NTX_STACKSIZE                    32     /* Maximum page stack size */

#define NTX_ROOTHEAD_HEADSIZE            12

/* index file structures - defined as ZH_BYTEs to avoid alignment problems */

typedef struct _NTXHEADER     /* Header of NTX file */
{
   ZH_BYTE  type[ 2 ];
   ZH_BYTE  version[ 2 ];
   ZH_BYTE  root[ 4 ];
   ZH_BYTE  next_page[ 4 ];
   ZH_BYTE  item_size[ 2 ];
   ZH_BYTE  key_size[ 2 ];
   ZH_BYTE  key_dec[ 2 ];
   ZH_BYTE  max_item[ 2 ];
   ZH_BYTE  half_page[ 2 ];
   ZH_BYTE  key_expr[ NTX_MAX_EXP ];
   ZH_BYTE  unique[ 1 ];
   ZH_BYTE  unknown1[ 1 ];
   ZH_BYTE  descend[ 1 ];
   ZH_BYTE  unknown2[ 1 ];
   ZH_BYTE  for_expr[ NTX_MAX_EXP ];
   ZH_BYTE  tag_name[ NTX_MAX_TAGNAME + 2 ];
   ZH_BYTE  custom[ 1 ];
   ZH_BYTE  unused[ NTX_HDR_UNUSED ];
} NTXHEADER;
typedef NTXHEADER * LPNTXHEADER;

typedef struct
{
   ZH_BYTE  type[ 2 ];
   ZH_BYTE  version[ 2 ];
   ZH_BYTE  root[ 4 ];
} NTXHEADERUPDT;

typedef struct _CTXTAGITEM    /* TAG item in compound NTX (CTX) header */
{
   ZH_BYTE  tag_name[ NTX_MAX_TAGNAME + 2 ];
   ZH_BYTE  tag_header[ 4 ];
} CTXTAGITEM;
typedef CTXTAGITEM * LPCTXTAGITEM;

typedef struct _CTXHEADER     /* Header of Ziher CTX file */
{
   ZH_BYTE  type[ 2 ];     /* 0x9591 LE */
   ZH_BYTE  ntags[ 2 ];    /* number of tag entries MAX63 */
   ZH_BYTE  version[ 4 ];  /* update counter LE */
   ZH_BYTE  freepage[ 4 ]; /* first free page in index file */
   ZH_BYTE  filesize[ 4 ]; /* size of index file in pages */
   CTXTAGITEM tags[ CTX_MAX_TAGS ];
} CTXHEADER;
typedef CTXHEADER * LPCTXHEADER;

#if 0
/* original CLIP CTX file header - for information only it's binary
   compatible so both RDD can read the same file but it's not safe
   to use CLIP for writing when the file is open by Ziher.
   In spare time I'll update CLIP to respect my extensions and send
   patches to Rust - hope they will be included in CLIP.
*/
typedef struct _CTXHEADER     /* Header of CLIP CTX file */
{
   ZH_BYTE  type[ 2 ];     /* 0x9591 in LE */
   ZH_BYTE  ntags[ 1 ];    /* number of tag entries */
   ZH_BYTE  unused[ 13 ];
   CTX_TAG tags[ 63 ];
} CTXHEADER
#endif


/* forward declarations
 */
struct _RDDFUNCS;
struct _NTXAREA;
struct _TAGINFO;
struct _NTXINDEX;

typedef struct _KEYINFO
{
   ZH_ULONG Tag;      /* page number */
   ZH_ULONG Xtra;     /* record number */
   char     key[ 1 ]; /* key value */
} KEYINFO;
typedef KEYINFO * LPKEYINFO;

typedef struct _TREE_STACK
{
   ZH_ULONG page;
   ZH_SHORT ikey;
}  TREE_STACK;
typedef TREE_STACK * LPTREESTACK;

typedef struct _ZH_PAGEINFO
{
   ZH_ULONG  Page;
   ZH_BOOL   Changed;
   int       iUsed;
   ZH_USHORT uiKeys;
   struct _ZH_PAGEINFO * pNext;
   struct _ZH_PAGEINFO * pPrev;
#ifdef ZH_NTX_EXTERNAL_PAGEBUFFER
   char *    buffer;
#else
   char      buffer[ NTXBLOCKSIZE ];
#endif
} ZH_PAGEINFO;
typedef ZH_PAGEINFO * LPPAGEINFO;

typedef struct _ZH_NTXSCOPE
{
   PZH_ITEM   scopeItem;
   LPKEYINFO  scopeKey;
   ZH_USHORT  scopeKeyLen;
} ZH_NTXSCOPE;
typedef ZH_NTXSCOPE * PZH_NTXSCOPE;

typedef struct _TAGINFO
{
   char *      TagName;
   char *      KeyExpr;
   char *      ForExpr;
   PZH_ITEM    pKeyItem;
   PZH_ITEM    pForItem;
   ZH_NTXSCOPE top;
   ZH_NTXSCOPE bottom;

   ZH_USHORT   Signature;

   ZH_BOOL     fTagName;
   ZH_BOOL     fUsrDescend;
   ZH_BOOL     AscendKey;
   ZH_BOOL     UniqueKey;

   ZH_BOOL     Custom;
   ZH_BOOL     ChgOnly;
   ZH_BOOL     Partial;
   ZH_BOOL     Template;
   ZH_BOOL     MultiKey;
   ZH_BOOL     fSortRec;

   ZH_BOOL     HdrChanged;
   ZH_BOOL     TagBOF;
   ZH_BOOL     TagEOF;
   ZH_ULONG    HeadBlock;
   ZH_ULONG    RootBlock;
   ZH_USHORT   uiNumber;
   ZH_BYTE     KeyType;
   ZH_USHORT   nField;
   ZH_USHORT   KeyLength;
   ZH_USHORT   KeyDec;
   ZH_USHORT   MaxKeys;
   LPTREESTACK stack;
   ZH_USHORT   stackSize;
   ZH_USHORT   stackLevel;
   ZH_ULONG    keyCount;
   LPKEYINFO   CurKeyInfo;
   LPKEYINFO   HotKeyInfo;
   ZH_BOOL     HotFor;

   struct     _NTXINDEX * pIndex;
} TAGINFO;
typedef TAGINFO * LPTAGINFO;

typedef struct _NTXINDEX
{
   char *      IndexName;
   char *      RealName;
   ZH_ULONG    Version;       /* The index VERSION filed to signal index updates for other stations */
   ZH_ULONG    NextAvail;
   ZH_ULONG    TagBlock;      /* Index attr, next free page */
   struct     _NTXAREA * pArea;
   PZH_FILE    DiskFile;
   ZH_BOOL     fDelete;       /* delete on close flag */
   ZH_BOOL     fReadonly;
   ZH_BOOL     fShared;
   ZH_BOOL     fFlush;
   ZH_BOOL     LargeFile;
   ZH_BOOL     Changed;
   ZH_BOOL     Update;
   ZH_BOOL     Compound;
   ZH_BOOL     Production;    /* Production index */
   ZH_DBFLOCKDATA lockData;   /* index lock data */
   int         lockWrite;     /* number of write lock set */
   int         lockRead;      /* number of read lock set */

   ZH_BYTE *   HeaderBuff;    /* TODO: make it member */
   ZH_BOOL     fValidHeader;
   int         iTags;
   LPTAGINFO * lpTags;

   ZH_ULONG    ulPages;
   ZH_ULONG    ulPageLast;
   ZH_ULONG    ulPagesDepth;
   LPPAGEINFO *pages;
   LPPAGEINFO  pChanged;
   LPPAGEINFO  pFirst;
   LPPAGEINFO  pLast;

   struct     _NTXINDEX * pNext;   /* The next index in the list */
} NTXINDEX;
typedef NTXINDEX * LPNTXINDEX;

/* for index creation */
typedef struct
{
   ZH_FOFFSET  nOffset;    /* offset in temporary file */
   ZH_ULONG    ulKeys;     /* number of keys in page */
   ZH_ULONG    ulKeyBuf;   /* number of keys in memory buffer */
   ZH_ULONG    ulCurKey;   /* current key in memory buffer */
   ZH_BYTE *   pKeyPool;   /* memory buffer */
} NTXSWAPPAGE;
typedef NTXSWAPPAGE * LPNTXSWAPPAGE;

typedef struct
{
   LPTAGINFO  pTag;           /* current Tag */
   PZH_FILE   pTempFile;      /* handle to temporary file */
   char *     szTempFileName; /* temporary file name */
   int        keyLen;         /* key length */
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
   LPNTXSWAPPAGE pSwapPage;   /* list of pages */
   LPPAGEINFO NodeList[ NTX_STACKSIZE ];   /* Stack of pages */
   ZH_ULONG   ulFirst;
   ZH_ULONG * pSortedPages;
   ZH_BYTE    pLastKey[ NTX_MAX_KEY ]; /* last key val */
   ZH_ULONG   ulLastRec;

   ZH_BYTE *  pBuffIO;        /* index IO buffer */
   ZH_ULONG   ulSizeIO;       /* size of IO buffer in index pages */
   ZH_ULONG   ulPagesIO;      /* number of index pages in buffer */
   ZH_ULONG   ulFirstIO;      /* first page in buffer */
   ZH_ULONG   ulLastIO;       /* last page in buffer */
} NTXSORTINFO;
typedef NTXSORTINFO * LPNTXSORTINFO;

/*
 *  DBF WORKAREA
 *  ------------
 *  The Workarea Structure of DBFNTX RDD
 *
 */

typedef struct _NTXAREA
{
   DBFAREA dbfarea;

   /*
   *  NTX's additions to the workarea structure
   *
   *  Warning: The above section MUST match DBFAREA exactly! Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   ZH_BOOL        fNtxAppend;       /* ZH_TRUE if new record is added */
   ZH_BOOL        fSetTagNumbers;   /* Tag number should be recreated */
   LPNTXINDEX     lpIndexes;        /* Pointer to list of indexes */
   LPTAGINFO      lpCurTag;         /* Pointer to current order */
   LPNTXSORTINFO  pSort;            /* Index build structure */

} NTXAREA;
typedef NTXAREA * LPNTXAREA;

#ifndef NTXAREAP
#define NTXAREAP LPNTXAREA
#endif

#undef  SUPERTABLE
#define SUPERTABLE                         ( &ntxSuper )

ZH_EXTERN_END

#endif /* ZH_RDDNTX_H_ */
