/*
 * DBFNSX RDD
 *
 * Copyright 2008 Przemyslaw Czerpak
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
NSX description:
1. Keys are stored in interior (branch) and exterior (leaf) nodes
   like in NTX not a CDX where all keys are in leaf nodes and keys
   in interior nodes are duplicated higher key value in subnodes.
2. The hidden part of index key is record number what causes that
   keys are always unique (like in CDX not in NTX)
3. Because the leaf and branch nodes have different internal structure
   it's necessary to keep the index tree well balanced (all leaf
   nodes have to be located at the same level). The number of keys
   in leaf pages is variable and the keys in interior node records
   are not duplicated in leafs so when index is created it's impossible
   to calculate the optimal tree in one pass. To resolve this problem
   at the end empty pages (without keys) must be added to the tree.
   Then the keys in the two last pages at each level can be balanced
   if we want to keep strict minimum keys in page factor or want to
   eliminate empty pages though it's not necessary in interior nodes.
   Only last leaf page cannot be empty because it breaks classic index
   navigation code. To resolve this problem SIX3 NSX RDD moves the
   last key added to index from branch node to the last empty leaf
   node and then moves the last key from the leaf node before last
   to the branch page from which the last index key was taken. It
   does not balance the right most pages. This may not work correctly
   for unique indexes if the last key is not added to the index because
   it's replicated. In such case SIX3 creates buggy indexes - Ziher
   will report them as corrupted after dbGoBottom(). Ziher creates
   correct indexes.
4. All numbers are in little endian byte order
5. The numeric and date key values are modified IEEE754 double value
   8 bytes length (the same format as in CDX), the logical values are
   1-byte length "T" or "F"
6. Minimum RecNoLen used by SIX3 driver seems to be 2 and maximum 3!!!
   It means that tables with more then 167772165 are not supported.
   SIX3 RDD creates indexes for them but they are buggy and the most
   important byte in record number is lost. It cannot be easy detected
   in normal navigation so Ziher will report corruption error only
   during update operation if previous key value with valid record
   number cannot be located. Ziher RDD does not have such limit
   and can support 2^32-2 records = 4294967294.
7. Branch nodes have also field to mark record length (RecNoLen) but
   it seems to be unused (filled with 0 or 4) and record number is
   always stored in 4 bytes.
8. Maximum key length is set to 250 but in fact with 4 bytes record
   length such keys cannot be supported without adding some hack because
   RecNoLen[4] + Size[1] + DupCount[1] + 250 gives 256 and it cannot
   be stored in 1 byte (Size). We can add support for such long
   indexes but we have to define that Size=0 means Size=256. It can
   be done because Size=0 cannot appear in other way. I implemented
   it and Ziher works well with such indexes.
9. In root header is address of 1st available free page in index file,
   addresses of next pages are stored in the free pages at the same
   position as in root header (offset = 6)
10.Leaf nodes use for duplicate compression keys' values from upper
   interior nodes so it's necessary to keep them on the index tree
   stack. The first key in index is not compressed at all.
11.Non duplicated part of key in leaf page is compressed by RLE
   algorithm and special RLE char is 0xFF.
12.During indexing SIX3 NSX RDD adds key to leaf node only if it can be
   added in raw form without any compression. It checks if free space
   in leaf node is at least KEY_LEN + RECNO_LEN + 2 and if it is
   then it always adds key. It should be safe but seems that it uses
   hard coded RECNO_LEN=2 value so when RECNO_LEN is 3 (DBF has more
   then 64645 records) the last byte from key can be lost!!! and corrupted
   index is created. Ziher NSX RDD checks if there is enough space for
   fully compressed key. It also means that it can hold more records in
   leaf nodes and total index size is usually smaller then in SIX3.
   SIX3 can use Ziher indexes though I cannot be sure if they will not
   cause some other memory buffer overflows like the one described above
   in SIX3 code. I only hope that not or at least they will not be critical.
13.During index update SIX3 NSX do not execute any code to balance key in
   index nodes to keep some minimum number of keys in page. It may cause
   that in systems with a lot of updates indexes will be much bigger then
   they should. It means that it's good to make some temporary reindexing.
   The base Ziher implementation replicate this behavior but it may
   change in the future and I'll add some code to keep keys in index
   pages well balanced. In well balanced indexes exist some conditions
   which can make whole code much simpler but unfortunately if we want
   to be SIX3 compatible we cannot use them because after updating index
   by SIX3 we will lost them.
14.Ziher do not use any tricks with byte oriented sorting and pass
   whole index keys for comparison to CP compare function which respects
   all national settings. Such tricks may speed up some operations but
   they break sorting in languages which uses multibyte characters or
   accented characters with the same weight as normal ones.
15.The default memo type in DBFNSX is set to SMT. It will affect only
   newly created files (dbCreate()). When DBF table is open memo type
   is detected automatically in all DBF* Ziher's RDDs. If you want
   to change default memo type for new DBF files then use:
      rddInfo( RDDI_MEMOTYPE, DB_MEMO_FPT, "DBFNSX" )
   or:
      rddInfo( RDDI_MEMOTYPE, DB_MEMO_DBT, "DBFNSX" )
16.Ziher NSX implementation in default format supports NSX files
   up to 4 GiB. This is maximum keeping binary compatibility with SIX3
   NSX RDD. But Ziher can support files up to 4 TiB - it's a little
   bit modified format with 'I' instead of 'i' in index header signature.
   Such indexes are created when locking mode is set to 64-bit. On open
   Ziher RDDs automatically recognize type of index files so it
   can use new format also with different locking schemes. SIX3 NSX
   cannot use new index.
17.This version do not have yet any speed optimization in navigation
   and index update code for easier detecting any problems with SIX3
   compatibility. I'll add it later.

LEAF KEY COMPRESSION:
   a. store record number (RecNo)
   b. count duplicated characters
   c. if full key is duplicated then key size = n + 1 and goto @g
   d. count trailing characters to the duplicate limit
   e. if rest of key value (without dup and trail chars) is not empty
      then store it using RLE compression:
      - replace each repeated 3 or more characters with with FF xx yy
      - replace each FF character with FF 01
      - if compressed key size is greater or equal then KEY_SIZE - DupCount
        break compression and store the key as raw data coping source,
        key size = KEY_SIZE - DupCount + n + 2
   f. store number of duplicated characters (DupCount)
   g. store key size (Size)
   Ziher NSX RDD has small extension here and also compressed two
   repeated 0xFF character so instead of FF 01 FF 01 it stores FF 02 FF
   It saves one byte and can be still decoded by SIX3 NSX RDD
*/


#ifndef ZH_RDDNSX_H_
#define ZH_RDDNSX_H_

#include "../dbf_cdx/zh_rdd_dbf.h"

ZH_EXTERN_BEGIN

#define NSX_INDEXEXT        ".nsx"

#define NSX_PAGELEN_BITS      10    /* Size of NSX page in bits */
#define NSX_PAGELEN           (1<<NSX_PAGELEN_BITS)  /* Size of page in NSX file */
#define NSX_MAXEXPLEN         256
#define NSX_MAXKEYLEN         250
#define NSX_MAXTAGS            50
#define NSX_TAGNAME            11
#define NSX_BRANCHPAGE          0
#define NSX_ROOTPAGE            1
#define NSX_LEAFPAGE            2
#define NSX_LEAFKEYOFFSET       6
#define NSX_LEAFSPLITOFFSET   592   /* minimum value to avoid anomalies in some worst cases */
#define NSX_PAGE_BUFFER         8
#define NSX_STACKSIZE          32   /* Maximum page stack size */
#define NSX_SIGNATURE         'i'
#define NSX_SIGNATURE_LARGE   'I'
#define NSX_RLE_CHAR          0xFF
#define NSX_DUMMYNODE         0xFFFFFFFFUL
#define NSX_IGNORE_REC_NUM    0x0UL
#define NSX_MAX_REC_NUM       0xFFFFFFFFUL

#define NSX_ROOTHEAD_HEADSIZE 14
#define NSX_TAGHEAD_HEADSIZE  6


/*
 * type of key expression
 * these are the same values as in CL5.3 extend.api
 * #define S_LNUM             2
 * #define S_DNUM             8
 * #define S_LDATE           32
 * #define S_LOG            128
 * #define S_CHAR          1024
 * #define S_ANYNUM     (S_LNUM | S_DNUM)
 * #define S_ANYEXP     (S_ANYNUM | S_CHAR | S_LDATE | S_LOG)
 */
#define NSX_TYPE_LNUM      0x0002
#define NSX_TYPE_DNUM      0x0008
#define NSX_TYPE_LDATE     0x0020
#define NSX_TYPE_TIMESTAMP 0x0040   
#define NSX_TYPE_LOG       0x0080
#define NSX_TYPE_CHAR      0x0400
#define NSX_TYPE_ANYNUM    ( NSX_TYPE_LNUM | NSX_TYPE_DNUM )
#define NSX_TYPE_ANYEXP    ( NSX_TYPE_ANYNUM | NSX_TYPE_CHAR | NSX_TYPE_LDATE | NSX_TYPE_TIMESTAMP | NSX_TYPE_LOG )

#define NSX_CMP_EXACT      0x00     /* exact comparison */
#define NSX_CMP_PREFIX     0x01     /* prefix comparison */
#define NSX_CMP_DATE       0x02     /* date comparison */

#define NSX_TAG_FULLUPDT   0x00
#define NSX_TAG_PARTIAL    0x01
#define NSX_TAG_TEMPLATE   0x02
#define NSX_TAG_CHGONLY    0x04
#define NSX_TAG_NOUPDATE   0x08
#define NSX_TAG_SHADOW     0x10
#define NSX_TAG_MULTIKEY   0x20

/*
sx_Chill()  => if ( indexFlag & ( NSX_TAG_NOUPDATE | NSX_TAG_TEMPLATE ) == 0 )
                  indexFlag |= NSX_TAG_CHGONLY | NSX_TAG_PARTIAL;
sx_Warm()   => if ( indexFlag & ( NSX_TAG_NOUPDATE | NSX_TAG_TEMPLATE ) == 0 )
                  indexFlag &= ~NSX_TAG_CHGONLY
                  indexFlag |= NSX_TAG_PARTIAL // I do not like this
sx_Freeze() => indexFlag |= NSX_TAG_NOUPDATE | NSX_TAG_PARTIAL
               indexFlag &= ~NSX_TAG_CHGONLY

sx_Thermometer() => if ( NSX_TAG_NOUPDATE | NSX_TAG_TEMPLATE ) -> 4
                    if ( NSX_TAG_CHGONLY )                     -> 3
                    if ( NSX_TAG_PARTIAL )                     -> 2
                    else                                       -> 1
         1 - NSX_TAG_FULLUPDT
         2 - NSX_TAG_PARTIAL
         3 - NSX_TAG_CHGONLY
         4 - NSX_TAG_NOUPDATE | NSX_TAG_TEMPLATE
*/

/* index file structures - defined in ZH_BYTEs to avoid alignment problems */

typedef struct _NSXTAGITEM
{
   ZH_UCHAR TagName[ NSX_TAGNAME + 1 ];  /* name of tag in ASCIIZ */
   ZH_UCHAR TagOffset[ 4 ];              /* Tag header offset */
} NSXTAGITEM;
typedef NSXTAGITEM * LPNSXTAGITEM;

typedef struct _NSXROOTHEADER
{
   ZH_UCHAR Signature[ 1 ];           /* "i" = 0x69 */
   ZH_UCHAR IndexFlags[ 1 ];          /* 0x00 */
   ZH_UCHAR TagCount[ 2 ];            /* number of tags in index file */
   ZH_UCHAR Version[ 2 ];             /* cyclic counter for concurrent access */
   ZH_UCHAR FreePage[ 4 ];            /* offset of first free page in index file */
   ZH_UCHAR FileSize[ 4 ];            /* the index file length */
   NSXTAGITEM TagList[ NSX_MAXTAGS ];
   ZH_UCHAR Unused[ NSX_PAGELEN - 14 - NSX_MAXTAGS * sizeof( NSXTAGITEM ) ];
} NSXROOTHEADER;
typedef NSXROOTHEADER * LPNSXROOTHEADER;

typedef struct _NSXTAGHEADER
{
   ZH_UCHAR Signature[ 1 ];           /* "i" = 0x69 */
   ZH_UCHAR TagFlags[ 1 ];            /* update flags: NSX_TAG_* */
   ZH_UCHAR RootPage[ 4 ];            /* offset of tag root page */
   ZH_UCHAR KeyType[ 2 ];             /* index key type: NSX_TYPE_* */
   ZH_UCHAR KeySize[ 2 ];             /* index key size */
   ZH_UCHAR Unique[ 2 ];              /* 0x0001 for UNIQUE indexes */
   ZH_UCHAR Descend[ 2 ];             /* 0x0001 for descend indexes */
   ZH_UCHAR KeyExpr[ NSX_MAXEXPLEN ]; /* index KEY expression ASCIIZ */
   ZH_UCHAR ForExpr[ NSX_MAXEXPLEN ]; /* index FOR expression ASCIIZ */
   ZH_UCHAR Unused[ NSX_PAGELEN - 14 - NSX_MAXEXPLEN - NSX_MAXEXPLEN ];
} NSXTAGHEADER;
typedef NSXTAGHEADER * LPNSXTAGHEADER;

typedef struct
{
   ZH_UCHAR Signature[ 1 ];           /* "i" = 0x69 */
   ZH_UCHAR TagFlags[ 1 ];            /* update flags: NSX_TAG_* */
   ZH_UCHAR RootPage[ 4 ];            /* offset of tag root page */
} NSXTAGHEADERUPDT;

typedef struct _NSXBRANCHPAGE
{
   ZH_UCHAR NodeID[ 1 ];              /* NSX_BRANCHPAGE | ( lRoot ? NSX_ROOTPAGE : 0 ) */
   ZH_UCHAR RecNoLen[ 1 ];            /* number of bytes for recno in branch keys - seems to be unused */
   ZH_UCHAR KeyCount[ 2 ];            /* number of key in page */
   ZH_UCHAR LowerPage[ 4 ];           /* offset to the page with lower keys */
   ZH_UCHAR KeyData[ NSX_PAGELEN - 8 ];  /* with branch keys */
} NSXBRANCHPAGE;
typedef NSXBRANCHPAGE * LPNSXBRANCHPAGE;

typedef struct _NSXLEAFPAGE
{
   ZH_UCHAR NodeID[ 1 ];              /* NSX_LEAFPAGE | ( lRoot ? NSX_ROOTPAGE : 0 ) */
   ZH_UCHAR RecNoLen[ 1 ];            /* number of bytes for recno in leaf keys */
   ZH_UCHAR KeyCount[ 2 ];            /* number of key in page */
   ZH_UCHAR UsedArea[ 2 ];            /* area used in page -> offset to free area */
   ZH_UCHAR KeyData[ NSX_PAGELEN - NSX_LEAFKEYOFFSET ];  /* with branch keys */
} NSXLEAFPAGE;
typedef NSXLEAFPAGE * LPNSXLEAFPAGE;

#if 0
/* meta structures for description only, cannot be compiled due to
   variable member sizes */
typedef struct _NSXBRANCHKEY
{
   ZH_UCHAR Page[ 4 ];        /* page offset with higher keys values */
   ZH_UCHAR RecNo[ n ];       /* where n is RecNoLen */
   ZH_UCHAR KeyData[ l ];     /* key value where l is KeySize */
} NSXBRANCHKEY;
typedef NSXBRANCHKEY * LPNSXBRANCHKEY;

typedef struct _NSXLEAFKEY
{
   ZH_UCHAR RecNo[ n ];       /* where n is RecNoLen */
   ZH_UCHAR Size[ 1 ];        /* key data size with this byte and n RecNo ZH_BYTEs
                               * if Size == n + 1 then key is fully duplicated
                               */
   ZH_UCHAR DupCount[ 1 ];    /* number of bytes from previous key */
   ZH_UCHAR KeyData[ m ];     /* rest of key value with RLE compression:
                               *    FF xx yy => Replicate(yy, xx)
                               *    FF 01    => FF
                               * m = Size - n - 2
                               * if m == KEY_SIZE - DupCount then key value
                               * is stored as raw data and can be copied as is
                               * if after decompression size of key value is
                               * smaller then KEY_SIZE then rest if filled with
                               * trailing character (\0 for numeric and date
                               * indexes and  SPACE for others (character
                               * and logical))
                               */
} NSXLEAFKEY;
typedef NSXLEAFKEY * LPNSXLEAFKEY;
#endif


/* forward declarations
 */
struct _RDDFUNCS;
struct _NSXAREA;
struct _TAGINFO;
struct _NSXINDEX;

typedef struct _KEYINFO
{
   ZH_ULONG page;     /* page number */
   ZH_ULONG rec;      /* record number */
   int      mode;     /* comparison mode NSX_CMP_* */
   ZH_UCHAR val[ 1 ]; /* key value */
} KEYINFO;
typedef KEYINFO * LPKEYINFO;

typedef struct _TREE_STACK
{
   ZH_ULONG page;
   ZH_SHORT ikey;
   ZH_UCHAR * value;
}  TREE_STACK;
typedef TREE_STACK * LPTREESTACK;

typedef struct _ZH_PAGEINFO
{
   ZH_ULONG   Page;
   ZH_BOOL    Changed;
   int        iUsed;
   ZH_USHORT  uiKeys;
   ZH_USHORT  uiOffset;
   struct _ZH_PAGEINFO * pNext;
   struct _ZH_PAGEINFO * pPrev;
#ifdef ZH_NSX_EXTERNAL_PAGEBUFFER
   union
   {
      ZH_UCHAR *        buffer;
      LPNSXROOTHEADER   rootHeader;
      LPNSXTAGHEADER    tagHeader;
   } data;
#else
   union
   {
      ZH_UCHAR       buffer[ NSX_PAGELEN ];
      NSXROOTHEADER  rootHeader;
      NSXTAGHEADER   tagHeader;
   } data;
#endif
} ZH_PAGEINFO;
typedef ZH_PAGEINFO * LPPAGEINFO;

typedef union
{
   ZH_UCHAR *        buffer;
   LPNSXROOTHEADER   header;
} ZH_NSXPAGEHEAD;

typedef struct _ZH_NSXSCOPE
{
   PZH_ITEM   scopeItem;
   LPKEYINFO  scopeKey;
   ZH_USHORT  scopeKeyLen;
} ZH_NSXSCOPE;
typedef ZH_NSXSCOPE * PZH_NSXSCOPE;

typedef struct _TAGINFO
{
   char *      TagName;
   char *      KeyExpr;
   char *      ForExpr;
   PZH_ITEM    pKeyItem;
   PZH_ITEM    pForItem;
   ZH_NSXSCOPE top;
   ZH_NSXSCOPE bottom;


   ZH_BOOL     fUsrDescend;
   ZH_BOOL     AscendKey;
   ZH_BOOL     UniqueKey;

   ZH_BOOL     Custom;
   ZH_BOOL     ChgOnly;
   ZH_BOOL     Partial;
   ZH_BOOL     Template;
   ZH_BOOL     MultiKey;

   ZH_BOOL     HdrChanged;
   ZH_BOOL     TagBOF;
   ZH_BOOL     TagEOF;
   ZH_BOOL     HotFor;

   ZH_ULONG    HeadBlock;
   ZH_ULONG    RootBlock;

   ZH_UCHAR    TagFlags;
   ZH_UCHAR    KeyType;
   ZH_UCHAR    TrailChar;
   ZH_USHORT   KeyLength;
   ZH_USHORT   nField;
   ZH_USHORT   uiNumber;
   ZH_USHORT   MaxKeys;

   ZH_USHORT   CurKeyOffset;
   ZH_USHORT   CurKeyNo;

   ZH_USHORT   stackSize;
   ZH_USHORT   stackLevel;
   LPTREESTACK stack;

   ZH_ULONG    keyCount;
   LPKEYINFO   CurKeyInfo;
   LPKEYINFO   HotKeyInfo;

   struct     _NSXINDEX * pIndex;
} TAGINFO;
typedef TAGINFO * LPTAGINFO;

typedef struct _NSXINDEX
{
   char *      IndexName;
   char *      RealName;
   ZH_ULONG    Version;       /* index VERSION filed to signal index updates for other stations */
   ZH_ULONG    NextAvail;     /* next free page in index file */
   ZH_ULONG    FileSize;      /* index file size */
   struct     _NSXAREA * pArea;
   PZH_FILE    pFile;
   ZH_BOOL     fDelete;       /* delete on close flag */
   ZH_BOOL     fReadonly;
   ZH_BOOL     fShared;
   ZH_BOOL     fFlush;
   ZH_BOOL     LargeFile;
   ZH_BOOL     Changed;
   ZH_BOOL     Update;
   ZH_BOOL     Production;    /* Production index */
   ZH_DBFLOCKDATA lockData;   /* index lock data */
   int         lockWrite;     /* number of write lock set */
   int         lockRead;      /* number of read lock set */

   NSXROOTHEADER  HeaderBuff;
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

   struct     _NSXINDEX * pNext;   /* The next index in the list */
} NSXINDEX;
typedef NSXINDEX * LPNSXINDEX;

/* for index creation */
typedef struct
{
   ZH_FOFFSET  nOffset;    /* offset in temporary file */
   ZH_ULONG    ulKeys;     /* number of keys in page */
   ZH_ULONG    ulKeyBuf;   /* number of keys in memory buffer */
   ZH_ULONG    ulCurKey;   /* current key in memory buffer */
   ZH_UCHAR *  pKeyPool;   /* memory buffer */
} NSXSWAPPAGE;
typedef NSXSWAPPAGE * LPNSXSWAPPAGE;

typedef struct
{
   LPTAGINFO  pTag;           /* current Tag */
   PZH_FILE   pTempFile;      /* handle to temporary file */
   char *     szTempFileName; /* temporary file name */
   int        keyLen;         /* key length */
   ZH_UCHAR   trailChar;      /* index key trail character */
   ZH_UCHAR   recSize;        /* record size in leaf keys */
   ZH_BOOL    fUnique;        /* ZH_TRUE if index is unique */
   ZH_BOOL    fReindex;       /* ZH_TRUE if reindexing is in process */
   ZH_ULONG   ulMaxRec;       /* the highest record number */
   ZH_ULONG   ulTotKeys;      /* total number of keys indexed */
   ZH_ULONG   ulKeys;         /* keys in currently created page */
   ZH_ULONG   ulPages;        /* number of pages */
   ZH_ULONG   ulCurPage;      /* current page */
   ZH_ULONG   ulPgKeys;       /* maximum number of key in page memory buffer */
   ZH_ULONG   ulMaxKey;       /* maximum number of keys in single page */
   ZH_UCHAR * pKeyPool;       /* memory buffer for current page then for pages */
   ZH_UCHAR * pStartKey;      /* beginning of key pool after sorting */
   LPNSXSWAPPAGE pSwapPage;   /* list of pages */
   LPPAGEINFO NodeList[ NSX_STACKSIZE ]; /* Stack of pages */
   ZH_ULONG   ulFirst;
   ZH_ULONG * pSortedPages;
   ZH_UCHAR   pLastKey[ NSX_MAXKEYLEN ]; /* last key val */
   ZH_ULONG   ulLastRec;
   ZH_ULONG   ulLastLeaf;     /* last non empty leaf page written to tag */

   ZH_UCHAR * pBuffIO;        /* index IO buffer */
   ZH_ULONG   ulSizeIO;       /* size of IO buffer in index pages */
   ZH_ULONG   ulPagesIO;      /* number of index pages in buffer */
   ZH_ULONG   ulFirstIO;      /* first page in buffer */
   ZH_ULONG   ulLastIO;       /* last page in buffer */
} NSXSORTINFO;
typedef NSXSORTINFO * LPNSXSORTINFO;

/*
 *  DBF WORKAREA
 *  ------------
 *  The Workarea Structure of DBFNSX RDD
 *
 */

typedef struct _NSXAREA
{
   DBFAREA dbfarea;

   /*
   *  NSX's additions to the workarea structure
   *
   *  Warning: The above section MUST match DBFAREA exactly! Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   ZH_BOOL        fIdxAppend;       /* ZH_TRUE if new record is added */
   ZH_BOOL        fSetTagNumbers;   /* Tag number should be recreated */
   LPNSXINDEX     lpIndexes;        /* Pointer to list of indexes */
   LPTAGINFO      lpCurTag;         /* Pointer to current order */
   LPNSXSORTINFO  pSort;            /* Index build structure */

} NSXAREA;
typedef NSXAREA * LPNSXAREA;

#ifndef NSXAREAP
#define NSXAREAP LPNSXAREA
#endif

#undef  SUPERTABLE
#define SUPERTABLE                         ( &nsxSuper )

ZH_EXTERN_END

#endif /* ZH_RDDNSX_H_ */
