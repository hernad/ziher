/*
 * HiPer-SEEK / CFTS compatible library
 *
 * Copyright 2005 Przemyslaw Czerpak <druzus@acn.waw.pl>
 *
 * Credits:
 *    Many thanks for Mindaugas Kavaliauskas for his assistance,
 *    information about HSX internals, code checking and general
 *    help in many things when this library was written.
 *                                                          Przemek.
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
   LOCKING/IO operations done by HiPpe-SEEK/CFTS library:
      A. in exclusive mode:
         Unimportant. Though tests shows that CFTS uses buffers
         only in ADD and NEXT operations. Other causes immediate
         IO call
      B. in shared mode
         OPEN:
            1. Test for lock existing - (open file, check length,
               lock one byte at offset equal to file size multiple
               by three, unlock it and close the file)
            2. Open file
            3. Check file length
            4. lock header area (@0:512)
            5. read header
            6. unlock header area (@0:512)
         KEYCOUNT:
            1. Lock header area (@0:512)
            2. read 4 bytes with record count from header (@0:4)
            3. unlock header area (@0:512)
         ADD:
            1. lock header area (@0:512)
            2. check file length
            3. lock the new record area (@file_length:record_size)
            4. read 4 bytes with record count from header (@0:4)
            5. write 4 bytes with record count from header (@0:4)
            6. write new records (@file_length:record_size)
            7. flush system buffers (COMMIT)
               !!! write 4096 bytes at offset 0 (I guess it's system call,
               result of calling FLUSH function because file size is not
               increased, so it's simple disk cluster/inode update)
            8. unlock header area (@0:512)
            9. unlock record area
         IFDEL:
            1. lock header area (@0:512)
            2. read 4 bytes with record count from header (@0:4)
            3. unlock header area (@0:512)
            4. read first byte of record (to check DELETED bit)
         DELETE(/UNDELETE):
            1. lock header area (@0:512)
            2. read 4 bytes with record count from header (@0:4)
            3. unlock header area (@0:512)
            4. read first byte of record (to check DELETED bit)
               if record not deleted (/is deleted) stop here
            5. lock the record area (@record_offset:record_size)
            6. write first byte to record (the values read in 4
               with updated DELETE flag
            7. flush system buffers (COMMIT) - the same effect as in ADD
            8. unlock the record area (@record_offset:record_size)
         REPLACE:
            1. lock header area (@0:512)
            2. read 4 bytes with record count from header (@0:4)
            3. unlock header area (@0:512)
            4. write the new record value (DELETE flag is not set)
            5. flush system buffers (COMMIT) - the same effect as in ADD
            6. unlock the record area (@record_offset:record_size)
         SET: nothing
         NEXT:
            1. if there is no record in the buffer read records to
               fill buffer size (1024 bytes by default)


         It's very complicated and I do not see big sense in all
         this operations - because the following scheme is enough
         for secure ADD operation.
            1. LOCK THE HEADER AREA (@x0000:0x0200)
            2. SEEK FROM EOF TO CHECK FILE SIZE AND COUNT THE RECORDS
            3. WRITE NEW RECORD (@RECOFFST:RECSIZE)
            4. WRITE NEW RECORD COUNTER TO HEADER (@0x0000:0x0004)
            5. UNLOCK THE HEADER AREA (@x0000:0x0200)
         Even the point 4 can be eliminated because the new record
         number is counted from file size.
         REPLACE does not need any locks because the whole record is
         overwritten with new value in single IO operation
         IFDEL also does not need any locks.
         DELETE and UNDELETE are not safe operation in HiPer-SEEK/CFTS.
         There is a race condition which may cause that the first byte
         of new record value set by other station will be overwritten
         by the old one with changed DELETED flag. TO make it really
         safe the whole operation should be covered by lock and the same
         lock should be used also by replace. The question is if this
         is really important. So we have to decide here if we should
         use exclusive lock on record area kept for whole: DELETE/UNDELETE/
         REPLACE operations or to not use any locks at all.
         The last important notice is that if we set that automatic HSX
         index update by RDD which uses exclusive record locking for update
         (e.g. DBF and related) then we do not have to set _any_ locks at
         all and we can use _only_ the REPLACE operation hacked to not
         check file size so the index will be automatically growing up by
         writing in the new offset related to appended records.
         Conclusion: it's enough to add single call to zh_hsxReplace()
         in GOCOLD() RDD method.

   Collecting the above information I do not see big sense to implement
   exact HiPer-SEEK/CFTS locking. They does not give anything (I could
   accept them only if they pass mandatory locking scheme) - the race
   conditions still exists and they causes very big general slowness.
   So I'm dropping it. If someone wants to implement it then please go
   on - IMHO it's a waste of time.
   For sure we need lock in ADD operation when HSX is not updated
   automatically by RDD and it is discussable if we should cover by common
   record lock updating the record area (ADD/REPLACE/DELETE/UNDELETE)
   due to possible race condition in DELETE/UNDELETE operations. It will
   have to cause noticeable speed reducing but makes these operations
   always safe though it's very seldom that it can happen in real
   application so maybe we should left it for user. Also there is a side
   effect of settings exclusive locks on non POSIX systems (DOS/Windows)
   They blocks other stations against reading from the locked region. It
   means that the original HS/CFTS locking schemes is buggy because it can
   cause unexpected errors in NEXT operation. To avoid this problem many
   systems use "phantom locking" (e.g. DBF/CDX/NTX locks). If we want to
   use locks we should care about it. Now I made it safe by setting
   exclusive lock on the header area for each (whole to eliminate race
   condition) update operation and shared lock for header reading.
   The results gives working and really network (concurrent access) safe
   HSX access in all operations though when the file is shared with program
   which uses original HiPer-SEEK/CFTS library some HSX_BADREAD errors can
   be returned by HS_NEXT and HS_IFDEL functions and should be served by
   user. The same effect appears if the file is shared only by original
   HiPer-SEEK/CFTS programs and it's a side effect of badly designed
   locking scheme. When only xZiher application access the file this
   problem does not exist.
 */

/* DIFFs:
   1. HS_INDEX copy deleted flag from DBF to HSX index and ignores
      any filters (standard and MachSIX ones)
   2. HS_INDEX accepts as key parameter also code block.
      Given key expression is remembered and later can be used by
      HS_ADD, HS_REPLACE, HS_FILTER and HS_VERIFY when this functions
      are called without key expression. When the key expression is
      set as string then it is also stored in HSX header and later
      is automatically retrieve by HS_OPEN.
   3. HS_CREATE has optional 6th parameter with key expression. It works
      in the same way as key parameter in HS_INDEX.
   4. other functions which accept the index key can receive it as
      direct the key value (string item) or codeblock
   5. I introduced two new error codes: HSX_NOTABLE, HSX_RDDFAILURE
      which are related to workarea errors
   6. The literal version passed to HS_SET is remembered and can be
      later used by HS_VERIFY if not given explicitly.
   7. HS_VERIFY respects the lCase flag (fixed SIX bug) and also cftsVeri()
      syntax (first parameter in numeric indicating the HSX handler)
      See also the note about HS_SET.
   8. HS_FILTER respects the filter flags in verification process, it
      also can accept handle to already open HSX index as first parameter
      instead of file name. This function needs RDD with record map (RM)
      functionality
   8. HS_ADD and HS_REPLACE have optional additional logical parameter
      which allow to set DELETE flag in new/modified record
   9. Mindaugas noticed me that tests shows CFTS effectively
      uses only the part of string to first Chr(0) byte.
      In first version this behavior was emulated but later I read
      in CFTS documentation that behavior for strings with
      embedded 0 is undefined (so it was not intentionally designed)
      and I decided to make it independent of embedded '0' and removed
      this limitation.
   10.SET DEFAULT and SET PATH is respected by xZiher when in SIX doesn't.
   11.xZiher accepts nFilterType == 3 what means that national characters
      in VM codepage are respected and lCase switch works properly
 */

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_fs_api.h"
#include "zh_rdd_api.h"
#include "zh_error_api.h"
#include "zh_vm.h"
#include "zh_thread.h"
#include "zh_set.h"
#include "zh_codepage_api.h"

/* error codes */
#define HSX_SUCCESSFALSE    0    /* operation finished successfully with false value */
#define HSX_SUCCESS         1    /* operation finished successfully with true value */
#define HSX_CREATEFAIL      -1   /* unable to create the file specified */
#define HSX_MEMERR          -2   /* unable to allocate the memory */
#define HSX_BADHDRWRITE     -3   /* write error while writing the index file header */
#define HSX_BADSEEK         -4   /* Error while attempting seek during buffer flushing */
#define HSX_BADREAD         -5   /* read error while reading */
#define HSX_BADWRITE        -6   /* Error while attempting write during buffer flush */
#define HSX_RECBOUND        -7   /* record number is not valid */
#define HSX_ISDELETED       -8   /* record number is already marked as deleted */
#define HSX_NOTDELETED      -9   /* record number is not marked as deleted */
#define HSX_OPENERR         -10  /* unable to open the file */
#define HSX_INTERR          -11  /* Internal Error */
#define HSX_NORECS          -13  /* index file empty */
#define HSX_BADPARMS        -16  /* Invalid parameters were passed to the function */
#define HSX_NOMOREHANDLES   -17  /* Ran out of HiPer-SEEK handles */
#define HSX_BADHANDLE       -18  /* Invalid handle was passed to the function */
#define HSX_BADIHANDLE      -19  /* Invalid internal handle */
#define HSX_LOCKFAILED      -20  /* Unable to lock file */
#define HSX_NOMORELOCKS     -21  /* Lock table exhausted */
#define HSX_CANNOTUNLOCK    -22  /* Unable to unlock file */
#define HSX_BADCOMMIT       -23  /* Unable to flush disk buffers */
#define HSX_NOTABLE         -24  /* no open table */
#define HSX_RDDFAILURE      -25  /* RDD error */

#define HSX_FILEEXT         ".hsx"

#define HSXMAXKEY_SIZE      3 /* maximum key size */
#define HSXDEFKEY_SIZE      2 /* default key size */
#define HSXDEFOPENMODE      2 /* default open mode 2=SHARED+READONLY */

#define HSXDEFFILTER        1 /* default character filter */

#define HSXHEADER_LEN       512L
#define HSXKEYEXP_LEN       ( 512 - sizeof( HSXHEADER ) )
#define HSXMINBUF_LEN       512L   /* minimum buffer size */
#define HSXMAXBUF_LEN       64536L /* maximum buffer size */
#define HSXDEFBUF_LEN       16384L /* default buffer size */
#define HSX_HALLOC          64     /* the handles' array resize factor - unlike
                                      in SIX number of handles isn't limited */

#define HSX_VERIFY_BEGIN    1
#define HSX_VERIFY_END      2
#define HSX_VERIFY_AND      3
#define HSX_VERIFY_PHRASE   4

#define HSX_HDRLOCKPOS      0
#define HSX_HDRLOCKSIZE     HSXHEADER_LEN

#define HSX_READLOCK        1
#define HSX_WRITELOCK       2
#define HSX_UPDATELOCK      3
#define HSX_APPENDLOCK      4
#define HSX_HDRREADLOCK     5
#define HSX_HDRWRITELOCK    6
#define HSX_READUNLOCK      7
#define HSX_WRITEUNLOCK     8
#define HSX_UPDATEUNLOCK    9
#define HSX_APPENDUNLOCK    10
#define HSX_HDRREADUNLOCK   11
#define HSX_HDRWRITEUNLOCK  12

typedef struct _HSXHEADER
{
   ZH_BYTE recCount[ 4 ];      /* number of records in HSX index file */
   ZH_BYTE recSize[ 4 ];       /* in bytes 16, 32, 64 */
   ZH_BYTE recSizeBits[ 4 ];   /* 4, 5 or 6 */
   ZH_BYTE ignoreCase[ 2 ];    /* 1=> index is not case sensitive */
   ZH_BYTE filterType[ 2 ];    /* 1=> all characters, 2=> chars in range 33..126 */
   ZH_BYTE hashLetters[ 4 ];   /* 1=> use hash function for letters */
   ZH_BYTE keyExpression[ 1 ]; /* xZiher extension: key expression for automatic update */
} HSXHEADER;
typedef HSXHEADER * LPHSXHEADER;

typedef union
{
   ZH_BYTE   data[ HSXHEADER_LEN ];
   HSXHEADER header;
} HSXHEADERBUF;

typedef struct _HSXINFO
{
   int       iHandle;           /* HSX handle */
   ZH_ULONG  ulRecCount;        /* number of records */
   ZH_USHORT uiRecordSize;      /* record size in bytes */
   ZH_BOOL   fIgnoreCase;       /* ignore case */
   int       iFilterType;       /* character filter */
   ZH_BOOL   fUseHash;          /* use Hash functions for alphas */

   PZH_FILE  pFile;             /* file handle */
   char *    szFileName;        /* file name */
   ZH_BOOL   fShared;           /* Shared file */
   ZH_BOOL   fReadonly;         /* Read only file */
   ZH_ULONG  ulBufSize;         /* size of buffer in records */
   ZH_ULONG  ulBufRec;          /* number of record in buffer */
   ZH_ULONG  ulFirstRec;        /* first record in the buffer */
   ZH_BYTE * pBuffer;           /* the buffer pointer */
   ZH_BOOL   fChanged;          /* the buffer is changed and should be written to index file */
   ZH_BOOL   fHdrChanged;       /* new records, header file has to be updated */
   ZH_BOOL   fWrLocked;         /* the index is locked for writing */

   char *    pSearchVal;        /* current search value for HS_NEXT */
   ZH_SIZE   nSearch;           /* the length of search value */
   ZH_BYTE * pSearchKey;        /* current search key val for HS_NEXT */
   ZH_ULONG  ulCurrRec;         /* current record for HS_NEXT */

   /* xZiher extension */
   int       iArea;             /* work area number if bound with WA or 0 */
   char *    szKeyExpr;         /* key expression when bound with WA for automatic update */
   PZH_ITEM  pKeyItem;          /* item with compiled key expression */
   ZH_BOOL   fFlush;            /* data was written to file and not committed */
} HSXINFO;
typedef HSXINFO * LPHSXINFO;


typedef struct
{
   int         iHandleCount;  /* number of active HSX indexes */
   int         iHandleSize;   /* size of handle array */
   LPHSXINFO * handleArray;   /* array indexed by handle number with HSXINFO pointers */
}
HSXTABLE, * LPHSXTABLE;

#if defined( ZH_HSX_TSDSTORE )

#include "zh_stack.h"

#define ZH_HSX_LOCK()    do {} while( 0 )
#define ZH_HSX_UNLOCK()  do {} while( 0 )

static int zh_hsxDestroy( int iHandle );

static void zh_hsxTableRelease( void * Cargo )
{
   LPHSXTABLE pTable = ( LPHSXTABLE ) Cargo;
   int iHandle;

   for( iHandle = 0; iHandle < pTable->iHandleSize; ++iHandle )
   {
      if( pTable->handleArray[ iHandle ] )
         zh_hsxDestroy( iHandle );
   }
}

static ZH_TSD_NEW( s_hsxTable, sizeof( HSXTABLE ), NULL, zh_hsxTableRelease );

static LPHSXTABLE zh_hsxTable( void )
{
   return ( LPHSXTABLE ) zh_stackGetTSD( &s_hsxTable );
}
#else

static HSXTABLE s_hsxTable;
#define zh_hsxTable()  ( &s_hsxTable )

static ZH_CRITICAL_NEW( s_hsxMtx );
#define ZH_HSX_LOCK()    zh_threadEnterCriticalSection( &s_hsxMtx )
#define ZH_HSX_UNLOCK()  zh_threadLeaveCriticalSection( &s_hsxMtx )

#endif

/* the conversion table for ASCII alpha pairs */
static const ZH_UCHAR zh_hsxHashArray[] = {
/*        A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   W   V   X   Y   Z */
/* A */   7,102,222,185, 19, 48,167,  4,173,  4, 79,251,194,250,  7,187,  7,251,209,249, 41,101, 39, 29, 71, 40,
/* B */ 156,  3,  7,  7,149,  7,  7,  7,172,  7,  7,100,  7,  7,148,  7,  7,107, 38,  7,126,  7,  7,  7,  7,  7,
/* C */ 234,  7, 38,  7,229,  7,  7,208,145,  7,116,106,  7,  7,253,  7,  7,166, 40,237,129,  7,  7,  7, 63,  4,
/* D */ 125,  4,  4, 29,253,  7, 28,  7,226,  7,  7,  3,  3,  4,128,  7,  7,124, 44,  4,115,  7,  4,  7, 37,  7,
/* E */ 193, 37,236,198,114, 94,105,  3, 44,  7,  4,245,159,251, 93,151, 36,248,253,252, 36, 70, 28,147, 19,  4,
/* F */  92,  7,  7,  7,123, 78,  7,  7,180,  7,  7,150,  7,  7,122,  7,  7,104,  4, 35, 55,  7,  7,  7,  7,  7,
/* G */ 121,  7,  7,  7,195,  7,  2, 86, 77,  7,  7, 85,  2, 76, 55,  7,  7,179, 27,  4, 54,  7,  7,  7, 63,  7,
/* H */ 197,  7,  7,  7,228,  7,  7,  7,164,  7,  7, 18,  4,  1,220,  7,  7, 99,  7, 62, 35,  7,  7,  7,169,  7,
/* I */ 192, 98,250,207,155,143,158,  1,  7,  7,  1,212,163,248,250, 97,  7,178,225,252,142,120,  7,  4,  7, 84,
/* J */   4,  7,  7,  7, 34,  7,  7,  6,  6,  6,  6,  6,  6,  6, 15,  6,  6,  6,  6,  6, 34,  6,  6,  6,  6,  6,
/* K */  15,  6,  6,  6,135,  6,  6,  6, 69,  6,  6,  4,  6, 14, 14,  6,  6,  6, 14,  6,  6,  6,  6,  6, 27,  6,
/* L */ 253,  4, 13, 62,251, 18,  4,  6,255,  6, 26,213, 17,  6,238, 13,  6,  6, 83,162,154, 12,  6,  6,134,  6,
/* M */ 216, 54,  6,  6,254,  6,  6,  6,231,  6,  6,  6, 68, 12,223,140,  6,  6,  4,  6,146,  6,  6,  6,  4,  6,
/* N */ 230,  6,204,202,252, 53,246,  6,227,  4, 53,  4,  4, 43,205,  4,  6, 11,201,251, 75, 52, 11,  6, 33,  4,
/* O */  74, 96,161,171, 33, 73,168, 17,133,  4, 10,243,244,248, 82,219,  6,250,210,215,191, 52,119, 51, 32,  4,
/* P */ 186,  6,  6,  6,232,  6,  6,224,160,  6,  6,190,  6,  6,217, 26,  6,189, 32, 90, 67,  6,  6,  6, 51,  6,
/* Q */   6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,111,  6,  6,  6,  6,  6,  6,
/* R */ 249, 89,117,113,250, 47,110, 47,252,  6, 46, 25,199,112,249,109,  6,139,183,144,138, 50, 10,  6,153,  6,
/* S */  81,  6,175,  5,218,  9,  5,170,247,  5, 43, 66, 50, 16,206,177,  4,  5,176,252,182,  5,  9,  5, 61,  5,
/* T */ 242,  5, 16,  4,249,  5,  5,221,248,  5,  5, 25,  8,  5,241,  5,  5,250, 49,132,152,  5,  4,  5,181,  4,
/* U */  80, 95, 88, 61, 60,  8, 46,  5, 60,  5,  5,214,196,184, 45,131,  5,203,188,174,  5,  5,  5,  4,  5,  5,
/* V */ 137,  5,  5,  5,200,  5,  5,  5,130,  5,  5,  5,  5,  5, 49,  5,  5,  5,  5,  5,  4,  5,  5,  5,  5,  5,
/* W */ 136,  5,  5,  5, 65,  5,  5, 31, 59,  5,  4,  4,  5, 23, 58,  5,  5,  4,  4,  5,  5,  5,  5,  5,  5,  5,
/* X */   4,  5, 23,  5, 31,  5,  5,  5, 24,  5,  5,  5,  5,  5, 22, 22,  5,  5,  5, 45,  5,  5,  5,  5, 30,  5,
/* Y */  30, 21, 42, 72, 21,  5,  4,  5,  4,  4,  4,127, 20,103, 20, 87,  4, 64,108,  4,  4,  4,  4,  4,  4,  4,
/* Z */  42,  4,  4,  4, 56,  4,  4,  4, 24,  4,  4,  4,  4,  4, 41,  4,  4,  4,  4,  4,  4,  4,  4,  4,  3,  4
};

static int zh_hsxHashVal( int c1, int c2, int iKeyBits,
                          ZH_BOOL fNoCase, int iFilter, ZH_BOOL fUseHash )
{
   int iBitNum;

   if( fNoCase )
   {
      if( iFilter == 3 )
      {
         PZH_CODEPAGE cdp = zh_vmCDP();
         c1 = ( ZH_UCHAR ) cdp->upper[ c1 ];
         c2 = ( ZH_UCHAR ) cdp->upper[ c2 ];
      }
      else
      {
         if( c1 >= 'a' && c1 <= 'z' )
            c1 -= 'a' - 'A';
         if( c2 >= 'a' && c2 <= 'z' )
            c2 -= 'a' - 'A';
      }
   }
   if( iFilter == 1 )
   {
      c1 &= 0x7F;
      if( c1 < 0x20 || c1 == 0x7f )
         c1 = ' ';
      c2 &= 0x7F;
      if( c2 < 0x20 || c2 == 0x7f )
         c2 = ' ';
   }

   if( c1 == ' ' || c2 == ' ' || c1 == 0 || c2 == 0 )
      iBitNum = 0;
   else if( fUseHash && c1 >= 'A' && c1 <= 'Z' && c2 >= 'A' && c2 <= 'Z' )
   {
      iBitNum = zh_hsxHashArray[ ( c1 - 'A' ) * 26 + ( c2 - 'A' ) ] + 1;
   }
   else
   {
      iBitNum = ( c1 + c2 * 78 ) % ( iKeyBits - 1 ) + 1;
      if( iBitNum == 1 )
         iBitNum++;
   }
   return iBitNum;
}

static void zh_hsxHashStr( const char * pStr, ZH_SIZE nLen, ZH_BYTE * pKey, int iKeySize,
                           ZH_BOOL fNoCase, int iFilter, ZH_BOOL fUseHash )
{
   int c1, iKeyBits = iKeySize << 3;

   memset( pKey, '\0', iKeySize );
#if 0
/* This code keeps the strict CFTS behavior which stops string
   manipulating at first Chr(0) character */
   if( pStr && nLen-- && ( c1 = ( ZH_UCHAR ) *pStr++ ) != 0 )
   {
      int c2;
      while( nLen-- && ( c2 = ( ZH_UCHAR ) *pStr++ ) != 0 )
      {
#else
   /* This version can work well with embedded 0 characters */
   if( pStr && nLen-- )
   {
      c1 = ( ZH_UCHAR ) *pStr++;
      while( nLen-- )
      {
         int c2 = ( ZH_UCHAR ) *pStr++;
#endif
         int iBitNum = zh_hsxHashVal( c1, c2, iKeyBits, fNoCase, iFilter, fUseHash );
         if( iBitNum-- )
         {
            pKey[ iBitNum >> 3 ] |= 0x80 >> ( iBitNum & 7 );
         }
         c1 = c2;
      }
   }
}

static int zh_hsxStrCmp( const char * pSub, ZH_SIZE nSub, const char * pStr, ZH_SIZE nLen,
                         ZH_BOOL fNoCase, int iFilter )
{
   ZH_BOOL fResult = ZH_FALSE;
   ZH_UCHAR c1, c2;

   if( nSub == 0 )
      return HSX_SUCCESSFALSE;

   while( ! fResult && nLen >= nSub )
   {
      ZH_SIZE nPos;
      fResult = ZH_TRUE;
      for( nPos = 0; fResult && nPos < nSub; nPos++ )
      {
         c1 = ( ZH_UCHAR ) pSub[ nPos ];
         c2 = ( ZH_UCHAR ) pStr[ nPos ];
         if( fNoCase )
         {
            if( iFilter == 3 )
            {
               PZH_CODEPAGE cdp = zh_vmCDP();
               c1 = ( ZH_UCHAR ) cdp->upper[ c1 ];
               c2 = ( ZH_UCHAR ) cdp->upper[ c2 ];
            }
            else
            {
               if( c1 >= 'a' && c1 <= 'z' )
                  c1 -= 'a' - 'A';
               if( c2 >= 'a' && c2 <= 'z' )
                  c2 -= 'a' - 'A';
            }
         }
#if 0
/* This code is for strict cftsVeri() behavior - uncomment if necessary
   but it's IMHO bug */
         if( iFilter == 1 )
         {
            c1 &= 0x7F;
            if( c1 < 0x20 || c1 == 0x7f )
               c1 = ' ';
            c2 &= 0x7F;
            if( c2 < 0x20 || c2 == 0x7f )
               c2 = ' ';
         }
#endif
         fResult = ( c1 == c2 );
      }
      --nLen;
      ++pStr;
   }

   return fResult ? HSX_SUCCESS : HSX_SUCCESSFALSE;
}

static LPHSXINFO zh_hsxGetPointer( int iHandle )
{
   LPHSXINFO pHSX = NULL;

   ZH_HSX_LOCK();
   {
      LPHSXTABLE pTable = zh_hsxTable();
      if( iHandle >= 0 && iHandle < pTable->iHandleSize )
         pHSX = pTable->handleArray[ iHandle ];
   }
   ZH_HSX_UNLOCK();

   return pHSX;
}

static int zh_hsxCompile( const char * szExpr, PZH_ITEM * pExpr )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   *pExpr = NULL;
   if( pArea )
   {
      if( SELF_COMPILE( pArea, szExpr ) == ZH_FAILURE )
         return HSX_BADPARMS;
      *pExpr = pArea->valResult;
      pArea->valResult = NULL;
   }
   else
   {
      PZH_MACRO pMacro = zh_macroCompile( szExpr );
      if( ! pMacro )
         return HSX_BADPARMS;
      *pExpr = zh_itemPutPtr( NULL, ( void * ) pMacro );
   }
   return HSX_SUCCESS;
}

static int zh_hsxEval( int iHandle, PZH_ITEM pExpr, ZH_BYTE * pKey, ZH_BOOL * fDeleted )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );
   int iResult = HSX_SUCCESS;
   const char * pStr;
   ZH_SIZE nLen;

   if( ! pHSX )
      return HSX_BADHANDLE;

   if( ! pExpr )
      pExpr = pHSX->pKeyItem;

   if( ! pExpr )
      return HSX_BADPARMS;

   if( ZH_IS_STRING( pExpr ) )
   {
      pStr = zh_itemGetCPtr( pExpr );
      nLen = zh_itemGetCLen( pExpr );
      if( fDeleted )
         *fDeleted = ZH_FALSE;
   }
   else
   {
      int iArea = 0;
      PZH_ITEM pItem;

      if( pHSX->iArea != 0 )
      {
         iArea = zh_rddGetCurrentWorkAreaNumber();
         if( iArea != pHSX->iArea )
            zh_rddSelectWorkAreaNumber( pHSX->iArea );
         else
            iArea = 0;
      }
      pItem = zh_vmEvalBlockOrMacro( pExpr );
      pStr = zh_itemGetCPtr( pItem );
      nLen = zh_itemGetCLen( pItem );
      if( fDeleted )
      {
         AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
         if( ! pArea )
            *fDeleted = ZH_FALSE;
         else if( SELF_DELETED( pArea, fDeleted ) == ZH_FAILURE )
            iResult = HSX_RDDFAILURE;
      }
      if( iArea )
         zh_rddSelectWorkAreaNumber( iArea );
      if( zh_vmRequestQuery() )
         iResult = HSX_BADPARMS;
   }

   if( iResult == HSX_SUCCESS )
      zh_hsxHashStr( pStr, nLen, pKey, pHSX->uiRecordSize, pHSX->fIgnoreCase,
                     pHSX->iFilterType, pHSX->fUseHash );

   return iResult;
}

static void zh_hsxGetRecCount( LPHSXINFO pHSX )
{
   pHSX->ulRecCount = ( ZH_ULONG ) ( ( zh_fileSize( pHSX->pFile ) -
                                       HSXHEADER_LEN ) / pHSX->uiRecordSize );
}

static int zh_hsxHdrFlush( int iHandle )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );

   if( ! pHSX )
      return HSX_BADHANDLE;

   if( pHSX->fHdrChanged )
   {
      ZH_USHORT uiBits = 0, uiSize = pHSX->uiRecordSize;
      HSXHEADERBUF buffer;

      while( uiSize >>= 1 )
         uiBits++;

      ZH_PUT_LE_UINT32( buffer.header.recCount,    pHSX->ulRecCount );
      ZH_PUT_LE_UINT32( buffer.header.recSize,     ( ZH_U32 ) pHSX->uiRecordSize );
      ZH_PUT_LE_UINT32( buffer.header.recSizeBits, ( ZH_U32 ) uiBits );
      ZH_PUT_LE_UINT16( buffer.header.ignoreCase,  pHSX->fIgnoreCase ? 1 : 0 );
      ZH_PUT_LE_UINT16( buffer.header.filterType,  pHSX->iFilterType );
      ZH_PUT_LE_UINT32( buffer.header.hashLetters, pHSX->fUseHash ? 1 : 0 );

      memset( buffer.header.keyExpression, 0, HSXKEYEXP_LEN + 1 );
      if( pHSX->szKeyExpr )
         zh_strncpy( ( char * ) buffer.header.keyExpression, pHSX->szKeyExpr, HSXKEYEXP_LEN );

      if( zh_fileWriteAt( pHSX->pFile, buffer.data, HSXHEADER_LEN, 0 ) != HSXHEADER_LEN )
         return HSX_BADHDRWRITE;

      pHSX->fHdrChanged = ZH_FALSE;
      pHSX->fFlush = ZH_TRUE;
   }
   return HSX_SUCCESS;
}

static int zh_hsxFlush( int iHandle )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );

   if( ! pHSX )
      return HSX_BADHANDLE;

   if( pHSX->fChanged )
   {
      ZH_FOFFSET fOffset;
      ZH_SIZE nSize;

      fOffset = ( ZH_FOFFSET ) HSXHEADER_LEN +
                ( ZH_FOFFSET ) ( pHSX->ulFirstRec - 1 ) *
                ( ZH_FOFFSET ) pHSX->uiRecordSize;
      nSize = pHSX->ulBufRec * pHSX->uiRecordSize;

      if( zh_fileWriteAt( pHSX->pFile, pHSX->pBuffer, nSize, fOffset ) != nSize )
         return HSX_BADWRITE;

      pHSX->fChanged = ZH_FALSE;
      pHSX->fFlush = ZH_TRUE;
   }
   return HSX_SUCCESS;
}

static int zh_hsxFlushAll( int iHandle )
{
   int iRetVal;

   iRetVal = zh_hsxFlush( iHandle );
   if( iRetVal == HSX_SUCCESS )
      iRetVal = zh_hsxHdrFlush( iHandle );

   return iRetVal;
}

static int zh_hsxHdrRead( int iHandle )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );
   HSXHEADERBUF buffer;
   int iResult = HSX_SUCCESS;

   if( ! pHSX )
      return HSX_BADHANDLE;

   if( zh_fileReadAt( pHSX->pFile, buffer.data, HSXHEADER_LEN, 0 ) != HSXHEADER_LEN )
      return HSX_BADREAD;

   pHSX->ulRecCount = ZH_GET_LE_UINT32( buffer.header.recCount );
   pHSX->uiRecordSize = ( ZH_USHORT ) ZH_GET_LE_UINT32( buffer.header.recSize );
   pHSX->fIgnoreCase = ZH_GET_LE_UINT16( buffer.header.ignoreCase ) != 0;
   pHSX->iFilterType = ZH_GET_LE_UINT16( buffer.header.filterType );
   pHSX->fUseHash = ZH_GET_LE_UINT32( buffer.header.hashLetters ) != 0;

   if( buffer.header.keyExpression[ 0 ] >= ' ' )
   {
      buffer.data[ HSXHEADER_LEN - 1 ] = '\0';
      pHSX->szKeyExpr = zh_strdup( ( char * ) buffer.header.keyExpression );
      iResult = zh_hsxCompile( pHSX->szKeyExpr, &pHSX->pKeyItem );
   }

   /* update the record counter */
   zh_hsxGetRecCount( pHSX );

   return iResult;
}

static int zh_hsxRead( int iHandle, ZH_ULONG ulRecord, ZH_BYTE ** pRecPtr )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );
   ZH_BOOL fCount;

   if( ! pHSX )
      return HSX_BADHANDLE;

   fCount = pHSX->fShared;

   if( ulRecord > pHSX->ulRecCount && fCount )
   {
      zh_hsxGetRecCount( pHSX );
      fCount = ZH_FALSE;
   }

   if( ulRecord == 0 || ulRecord > pHSX->ulRecCount )
      return HSX_RECBOUND;

   if( pHSX->ulFirstRec == 0 || ulRecord < pHSX->ulFirstRec ||
       ulRecord >= pHSX->ulFirstRec + pHSX->ulBufRec )
   {
      ZH_FOFFSET fOffset;
      ZH_SIZE nSize;
      ZH_ULONG ulFirst;
      int iRetVal;

      if( ( iRetVal = zh_hsxFlush( iHandle ) ) != HSX_SUCCESS )
         return iRetVal;

      ulFirst = ulRecord;
      if( pHSX->fWrLocked && pHSX->fShared )
         pHSX->ulBufRec = 1;
      else if( ulFirst + pHSX->ulBufSize - 1 <= pHSX->ulRecCount )
         pHSX->ulBufRec = pHSX->ulBufSize;
      else
      {
         if( fCount )
            zh_hsxGetRecCount( pHSX );
         pHSX->ulBufRec = ZH_MIN( pHSX->ulBufSize, pHSX->ulRecCount - ulFirst + 1 );
      }

      fOffset = ( ZH_FOFFSET ) HSXHEADER_LEN +
                ( ZH_FOFFSET ) ( ulFirst - 1 ) *
                ( ZH_FOFFSET ) pHSX->uiRecordSize;
      nSize = pHSX->ulBufRec * pHSX->uiRecordSize;

      if( zh_fileReadAt( pHSX->pFile, pHSX->pBuffer, nSize, fOffset ) != nSize )
      {
         pHSX->ulFirstRec = pHSX->ulBufRec = 0;
         return HSX_BADREAD;
      }
      pHSX->ulFirstRec = ulFirst;
   }

   *pRecPtr = pHSX->pBuffer + ( ulRecord - pHSX->ulFirstRec ) * pHSX->uiRecordSize;

   return HSX_SUCCESS;
}

static int zh_hsxAppend( int iHandle, ZH_ULONG * pulRecNo, ZH_BYTE ** pRecPtr )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );

   if( ! pHSX )
      return HSX_BADHANDLE;

   if( pHSX->ulFirstRec == 0 || pHSX->ulBufRec == pHSX->ulBufSize ||
        pHSX->ulFirstRec + pHSX->ulBufRec != pHSX->ulRecCount + 1 )
   {
      int iRetVal;

      if( ( iRetVal = zh_hsxFlush( iHandle ) ) != HSX_SUCCESS )
         return iRetVal;

      *pulRecNo = pHSX->ulFirstRec = ++pHSX->ulRecCount;
      pHSX->ulBufRec = 1;
   }
   else
   {
      pHSX->ulBufRec++;
      *pulRecNo = ++pHSX->ulRecCount;
   }
   *pRecPtr = pHSX->pBuffer + ( pHSX->ulBufRec - 1 ) * pHSX->uiRecordSize;
   pHSX->fHdrChanged = ZH_TRUE;

   return HSX_SUCCESS;
}

static int zh_hsxUpdate( int iHandle, ZH_ULONG ulRecord, ZH_BYTE ** pRecPtr )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );

   if( ! pHSX )
      return HSX_BADHANDLE;

   if( ulRecord > pHSX->ulRecCount )
   {
      /* this is intentional - when HSX index is bound with workarea
       * then all updates should be synced by WA locks and it should
       * be save to use REPLACE called from GOCOLD() method instead of
       * ADD for newly appended records */
      if( pHSX->iArea != 0 )
         pHSX->ulRecCount = ulRecord;
      else if( pHSX->fShared )
         zh_hsxGetRecCount( pHSX );
   }

   if( ulRecord == 0 || ulRecord > pHSX->ulRecCount )
      return HSX_RECBOUND;

   if( pHSX->ulFirstRec == 0 || ulRecord < pHSX->ulFirstRec ||
       ulRecord >= pHSX->ulFirstRec + pHSX->ulBufRec )
   {
      int iRetVal;

      if( ( iRetVal = zh_hsxFlush( iHandle ) ) != HSX_SUCCESS )
         return iRetVal;

      pHSX->ulFirstRec = ulRecord;
      pHSX->ulBufRec = 1;
   }
   *pRecPtr = pHSX->pBuffer + ( ulRecord - pHSX->ulFirstRec ) * pHSX->uiRecordSize;

   return HSX_SUCCESS;
}

static int zh_hsxLock( int iHandle, int iAction, ZH_ULONG ulRecord )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );
   int iRetVal = HSX_SUCCESS;
   ZH_BOOL fResult;

   ZH_SYMBOL_UNUSED( ulRecord );

   if( ! pHSX )
      return HSX_BADHANDLE;

   if( pHSX->fReadonly )
   {
      switch( iAction )
      {
         case HSX_WRITELOCK:
         case HSX_UPDATELOCK:
         case HSX_APPENDLOCK:
         case HSX_HDRWRITELOCK:
            return HSX_LOCKFAILED;
      }
   }

   /*
    * When HSX is bound with with workarea it should be synced
    * by WA locks to not cause additional overhead with repeated
    * operations. zh_hsxAdd() should be called when WA APPEND_LOCK
    * is set and zh_hsxReplace() inside GOCOLD() method
    */
   if( pHSX->fShared && pHSX->iArea == 0 )
   {
      switch( iAction )
      {
         case HSX_READLOCK:
            break;

         case HSX_WRITELOCK:
         case HSX_UPDATELOCK:
         case HSX_APPENDLOCK:
            for( ;; )
            {
               fResult = zh_fileLock( pHSX->pFile, HSX_HDRLOCKPOS, HSX_HDRLOCKSIZE,
                                      FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT );
               if( fResult )
                  break;
               zh_releaseCPU();
            }
            if( iRetVal == HSX_SUCCESS )
            {
               /* discard buffers in shared mode */
               pHSX->ulFirstRec = pHSX->ulBufRec = 0;
               if( iAction == HSX_APPENDLOCK )
                  zh_hsxGetRecCount( pHSX );
               else if( iAction == HSX_WRITELOCK )
                  pHSX->fWrLocked = ZH_TRUE;
            }
            break;

         case HSX_HDRREADLOCK:
            for( ;; )
            {
               fResult = zh_fileLock( pHSX->pFile, HSX_HDRLOCKPOS, HSX_HDRLOCKSIZE,
                                      FL_LOCK | FLX_SHARED | FLX_WAIT );
               if( fResult )
                  break;
               zh_releaseCPU();
            }
            break;

         case HSX_HDRWRITELOCK:
            for( ;; )
            {
               fResult = zh_fileLock( pHSX->pFile, HSX_HDRLOCKPOS, HSX_HDRLOCKSIZE,
                                      FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT );
               if( fResult )
                  break;
               zh_releaseCPU();
            }
            break;

         case HSX_READUNLOCK:
            break;

         case HSX_WRITEUNLOCK:
         case HSX_UPDATEUNLOCK:
         case HSX_APPENDUNLOCK:
            iRetVal = zh_hsxFlush( iHandle );
            if( iAction == HSX_APPENDLOCK )
               pHSX->fWrLocked = ZH_FALSE;
            /* fallthrough */
         case HSX_HDRWRITEUNLOCK:
         {
            int iRet = zh_hsxHdrFlush( iHandle );
            if( iRetVal == HSX_SUCCESS )
               iRetVal = iRet;
         }
         /* fallthrough */
         case HSX_HDRREADUNLOCK:
            if( ! zh_fileLock( pHSX->pFile, HSX_HDRLOCKPOS, HSX_HDRLOCKSIZE,
                               FL_UNLOCK ) )
            {
               if( iRetVal == HSX_SUCCESS )
                  iRetVal = HSX_CANNOTUNLOCK;
            }
            break;
      }
   }

   return iRetVal;
}

static int zh_hsxIfDel( int iHandle, ZH_ULONG ulRecord )
{
   ZH_BYTE * pRecPtr;
   int iRetVal, iRet;

   iRetVal = zh_hsxLock( iHandle, HSX_READLOCK, ulRecord );

   if( iRetVal == HSX_SUCCESS )
   {
      iRetVal = zh_hsxRead( iHandle, ulRecord, &pRecPtr );
      if( iRetVal == HSX_SUCCESS )
         iRetVal = ( *pRecPtr & 0x80 ) ? HSX_SUCCESS : HSX_SUCCESSFALSE;
   }
   iRet = zh_hsxLock( iHandle, HSX_READUNLOCK, ulRecord );
   if( iRet != HSX_SUCCESS )
      iRetVal = iRet;
   return iRetVal;
}

static int zh_hsxDelete( int iHandle, ZH_ULONG ulRecord )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );
   int iRetVal;

   if( ! pHSX )
      return HSX_BADHANDLE;

   iRetVal = zh_hsxLock( iHandle, HSX_UPDATELOCK, ulRecord );
   if( iRetVal == HSX_SUCCESS )
   {
      ZH_BYTE * pRecPtr;
      int iRet;

      iRetVal = zh_hsxRead( iHandle, ulRecord, &pRecPtr );
      if( iRetVal == HSX_SUCCESS )
      {
         if( *pRecPtr & 0x80 )
            iRetVal = HSX_ISDELETED;
         else
         {
            *pRecPtr |= 0x80;
            pHSX->fChanged = ZH_TRUE;
            iRetVal = HSX_SUCCESS;
         }
      }
      iRet = zh_hsxLock( iHandle, HSX_UPDATEUNLOCK, ulRecord );
      if( iRetVal == HSX_SUCCESS )
         iRetVal = iRet;
   }
   return iRetVal;
}

static int zh_hsxUnDelete( int iHandle, ZH_ULONG ulRecord )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );
   int iRetVal;

   if( ! pHSX )
      return HSX_BADHANDLE;

   iRetVal = zh_hsxLock( iHandle, HSX_UPDATELOCK, ulRecord );
   if( iRetVal == HSX_SUCCESS )
   {
      ZH_BYTE * pRecPtr;
      int iRet;

      iRetVal = zh_hsxRead( iHandle, ulRecord, &pRecPtr );
      if( iRetVal == HSX_SUCCESS )
      {
         if( ( *pRecPtr & 0x80 ) == 0 )
            iRetVal = HSX_NOTDELETED;
         else
         {
            *pRecPtr &= ~0x80;
            pHSX->fChanged = ZH_TRUE;
            iRetVal = HSX_SUCCESS;
         }
      }
      iRet = zh_hsxLock( iHandle, HSX_UPDATEUNLOCK, ulRecord );
      if( iRetVal == HSX_SUCCESS )
         iRetVal = iRet;
   }
   return iRetVal;
}

static int zh_hsxReplace( int iHandle, ZH_ULONG ulRecord, PZH_ITEM pExpr, ZH_BOOL fDeleted )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );
   int iRetVal;

   if( ! pHSX )
      return HSX_BADHANDLE;

   iRetVal = zh_hsxLock( iHandle, HSX_WRITELOCK, ulRecord );
   if( iRetVal == HSX_SUCCESS )
   {
      ZH_BYTE * pRecPtr;
      int iRet;

      iRetVal = zh_hsxUpdate( iHandle, ulRecord, &pRecPtr );
      if( iRetVal == HSX_SUCCESS )
      {
         iRetVal = zh_hsxEval( iHandle, pExpr, pRecPtr, pExpr ? NULL : &fDeleted );
         if( iRetVal == HSX_SUCCESS )
         {
            if( fDeleted )
               *pRecPtr |= 0x80;
            pHSX->fChanged = ZH_TRUE;
         }
      }
      iRet = zh_hsxLock( iHandle, HSX_WRITEUNLOCK, ulRecord );
      if( iRetVal == HSX_SUCCESS )
         iRetVal = iRet;
   }
   return iRetVal;
}

static int zh_hsxAdd( int iHandle, ZH_ULONG * pulRecNo, PZH_ITEM pExpr, ZH_BOOL fDeleted )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );
   int iRetVal;

   if( pulRecNo )
      *pulRecNo = 0;

   if( ! pHSX )
      return HSX_BADHANDLE;

   if( ! pExpr && ! pHSX->pKeyItem )
      return HSX_BADPARMS;

   iRetVal = zh_hsxLock( iHandle, HSX_APPENDLOCK, 0 );
   if( iRetVal == HSX_SUCCESS )
   {
      ZH_BYTE * pRecPtr;
      ZH_ULONG ulRecNo;
      int iRet;

      iRetVal = zh_hsxAppend( iHandle, &ulRecNo, &pRecPtr );
      if( iRetVal == HSX_SUCCESS )
      {
         iRetVal = zh_hsxEval( iHandle, pExpr, pRecPtr, pExpr ? NULL : &fDeleted );
         if( iRetVal == HSX_SUCCESS )
         {
            if( fDeleted )
               *pRecPtr |= 0x80;
            pHSX->fChanged = ZH_TRUE;
            if( pulRecNo )
               *pulRecNo = ulRecNo;
         }
      }
      iRet = zh_hsxLock( iHandle, HSX_APPENDUNLOCK, 0 );
      if( iRetVal == HSX_SUCCESS )
         iRetVal = iRet;
   }

   return iRetVal;
}

static int zh_hsxSeekSet( int iHandle, const char * pStr, ZH_SIZE nLen )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );
   int iRetVal;

   if( ! pHSX )
      return HSX_BADHANDLE;

   iRetVal = zh_hsxFlushAll( iHandle );
   if( iRetVal == HSX_SUCCESS )
   {
      if( pHSX->ulRecCount == 0 )
         iRetVal = HSX_NORECS;
      else
      {
         if( pHSX->pSearchVal )
            zh_xfree( pHSX->pSearchVal );
         pHSX->pSearchVal = ( char * ) zh_xgrab( nLen + 1 );
         memcpy( pHSX->pSearchVal, pStr, nLen );
         pHSX->pSearchVal[ nLen ] = '\0';
         pHSX->nSearch = nLen;
         if( ! pHSX->pSearchKey )
            pHSX->pSearchKey = ( ZH_BYTE * ) zh_xgrab( pHSX->uiRecordSize );
         zh_hsxHashStr( pStr, nLen, pHSX->pSearchKey,
                        pHSX->uiRecordSize, pHSX->fIgnoreCase,
                        pHSX->iFilterType, pHSX->fUseHash );
         pHSX->ulCurrRec = 0;
      }
   }
   return iRetVal;
}

static int zh_hsxNext( int iHandle, ZH_ULONG * pulRecNo )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );
   int iRetVal;

   *pulRecNo = 0;

   if( ! pHSX )
      return HSX_BADHANDLE;

   iRetVal = zh_hsxLock( iHandle, HSX_READLOCK, 0 );
   if( iRetVal == HSX_SUCCESS )
   {
      ZH_BYTE * pRecPtr;
      int i;
      int iRet;

      while( pHSX->ulCurrRec < pHSX->ulRecCount )
      {
         iRetVal = zh_hsxRead( iHandle, ++pHSX->ulCurrRec, &pRecPtr );
         if( iRetVal != HSX_SUCCESS )
            break;
         if( ! zh_setGetDeleted() || ( *pRecPtr & 0x80 ) == 0 ) /* Not deleted */
         {
            for( i = 0; i < pHSX->uiRecordSize; i++ )
            {
               if( ( pRecPtr[ i ] & pHSX->pSearchKey[ i ] ) != pHSX->pSearchKey[ i ] )
                  break;
            }
            if( i == pHSX->uiRecordSize )
            {
               *pulRecNo = pHSX->ulCurrRec;
               break;
            }
         }
      }

      iRet = zh_hsxLock( iHandle, HSX_READUNLOCK, 0 );
      if( iRetVal == HSX_SUCCESS )
         iRetVal = iRet;
   }
   return iRetVal;
}

static LPHSXINFO zh_hsxNew( void )
{
   LPHSXINFO pHSX;
   int iHandle = 0;
   LPHSXTABLE pTable;

   ZH_HSX_LOCK();

   pTable = zh_hsxTable();
   if( pTable->iHandleSize == 0 )
   {
      pTable->iHandleSize = HSX_HALLOC;
      pTable->handleArray = ( LPHSXINFO * ) zh_xgrabz( sizeof( LPHSXINFO ) * HSX_HALLOC );
   }
   else
   {
      while( iHandle < pTable->iHandleSize )
      {
         if( pTable->handleArray[ iHandle ] == NULL )
            break;
         iHandle++;
      }
      if( iHandle == pTable->iHandleSize )
      {
         pTable->iHandleSize += HSX_HALLOC;
         pTable->handleArray = ( LPHSXINFO * ) zh_xrealloc( pTable->handleArray,
                                          sizeof( LPHSXINFO ) * pTable->iHandleSize );
         memset( &pTable->handleArray[ iHandle ], 0, sizeof( LPHSXINFO ) * HSX_HALLOC );
      }
   }
   pTable->handleArray[ iHandle ] = pHSX = ( LPHSXINFO ) zh_xgrabz( sizeof( HSXINFO ) );
   pTable->iHandleCount++;
   pHSX->iHandle = iHandle;
   pHSX->pFile = NULL;

   ZH_HSX_UNLOCK();

   return pHSX;
}

static void zh_hsxExpDestroy( PZH_ITEM pItem )
{
   zh_vmDestroyBlockOrMacro( pItem );
}

static int zh_hsxVerify( int iHandle, const char * szText, ZH_SIZE nLen,
                         const char * szSub, ZH_SIZE nSub, int iType )
{
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );
   int iResult;

   if( ! szSub && pHSX )
   {
      szSub = pHSX->pSearchVal;
      nSub = pHSX->nSearch;
   }
   if( ! pHSX )
      iResult = HSX_BADHANDLE;
   else if( ! szText || ! szSub )
      iResult = HSX_BADPARMS;
   else if( nSub > nLen || nSub == 0 )
      /* ! nSub -> do not accept empty substrings as $ operator at runtime */
      iResult = HSX_SUCCESSFALSE;
   else
   {
      ZH_SIZE nPos1, nPos2;

      switch( iType )
      {
         case HSX_VERIFY_BEGIN:
            iResult = zh_hsxStrCmp( szSub, nSub, szText, nSub,
                                    pHSX->fIgnoreCase, pHSX->iFilterType );
            break;
         case HSX_VERIFY_END:
            iResult = zh_hsxStrCmp( szSub, nSub, szText + nLen - nSub, nSub,
                                    pHSX->fIgnoreCase, pHSX->iFilterType );
            break;
         case HSX_VERIFY_AND:
            iResult = HSX_SUCCESS;
            for( nPos1 = 0; nPos1 < nSub && iResult == HSX_SUCCESS; nPos1++ )
            {
               while( szSub[ nPos1 ] == ' ' && nPos1 < nSub )
                  ++nPos1;
               nPos2 = nPos1;
               while( szSub[ nPos2 ] != ' ' && nPos2 < nSub )
                  ++nPos2;
               iResult = zh_hsxStrCmp( &szSub[ nPos1 ], nPos2 - nPos1, szText, nLen,
                                       pHSX->fIgnoreCase, pHSX->iFilterType );
               nPos1 = nPos2;
            }
            break;
#if 0
         case HSX_VERIFY_OR:
            iResult = HSX_SUCCESSFALSE;
            for( nPos1 = 0; nPos1 < nSub && iResult == HSX_SUCCESSFALSE; nPos1++ )
            {
               while( szSub[ nPos1 ] == ' ' && nPos1 < nSub )
                  ++nPos1;
               nPos2 = nPos1;
               while( szSub[ nPos2 ] != ' ' && nPos2 < nSub )
                  ++nPos2;
               iResult = zh_hsxStrCmp( &szSub[ nPos1 ], nPos2 - nPos1, szText, nLen,
                                       pHSX->fIgnoreCase, pHSX->iFilterType );
               nPos1 = nPos2;
            }
            break;
#endif
         case HSX_VERIFY_PHRASE:
         default:
            iResult = zh_hsxStrCmp( szSub, nSub, szText, nLen,
                                    pHSX->fIgnoreCase, pHSX->iFilterType );
      }
   }
   return iResult;
}

static int zh_hsxDestroy( int iHandle )
{
   LPHSXINFO pHSX = NULL;
   int iRetVal;

   iRetVal = zh_hsxFlushAll( iHandle );

   ZH_HSX_LOCK();
   {
      LPHSXTABLE pTable = zh_hsxTable();
      if( iHandle >= 0 && iHandle < pTable->iHandleSize &&
          pTable->handleArray[ iHandle ] != NULL )
      {
         pHSX = pTable->handleArray[ iHandle ];
         pTable->handleArray[ iHandle ] = NULL;
         if( --pTable->iHandleCount == 0 )
         {
            zh_xfree( pTable->handleArray );
            pTable->iHandleSize = 0;
            pTable->handleArray = NULL;
         }
      }
   }
   ZH_HSX_UNLOCK();

   if( pHSX )
   {
      if( pHSX->pFile )
         zh_fileClose( pHSX->pFile );
      if( pHSX->szFileName )
         zh_xfree( pHSX->szFileName );
      if( pHSX->pSearchVal )
         zh_xfree( pHSX->pSearchVal );
      if( pHSX->pSearchKey )
         zh_xfree( pHSX->pSearchKey );
      if( pHSX->pBuffer )
         zh_xfree( pHSX->pBuffer );
      if( pHSX->szKeyExpr )
         zh_xfree( pHSX->szKeyExpr );
      if( pHSX->pKeyItem )
         zh_hsxExpDestroy( pHSX->pKeyItem );
      zh_xfree( pHSX );
   }
   return iRetVal;
}

static int zh_hsxCreate( const char * szFile, int iBufSize, int iKeySize,
                         ZH_BOOL fIgnoreCase, int iFilter, PZH_ITEM pExpr )
{
   char szFileName[ ZH_PATH_MAX ];
   const char * szExpr = NULL;
   PZH_ITEM pKeyExpr = NULL;
   ZH_ULONG ulBufSize;
   ZH_USHORT uiRecordSize;
   LPHSXINFO pHSX;
   PZH_FILE pFile;
   int iRetVal;

   if( ! szFile || ! *szFile )
      return HSX_BADPARMS;

   zh_strncpy( szFileName, szFile, ZH_PATH_MAX - 1 );

   if( iKeySize < 1 || iKeySize > HSXMAXKEY_SIZE )
      iKeySize = HSXDEFKEY_SIZE;
   if( iFilter < 1 || iFilter > 3 )
      iFilter = HSXDEFFILTER;

   ulBufSize = iBufSize * 1024;
   if( ulBufSize == 0 )
      ulBufSize = HSXDEFBUF_LEN;
   else if( ulBufSize < HSXMINBUF_LEN )
      ulBufSize = HSXMINBUF_LEN;
   else if( ulBufSize > HSXMAXBUF_LEN )
      ulBufSize = HSXMAXBUF_LEN;
   uiRecordSize = ( ZH_USHORT ) 0x08 << iKeySize;
   ulBufSize /= uiRecordSize;
   if( ulBufSize == 0 )
      ulBufSize = 1;

   if( pExpr )
   {
      if( zh_itemGetCLen( pExpr ) > 0 )
      {
         szExpr = zh_itemGetCPtr( pExpr );
         iRetVal = zh_hsxCompile( szExpr, &pKeyExpr );
         if( iRetVal != HSX_SUCCESS )
            return iRetVal;
      }
      else if( ZH_IS_BLOCK( pExpr ) )
         pKeyExpr = zh_itemNew( pExpr );
   }

   pFile = zh_fileExtOpen( szFileName, HSX_FILEEXT,
                           FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE |
                           FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME |
                           FXO_NOSEEKPOS,
                           NULL, NULL );

   if( ! pFile )
   {
      if( pKeyExpr )
         zh_hsxExpDestroy( pKeyExpr );
      return HSX_CREATEFAIL;
   }

   pHSX = zh_hsxNew();
   pHSX->pFile = pFile;
   pHSX->szFileName = zh_strdup( szFileName );
   pHSX->fShared = ZH_FALSE;
   pHSX->fReadonly = ZH_FALSE;
   pHSX->uiRecordSize = uiRecordSize;
   pHSX->fIgnoreCase = fIgnoreCase;
   pHSX->iFilterType = iFilter;
   pHSX->fUseHash = fIgnoreCase && iKeySize == 2 && iFilter != 3;
   if( szExpr )
      pHSX->szKeyExpr = zh_strdup( szExpr );
   pHSX->pKeyItem = pKeyExpr;
   pHSX->pBuffer = ( ZH_BYTE * ) zh_xalloc( ulBufSize * uiRecordSize );
   if( pHSX->pBuffer == NULL )
   {
      zh_hsxDestroy( pHSX->iHandle );
      return HSX_MEMERR;
   }
   pHSX->ulBufSize = ulBufSize;

   pHSX->fHdrChanged = ZH_TRUE;
   iRetVal = zh_hsxHdrFlush( pHSX->iHandle );
   if( iRetVal != HSX_SUCCESS )
   {
      zh_hsxDestroy( pHSX->iHandle );
      return iRetVal;
   }

   return pHSX->iHandle;
}

static int zh_hsxOpen( const char * szFile, int iBufSize, int iMode )
{
   char szFileName[ ZH_PATH_MAX ];
   ZH_BOOL fShared, fReadonly;
   PZH_FILE pFile;
   ZH_ULONG ulBufSize;
   LPHSXINFO pHSX;
   int iRetVal;

   if( ! szFile || ! *szFile )
      return HSX_BADPARMS;

   zh_strncpy( szFileName, szFile, ZH_PATH_MAX - 1 );

   ulBufSize = iBufSize * 1024;
   if( ulBufSize == 0 )
      ulBufSize = HSXDEFBUF_LEN;
   else if( ulBufSize < HSXMINBUF_LEN )
      ulBufSize = HSXMINBUF_LEN;
   else if( ulBufSize > HSXMAXBUF_LEN )
      ulBufSize = HSXMAXBUF_LEN;

   if( iMode < 0 || iMode > 3 )
      iMode = HSXDEFOPENMODE;

   fReadonly = ( iMode & 0x02 ) != 0;
   fShared = ( iMode & 0x01 ) == 0;
   if( zh_setGetAutoShare() == 2 )
      fShared = ZH_FALSE;

   pFile = zh_fileExtOpen( szFileName, HSX_FILEEXT,
                           ( fReadonly ? FO_READ : FO_READWRITE ) |
                           ( fShared ? FO_DENYNONE : FO_EXCLUSIVE ) |
                           FXO_DEFAULTS | FXO_SHARELOCK |
                           FXO_COPYNAME | FXO_NOSEEKPOS,
                           NULL, NULL );

   if( ! pFile )
      return HSX_OPENERR;

   pHSX = zh_hsxNew();
   pHSX->pFile = pFile;
   pHSX->szFileName = zh_strdup( szFileName );
   pHSX->fShared = fShared;
   pHSX->fReadonly = fReadonly;
   iRetVal = zh_hsxLock( pHSX->iHandle, HSX_HDRREADLOCK, 0 );
   if( iRetVal == HSX_SUCCESS )
   {
      int iRet;
      iRetVal = zh_hsxHdrRead( pHSX->iHandle );
      iRet = zh_hsxLock( pHSX->iHandle, HSX_HDRREADUNLOCK, 0 );
      if( iRetVal == HSX_SUCCESS )
         iRetVal = iRet;
   }
   if( iRetVal != HSX_SUCCESS )
   {
      zh_hsxDestroy( pHSX->iHandle );
      return iRetVal;
   }

   ulBufSize /= pHSX->uiRecordSize;
   if( ulBufSize == 0 )
      ulBufSize = 1;

   pHSX->pBuffer = ( ZH_BYTE * ) zh_xalloc( ulBufSize * pHSX->uiRecordSize );
   if( pHSX->pBuffer == NULL )
   {
      zh_hsxDestroy( pHSX->iHandle );
      return HSX_MEMERR;
   }
   pHSX->ulBufSize = ulBufSize;

   return pHSX->iHandle;
}

static int zh_hsxIndex( const char * szFile, PZH_ITEM pExpr, int iKeySize,
                        int iMode, int iBufSize, ZH_BOOL fIgnoreCase, int iFilter )
{
   int iRetVal = HSX_SUCCESS, iHandle;
   ZH_ULONG ulRecNo = 0, ulRecCount = 0, ulNewRec, ulRec;
   ZH_ERRCODE errCode;
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

   if( ! pArea )
   {
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "HS_INDEX" );
      return HSX_NOTABLE;
   }

   iHandle = zh_hsxCreate( szFile, iBufSize, iKeySize, fIgnoreCase, iFilter, pExpr );
   if( iHandle < 0 )
      return iHandle;

   errCode = SELF_RECCOUNT( pArea, &ulRecCount );
   if( errCode != ZH_FAILURE && ulRecCount )
   {
      errCode = SELF_RECNO( pArea, &ulRecNo );
      if( errCode != ZH_FAILURE )
      {
         for( ulRec = 1; ulRec <= ulRecCount; ulRec++ )
         {
            errCode = SELF_GOTO( pArea, ulRec );
            if( errCode == ZH_FAILURE )
               break;
            iRetVal = zh_hsxAdd( iHandle, &ulNewRec, NULL, ZH_FALSE );
            if( iRetVal != HSX_SUCCESS )
               break;
            if( ulNewRec != ulRec )
            {
               iRetVal = HSX_RECBOUND;
               break;
            }
         }
         if( pArea->valResult )
         {
            zh_itemRelease( pArea->valResult );
            pArea->valResult = NULL;
         }
         if( ulRecNo )
            SELF_GOTO( pArea, ulRecNo );
      }
   }
   zh_hsxDestroy( iHandle );
   if( iRetVal != HSX_SUCCESS )
      return iRetVal;
   if( errCode == ZH_FAILURE )
      return HSX_RDDFAILURE;

   return zh_hsxOpen( szFile, iBufSize, iMode );
}

static int zh_hsxFilter( int iHandle, const char * pSeek, ZH_SIZE nSeek,
                         PZH_ITEM pVerify, int iVerifyType )
{
   AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();
   LPHSXINFO pHSX = zh_hsxGetPointer( iHandle );
   ZH_BOOL fDestroyExpr = ZH_FALSE, fValid;
   int iResult = HSX_SUCCESS;
   ZH_ERRCODE errCode;
   ZH_ULONG ulRecNo = 0, ulRec;
   PZH_ITEM pItem;

   if( ! pHSX )
      return HSX_BADHANDLE;

   if( ! pArea )
   {
      zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "HS_FILTER" );
      return HSX_NOTABLE;
   }

   if( ! pVerify || ZH_IS_NIL( pVerify ) )
      pVerify = pHSX->pKeyItem;
   else
   {
      if( zh_itemGetCLen( pVerify ) > 0 )
      {
         iResult = zh_hsxCompile( zh_itemGetCPtr( pVerify ), &pVerify );
         if( iResult != HSX_SUCCESS )
            return HSX_BADPARMS;
         fDestroyExpr = ZH_TRUE;
      }
      else if( ! ZH_IS_BLOCK( pVerify ) )
      {
         pVerify = NULL;
      }
   }

   errCode = SELF_RECNO( pArea, &ulRecNo );
   if( errCode != ZH_FAILURE )
      iResult = zh_hsxSeekSet( iHandle, pSeek, nSeek );

   fValid = ZH_TRUE;
   pItem = zh_itemNew( NULL );
   while( iResult == HSX_SUCCESS && errCode != ZH_FAILURE )
   {
      iResult = zh_hsxNext( iHandle, &ulRec );
      if( iResult != HSX_SUCCESS || ulRec == 0 )
         break;
      if( pVerify )
      {
         errCode = SELF_GOTO( pArea, ulRec );
         if( errCode == ZH_FAILURE )
            break;
         errCode = SELF_EVALBLOCK( pArea, pVerify );
         if( errCode == ZH_FAILURE )
            break;
         fValid = zh_hsxVerify( iHandle,
                                zh_itemGetCPtr( pArea->valResult ),
                                zh_itemGetCLen( pArea->valResult ),
                                pSeek, nSeek, iVerifyType ) == HSX_SUCCESS;
      }
      if( fValid )
      {
         /* set record in WA RM filter */
         zh_itemPutNInt( pItem, ulRec );
         errCode = SELF_INFO( pArea, DBI_RM_ADD, pItem );
      }
   }
   if( pArea->valResult )
   {
      zh_itemRelease( pArea->valResult );
      pArea->valResult = NULL;
   }
   zh_itemRelease( pItem );

   if( ulRecNo )
      SELF_GOTO( pArea, ulRecNo );

   if( fDestroyExpr )
      zh_hsxExpDestroy( pVerify );

   return errCode == ZH_FAILURE ? HSX_RDDFAILURE : iResult;
}


/* ************************************************************************ */
/* .prg level functions: HS_*() */
/* ************************************************************************ */

/* hs_Create( <cFile>, <nBufSize>, <nKeySize>, <lCase>, <nFiltSet>, <xExpr> )
                     --> nVal >=0 (OK: <hIndex>), nVal < 0 (ERROR CODE)
   Creates a new, empty HiPer-SEEK index file */
ZH_FUNC( HS_CREATE )
{
   zh_retni( zh_hsxCreate( zh_parc( 1 ), zh_parni( 2 ), zh_parni( 3 ),
                           zh_param( 4, ZH_IT_LOGICAL ) == NULL || zh_parl( 4 ),
                           zh_parni( 5 ), zh_param( 6, ZH_IT_ANY ) ) );
}

/* hs_Open( <cFile>, <nBufSize>, <nOpenMode> )
                     --> nVal >=0 (OK: <hIndex>), nVal < 0 (ERROR CODE)
   Opens an existing HiPer-SEEK index file */
ZH_FUNC( HS_OPEN )
{
   zh_retni( zh_hsxOpen( zh_parc( 1 ), zh_parni( 2 ),
             zh_param( 3, ZH_IT_NUMERIC ) ? zh_parni( 3 ) : HSXDEFOPENMODE ) );
}

/* hs_Close( <hIndex> ) --> nVal = 1 (OK), nVal < 0 (ERROR CODE)
   Closes a previously opened HiPer-SEEK index file */
ZH_FUNC( HS_CLOSE )
{
   if( zh_param( 1, ZH_IT_NUMERIC ) )
      zh_retni( zh_hsxDestroy( zh_parni( 1 ) ) );
   else
      zh_retni( HSX_BADPARMS );
}

/* hs_Index( <cFile>, <cExpr>, <nKeySize>, <nOpenMode>, <nBufSize>, <lCase>,
             <nFiltSet> ) --> nVal >=0 (OK: <hIndex>), nVal < 0 (ERROR CODE)
   Creates and populates a new HiPer-SEEK index */
ZH_FUNC( HS_INDEX )
{
   zh_retni( zh_hsxIndex( zh_parc( 1 ), zh_param( 2, ZH_IT_ANY ), zh_parni( 3 ),
                          zh_param( 4, ZH_IT_NUMERIC ) ? zh_parni( 4 ) : HSXDEFOPENMODE,
                          zh_parni( 5 ),
                          zh_param( 6, ZH_IT_LOGICAL ) == NULL || zh_parl( 6 ),
                          zh_parni( 7 ) ) );
}

/* hs_Add( <hIndex>, [<xExpr>], [lDel] ) --> nVal >= 1 (RECNO), nVal < 0 (ERROR CODE)
   Adds a text string entry to a HiPer-SEEK index file */
ZH_FUNC( HS_ADD )
{
   if( zh_param( 1, ZH_IT_NUMERIC ) )
   {
      ZH_ULONG ulRecNo;
      int iRetVal;

      iRetVal = zh_hsxAdd( zh_parni( 1 ), &ulRecNo,
                           zh_param( 2, ZH_IT_BLOCK | ZH_IT_STRING ),
                           zh_parl( 3 ) );

      if( iRetVal == HSX_SUCCESS )
         zh_retnint( ulRecNo );
      else
         zh_retni( iRetVal );
   }
   else
      zh_retni( HSX_BADPARMS );
}

/* hs_Replace( <hIndex>, [<xExpr>], <nRecNo>, [lDel] ) --> nVal = 1 (OK), nVal < 0 (ERROR CODE)
   Replaces current HiPer-SEEK index entry with a new value */
ZH_FUNC( HS_REPLACE )
{
   if( zh_param( 1, ZH_IT_NUMERIC ) && zh_param( 3, ZH_IT_NUMERIC ) )
      zh_retni( zh_hsxReplace( zh_parni( 1 ), zh_parnl( 3 ),
                               zh_param( 2, ZH_IT_BLOCK | ZH_IT_STRING ),
                               zh_parl( 4 ) ) );
   else
      zh_retni( HSX_BADPARMS );
}

/* hs_IfDel( <hIndex>, <nRecNo> ) --> nVal = {0|1} (DELETED), nVal < 0 (ERROR CODE)
   Determines if a HiPer-SEEK record is marked as deleted */
ZH_FUNC( HS_IFDEL )
{
   if( zh_param( 1, ZH_IT_NUMERIC ) && zh_param( 2, ZH_IT_NUMERIC ) )
      zh_retni( zh_hsxIfDel( zh_parni( 1 ), zh_parnl( 2 ) ) );
   else
      zh_retni( HSX_BADPARMS );
}

/* hs_Delete( <hIndex>, <nRecNo> ) --> nVal = 1 (OK), nVal < 0 (ERROR CODE)
   Deletes specifed index record from HiPer-SEEK index file */
ZH_FUNC( HS_DELETE )
{
   if( zh_param( 1, ZH_IT_NUMERIC ) && zh_param( 2, ZH_IT_NUMERIC ) )
      zh_retni( zh_hsxDelete( zh_parni( 1 ), zh_parnl( 2 ) ) );
   else
      zh_retni( HSX_BADPARMS );
}

/* hs_Undelete( <hIndex>, <nRecNo> ) --> nVal = 1 (OK), nVal < 0 (ERROR CODE)
   Unmarks the specified HiPer-SEEK record as being deleted */
ZH_FUNC( HS_UNDELETE )
{
   if( zh_param( 1, ZH_IT_NUMERIC ) && zh_param( 2, ZH_IT_NUMERIC ) )
      zh_retni( zh_hsxUnDelete( zh_parni( 1 ), zh_parnl( 2 ) ) );
   else
      zh_retni( HSX_BADPARMS );
}

/* hs_KeyCount( <hIndex> ) --> nVal >= 0 (RECCOUNT), nVal < 0 (ERROR CODE)
   Returns the number of entries in a HiPer-SEEK index */
ZH_FUNC( HS_KEYCOUNT )
{
   if( zh_param( 1, ZH_IT_NUMERIC ) )
   {
      LPHSXINFO pHSX = zh_hsxGetPointer( zh_parni( 1 ) );

      if( pHSX )
      {
         if( pHSX->fShared )
            zh_hsxGetRecCount( pHSX );

         zh_retnint( pHSX->ulRecCount );
      }
      else
         zh_retni( HSX_BADHANDLE );
   }
   else
      zh_retni( HSX_BADPARMS );
}

/* hs_Set( <hIndex>, <cExpr> ) --> nVal = 1 (OK), nVal < 0 (ERROR CODE)
   Sets up parameters for a subsequent hs_Next() call */
ZH_FUNC( HS_SET )
{
   const char * pStr = zh_parc( 2 );
   int iRetVal = HSX_BADPARMS;

   if( pStr && zh_param( 1, ZH_IT_NUMERIC ) )
      iRetVal = zh_hsxSeekSet( zh_parni( 1 ), pStr, zh_parclen( 2 ) );
   zh_retni( iRetVal );
}

/* hs_Filter( <cIndex>, <cVal>, [xRealExp], [nBufSize], [nOpenMode] ) --> nRecMatch
   Sets a WA RM filter using a HiPer-SEEK index */
ZH_FUNC( HS_FILTER )
{
   const char * szText = zh_parc( 2 );
   char * pBuff = NULL;
   ZH_SIZE nLen = zh_parclen( 2 );
   ZH_ULONG ulRecords = 0;
   int iHandle = -1, iResult = HSX_BADPARMS;
   ZH_BOOL fNew = ZH_FALSE, fToken = ZH_TRUE;

   if( zh_parclen( 1 ) > 0 )
   {
      if( nLen > 0 )
      {
         iHandle = zh_hsxOpen( zh_parc( 1 ), zh_parni( 4 ),
               zh_param( 5, ZH_IT_NUMERIC ) ? zh_parni( 5 ) : HSXDEFOPENMODE );
         if( iHandle >= 0 )
            fNew = ZH_TRUE;
         else
            iResult = iHandle;
      }
   }
   else if( zh_param( 1, ZH_IT_NUMERIC ) )
   {
      LPHSXINFO pHSX = zh_hsxGetPointer( zh_parni( 1 ) );

      if( ! pHSX )
         iResult = HSX_BADHANDLE;
      else
      {
         iHandle = pHSX->iHandle;
         if( ! szText )
         {
            nLen = pHSX->nSearch;
            if( nLen && pHSX->pSearchVal )
            {
               pBuff = ( char * ) zh_xgrab( nLen + 1 );
               memcpy( pBuff, pHSX->pSearchVal, nLen );
               pBuff[ nLen ] = '\0';
               szText = pBuff;
               fToken = ZH_FALSE;
            }
         }
      }
   }
   if( iHandle >= 0 && nLen > 0 && szText )
   {
      PZH_ITEM pItem = zh_itemNew( NULL );
      AREAP pArea = ( AREAP ) zh_rddGetCurrentWorkAreaPointer();

      if( ! pArea )
      {
         zh_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "HS_FILTER" );
         iResult = HSX_NOTABLE;
      }
      /* create empty workarea RM filter */
      else if( SELF_INFO( pArea, DBI_RM_CREATE, pItem ) == ZH_FAILURE )
         iResult = HSX_RDDFAILURE;
      else
      {
         /* to be SIX compatible divide given text on space delimited tokens */
         if( fToken )
         {
            ZH_SIZE nPos2, nPos1;

            iResult = HSX_SUCCESS;
            for( nPos1 = 0; nPos1 < nLen && iResult == HSX_SUCCESS; nPos1++ )
            {
               while( szText[ nPos1 ] == ' ' && nPos1 < nLen )
                  ++nPos1;
               nPos2 = nPos1;
               while( szText[ nPos2 ] != ' ' && nPos2 < nLen )
                  ++nPos2;
               iResult = zh_hsxFilter( iHandle, &szText[ nPos1 ], nPos2 - nPos1,
                                       zh_param( 3, ZH_IT_ANY ), HSX_VERIFY_PHRASE );
               nPos1 = nPos2;
            }
         }
         else
         {
            iResult = zh_hsxFilter( iHandle, szText, nLen,
                                    zh_param( 3, ZH_IT_ANY ),
                                    HSX_VERIFY_PHRASE );
         }
      }
      if( iResult == HSX_SUCCESS )
      {
         zh_itemClear( pItem );
         if( SELF_INFO( pArea, DBI_RM_COUNT, pItem ) == ZH_FAILURE )
            iResult = HSX_RDDFAILURE;
         else
            ulRecords = zh_itemGetNL( pItem );
      }
      zh_itemRelease( pItem );

      if( fNew )
         zh_hsxDestroy( iHandle );
   }
   if( pBuff )
      zh_xfree( pBuff );

   if( iResult != HSX_SUCCESS )
      zh_retni( iResult );
   else
      zh_retnint( ulRecords );
}

/* hs_Next( <hIndex> ) --> nVal >= 0 (RECNO), nVal < 0 (ERROR CODE)
   Searches a HiPer-SEEK index file for first/next match */
ZH_FUNC( HS_NEXT )
{
   ZH_ULONG ulRecNo = 0;
   int iRetVal = HSX_BADPARMS;

   if( zh_param( 1, ZH_IT_NUMERIC ) )
      iRetVal = zh_hsxNext( zh_parni( 1 ), &ulRecNo );

   if( iRetVal == HSX_SUCCESS )
      zh_retnint( ulRecNo );
   else
      zh_retni( iRetVal );
}

/* hs_Verify( <hIndex>, <bSource>, <cValue>, <nType> )
          --> nVal = {0|1} (VERIFIED), nVal < 0 (ERROR CODE)
   hs_Verify( <bSource>, <cValue> ) --> lOK
   Verifies hs_Next() hit against code block expression */
ZH_FUNC( HS_VERIFY )
{
   if( zh_param( 1, ZH_IT_NUMERIC ) )
   {
      int iHandle = zh_parni( 1 );
      PZH_ITEM pExpr = zh_param( 2, ZH_IT_BLOCK );
      const char * szText = NULL;
      ZH_SIZE nLen = 0;
      LPHSXINFO pHSX;

      pHSX = zh_hsxGetPointer( iHandle );
      if( ! pHSX )
      {
         zh_retni( HSX_BADHANDLE );
         return;
      }
      if( pExpr )
         pExpr = zh_vmEvalBlockOrMacro( pExpr );
      else
      {
         pExpr = zh_param( 2, ZH_IT_STRING );
         if( ! pExpr && pHSX->pKeyItem )
            pExpr = zh_vmEvalBlockOrMacro( pHSX->pKeyItem );
      }
      if( pExpr )
      {
         szText = zh_itemGetCPtr( pExpr );
         nLen = zh_itemGetCLen( pExpr );
      }

      zh_retni( zh_hsxVerify( zh_parni( 1 ), szText, nLen,
                              zh_parc( 3 ), zh_parclen( 3 ),
                              zh_parni( 4 ) ) );
   }
   else
   {
      PZH_ITEM pExpr = zh_param( 1, ZH_IT_BLOCK );
      const char * szSub = zh_parc( 2 ), * szText = NULL;
      ZH_SIZE nSub = zh_parclen( 2 ), nLen = 0;
      ZH_BOOL fIgnoreCase = zh_parl( 3 );

      if( nSub )
      {
         pExpr = pExpr ? zh_vmEvalBlockOrMacro( pExpr ) : zh_param( 2, ZH_IT_STRING );

         if( pExpr )
         {
            szText = zh_itemGetCPtr( pExpr );
            nLen = zh_itemGetCLen( pExpr );
         }
      }
      zh_retl( nLen && nSub && zh_hsxStrCmp( szSub, nSub, szText, nLen,
                                             fIgnoreCase, 3 ) );
   }
}

/* hs_Version() --> <cVersion> */
ZH_FUNC( HS_VERSION )
{
   static const char sc_szVer[] = "HiPer-SEEK / FTS library emulation";

   char * pszHBVersion = zh_verZiher();
   char * pszVersion = zh_xstrcpy( NULL, sc_szVer, ": ", pszHBVersion, NULL );

   zh_retclen_buffer( pszVersion, strlen( pszVersion ) );
   zh_xfree( pszHBVersion );
}
