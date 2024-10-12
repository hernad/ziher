/*
 * DBF RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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

#ifndef ZH_RDDDBF_H_
#define ZH_RDDDBF_H_

#include "zh_rdd_api.h"
#include "zh_dbf_error.h"
#include "zh_dbf.h"

ZH_EXTERN_BEGIN

/* DBF default file extensions */
#define DBF_TABLEEXT                      ".dbf"

/* DBF locking schemes */
#define DBF_LOCKPOS_COMIX                 1000000000UL
#define DBF_LOCKPOS_VFP                   0x40000000UL
#define DBF_LOCKPOS_VFPX                  0x7ffffffeUL
#define DBF_LOCKPOS_HB32                  4000000000UL
#define DBF_LOCKPOS_HB64                  ZH_LL( 0x7F00000000000000 )

#define DBF_LOCKDIR_COMIX                 1
#define DBF_LOCKDIR_VFP                   2  /* lock forward at at record offset */
#define DBF_LOCKDIR_VFPX                  -1
#define DBF_LOCKDIR_HB32                  1
#define DBF_LOCKDIR_HB64                  1

#define DBF_FLCKSIZE_COMIX                1000000000UL
#define DBF_FLCKSIZE_VFP                  0x3ffffffdUL
#define DBF_FLCKSIZE_VFPX                 0x07ffffffUL
#define DBF_FLCKSIZE_HB32                 294967295UL
#define DBF_FLCKSIZE_HB64                 0xfffffffeUL

#define DBF_RLCKSIZE_COMIX                1UL
#define DBF_RLCKSIZE_VFP                  1UL
#define DBF_RLCKSIZE_VFPX                 1UL
#define DBF_RLCKSIZE_HB32                 1UL
#define DBF_RLCKSIZE_HB64                 1UL

#define IDX_LOCKPOS_COMIX                 0xfffeffffUL
#define IDX_LOCKPOS_VFP                   0x7ffffffeUL
#define IDX_LOCKPOS_HB32                  0xfffeffffUL
#define IDX_LOCKPOS_HB64                  ZH_LL( 0x7FFFFFFF00000001 )

#define IDX_LOCKPOOL_COMIX                0x00010000UL
#define IDX_LOCKPOOL_VFP                  0UL
#define IDX_LOCKPOOL_HB32                 0x00010000UL
#define IDX_LOCKPOOL_HB64                 0x00010000UL


/* Index dirty read flags */
#define ZH_IDXREAD_CLEAN      0
#define ZH_IDXREAD_DEFAULT    1
#define ZH_IDXREAD_DIRTY      2

#define ZH_IDXREAD_CLEANMASK  ZH_IDXREAD_DIRTY
#define ZH_IDXREAD_DIRTYMASK  (ZH_IDXREAD_DIRTY|ZH_IDXREAD_DEFAULT)

#define DBFNODE_DATA( r )     ( ( LPDBFDATA ) zh_stackGetTSD( ( PZH_TSD ) \
                                                      ( r )->lpvCargo ) )
#define DBFAREA_DATA( w )     DBFNODE_DATA( SELF_RDDNODE( &( w )->area ) )


#define ZH_DIRTYREAD( w )     ( ( DBFAREA_DATA( w )->uiDirtyRead & \
                                              ( w )->uiDirtyRead ) != 0 )


/*
 * Private DBF* RDD data kept in RDDNODE
 */
typedef struct _DBFDATA
{
   char      szTableExt[ ZH_MAX_FILE_EXT + 1 ];
   char      szIndexExt[ ZH_MAX_FILE_EXT + 1 ];
   char      szMemoExt[ ZH_MAX_FILE_EXT + 1 ];

   char *    szPasswd;
   char *    szPendingPasswd;
   char *    szTrigger;
   char *    szPendingTrigger;

   ZH_BYTE   bLockType;        /* 0 */
   ZH_BYTE   bTableType;       /* DB_DBF_STD */
   ZH_BYTE   bCryptType;       /* DB_CRYPT_NONE */
   ZH_BYTE   bMemoType;        /* DB_MEMO_FPT */
   ZH_BYTE   bMemoExtType;     /* DB_MEMOVER_FLEX */
   ZH_BYTE   bDecimals;        /* RDDI_DECIMALS */
   ZH_USHORT uiSetHeader;      /* RDDI_SETHEADER */
   ZH_USHORT uiDirtyRead;      /* ZH_IDXREAD_CLEANMASK */
   ZH_USHORT uiIndexPageSize;  /* 0 */
   ZH_ULONG  ulMemoBlockSize;  /* 0 */

   ZH_BOOL   fSortRecNo;
   ZH_BOOL   fMultiKey;
   ZH_BOOL   fStruct;
   ZH_BOOL   fStrictStruct;
   ZH_BOOL   fMultiTag;
} DBFDATA, * LPDBFDATA;

typedef struct _ZH_DBFFIELDBITS
{
   ZH_USHORT uiNullBit;
   ZH_USHORT uiLengthBit;
} ZH_DBFFIELDBITS, * PZH_DBFFIELDBITS;

typedef struct _ZH_DBFLOCKDATA
{
   ZH_FOFFSET     offset;
   ZH_FOFFSET     size;
   ZH_FOFFSET     next;
   ZH_FOFFSET     tolock;
   int            type;
   int            count;
} ZH_DBFLOCKDATA, * PZH_DBFLOCKDATA;


/*
 *  DBF WORKAREA
 *  ------------
 *  The Workarea Structure of DBF RDD
 *
 */

typedef struct _DBFAREA
{
   AREA area;

   /*
   *  DBFS's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   PZH_FILE    pDataFile;           /* Data file handle */
   PZH_FILE    pMemoFile;           /* Memo file handle */
   PZH_FILE    pMemoTmpFile;        /* Memo temporary file handle */
   char *      szDataFileName;      /* Name of data file */
   char *      szMemoFileName;      /* Name of memo file */
   ZH_USHORT   uiHeaderLen;         /* Size of header */
   ZH_USHORT   uiRecordLen;         /* Size of record */
   ZH_ULONG    ulMemoBlockSize;     /* Size of memo block */
   ZH_ULONG    ulNewBlockSize;      /* Size of new memo block */
   ZH_USHORT   uiMemoVersion;       /* MEMO file version */
   ZH_USHORT   uiDirtyRead;         /* Index dirty read bit filed */
   ZH_USHORT   uiNullOffset;        /* Offset to _NullFlags filed */
   ZH_USHORT   uiNullCount;         /* Number of null flags */
   ZH_BYTE     bTableType;          /* DBF type */
   ZH_BYTE     bMemoType;           /* MEMO type used in DBF memo fields */
   ZH_BYTE     bLockType;           /* Type of locking schemes */
   ZH_BYTE     bCryptType;          /* Type of used encryption */
   ZH_UINT     uiSetHeader;         /* DBF header updating modes DBI_SETHEADER */
   DBFHEADER   dbfHeader;           /* DBF header buffer */
   ZH_USHORT * pFieldOffset;        /* Pointer to field offset array */
   PZH_DBFFIELDBITS pFieldBits;     /* Pointer to extended DBF field info array */
   ZH_BYTE *   pRecord;             /* Buffer of record data */
   ZH_ULONG    ulRecCount;          /* Total records */
   ZH_ULONG    ulRecNo;             /* Current record */
   ZH_BOOL     fAutoInc;            /* WorkArea with auto increment fields */
   ZH_BOOL     fHasMemo;            /* WorkArea with Memo fields */
   ZH_BOOL     fHasTags;            /* WorkArea with MDX or CDX index */
   ZH_BOOL     fModStamp;           /* WorkArea with modification autoupdate fields */
   ZH_BOOL     fDataFlush;          /* data was written to DBF and not committed */
   ZH_BOOL     fMemoFlush;          /* data was written to MEMO and not committed */
   ZH_BOOL     fShared;             /* Shared file */
   ZH_BOOL     fReadonly;           /* Read only file */
   ZH_BOOL     fTemporary;          /* Temporary file */
   ZH_BOOL     fValidBuffer;        /* State of buffer */
   ZH_BOOL     fPositioned;         /* Positioned record */
   ZH_BOOL     fRecordChanged;      /* Record.zhhanged */
   ZH_BOOL     fAppend;             /* ZH_TRUE if new record is added */
   ZH_BOOL     fDeleted;            /* ZH_TRUE if record is deleted */
   ZH_BOOL     fTableEncrypted;     /* ZH_TRUE if table is encrypted */
   ZH_BOOL     fUpdateHeader;       /* Update header of file */
   ZH_BOOL     fFLocked;            /* ZH_TRUE if file is locked */
   ZH_BOOL     fHeaderLocked;       /* ZH_TRUE if DBF header is locked */
   ZH_BOOL     fPackMemo;           /* Pack memo file in pack operation */
   ZH_BOOL     fTransRec;           /* ZH_TRUE if records are transferred to this area, allow to change autoupdate fields and disable their initialization */
   LPDBOPENINFO lpdbOpenInfo;       /* Pointer to current dbOpenInfo structure in OPEN/CREATE methods */
   LPDBRELINFO lpdbPendingRel;      /* Pointer to parent relation struct */
   ZH_ULONG *  pLocksPos;           /* List of records locked */
   ZH_ULONG    ulNumLocksPos;       /* Number of records locked */
   char *      pCryptKey;           /* Pointer to encryption key */
} DBFAREA;

typedef DBFAREA * LPDBFAREA;

#ifndef DBFAREAP
#define DBFAREAP LPDBFAREA
#endif

#define SUPERTABLE                         ( &dbfSuper )

extern ZH_EXPORT ZH_ULONG   zh_dbfGetMemoBlock( DBFAREAP pArea, ZH_USHORT uiIndex );
extern ZH_EXPORT void       zh_dbfPutMemoBlock( DBFAREAP pArea, ZH_USHORT uiIndex,
                                                ZH_ULONG ulBlock );
extern ZH_EXPORT ZH_ERRCODE zh_dbfGetMemoData( DBFAREAP pArea, ZH_USHORT uiIndex,
                                               ZH_ULONG * pulBlock, ZH_ULONG * pulSize,
                                               ZH_ULONG * pulType );
extern ZH_EXPORT ZH_ERRCODE zh_dbfSetMemoData( DBFAREAP pArea, ZH_USHORT uiIndex,
                                               ZH_ULONG ulBlock, ZH_ULONG ulSize,
                                               ZH_ULONG ulType );
extern ZH_EXPORT ZH_ERRCODE zh_dbfGetEGcode( ZH_ERRCODE errCode );
extern ZH_EXPORT ZH_BOOL    zh_dbfLockIdxGetData( ZH_BYTE bScheme, PZH_DBFLOCKDATA pLockData );
extern ZH_EXPORT ZH_BOOL    zh_dbfLockIdxFile( DBFAREAP pArea, PZH_FILE pFile,
                                               int iType, ZH_BOOL fLateWrlck,
                                               PZH_DBFLOCKDATA pLockData );
extern ZH_EXPORT ZH_BOOL    zh_dbfLockIdxWrite( DBFAREAP pArea, PZH_FILE pFile,
                                                PZH_DBFLOCKDATA pLockData );

extern ZH_EXPORT void zh_dbfTranslateRec( DBFAREAP pArea, ZH_BYTE * pBuffer, PZH_CODEPAGE cdp_src, PZH_CODEPAGE cdp_dest );

ZH_EXTERN_END

#endif /* ZH_RDDDBF_H_ */
