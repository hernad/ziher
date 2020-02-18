/*
 * SDF RDD
 *
 * Copyright 2006 Przemyslaw Czerpak
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

#ifndef ZH_RDDSDF_H_
#define ZH_RDDSDF_H_

#include "zh_rdd_api.h"

ZH_EXTERN_BEGIN

/* SDF default file extensions */
#define SDF_TABLEEXT                      ".txt"


/*
 *  SDF WORKAREA
 *  ------------
 *  The Workarea Structure of SDF RDD
 *
 */

typedef struct _SDFAREA
{
   AREA area;

   /*
   *  SDFS's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   PZH_FILE    pFile;               /* Data file handle */
   char *      szFileName;          /* Name of data file */
   char *      szEol;               /* EOL marker */
   ZH_USHORT   uiEolLen;            /* Size of EOL marker */
   ZH_USHORT   uiRecordLen;         /* Size of record */
   ZH_USHORT * pFieldOffset;        /* Pointer to field offset array */
   ZH_BYTE *   pRecord;             /* Buffer of record data */
   ZH_BYTE *   pBuffer;             /* Read/Write */
   ZH_SIZE     nBufferSize;         /* IO buffer size */
   ZH_SIZE     nBufferRead;         /* Number of bytes in read buffer */
   ZH_SIZE     nBufferIndex;        /* Index to read read buffer */
   ZH_ULONG    ulRecNo;             /* Current record */
   ZH_ULONG    ulRecCount;          /* Number of records (in export) */
   ZH_BOOL     fTransRec;           /* Can put whole records */
   ZH_BOOL     fFlush;              /* Data was written to SDF and not committed */
   ZH_BOOL     fShared;             /* Shared file */
   ZH_BOOL     fReadonly;           /* Read only file */
   ZH_BOOL     fPositioned;         /* Positioned record */
   ZH_BOOL     fRecordChanged;      /* Record.zhhanged */
   ZH_BOOL     fAnyEol;             /* Check for CRLF, LF, CR and LFCR EOLs */
} SDFAREA;

typedef SDFAREA * LPSDFAREA;

#ifndef SDFAREAP
#define SDFAREAP LPSDFAREA
#endif

ZH_EXTERN_END

#endif /* ZH_RDDSDF_H_ */
