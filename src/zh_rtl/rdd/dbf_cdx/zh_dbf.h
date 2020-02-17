/*
 * DBF structures
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

#ifndef ZH_DBF_H_
#define ZH_DBF_H_

#include "zh_rdd_api.h"

ZH_EXTERN_BEGIN

/* DBF header */

typedef struct _DBFHEADER
{
   ZH_BYTE   bVersion;
   ZH_BYTE   bYear;
   ZH_BYTE   bMonth;
   ZH_BYTE   bDay;
   ZH_BYTE   ulRecCount[ 4 ];
   ZH_BYTE   uiHeaderLen[ 2 ];
   ZH_BYTE   uiRecordLen[ 2 ];
   ZH_BYTE   bReserved1[ 2 ];
   ZH_BYTE   bTransaction;       /* 1-transaction begin */
   ZH_BYTE   bEncrypted;         /* 1-encrypted table */
   ZH_BYTE   bReserved2[ 12 ];
   ZH_BYTE   bHasTags;           /* bit filed: 1-production index, 2-memo file in VFP */
   ZH_BYTE   bCodePage;
   ZH_BYTE   bReserved3[ 2 ];
} DBFHEADER;

typedef DBFHEADER * LPDBFHEADER;



/* DBF fields */

typedef struct _DBFFIELD
{
   ZH_BYTE   bName[ 11 ];
   ZH_BYTE   bType;
   ZH_BYTE   bReserved1[ 4 ];      /* offset from record begin in FP */
   ZH_BYTE   bLen;
   ZH_BYTE   bDec;
   ZH_BYTE   bFieldFlags;          /* 1-system column, 2-nullable, 4-binary */
   ZH_BYTE   bCounter[ 4 ];        /* auto-increment counter */
   ZH_BYTE   bStep;                /* auto-increment step */
   ZH_BYTE   bReserved2[ 7 ];
   ZH_BYTE   bHasTag;
} DBFFIELD;

typedef DBFFIELD * LPDBFFIELD;



/* SMT MEMO field */

typedef struct _SMTFIELD
{
   ZH_BYTE   type[ 2 ];
   ZH_BYTE   length[ 4 ];
   ZH_BYTE   block[ 4 ];
} SMTFIELD;

typedef SMTFIELD * LPSMTFIELD;


ZH_EXTERN_END

#endif /* ZH_DBF_H_ */
