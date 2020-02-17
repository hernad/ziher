/*
 * SQL Database Driver include file
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


#ifndef ZH_RDDSQL_H_
#define ZH_RDDSQL_H_

#include "zh_api.h"
#include "zh_rdd_api.h"
#include "../dbf_cdx/zh_dbf_error.h"
#include "zh_api_error.h"
#include "zh_lang_api.h"
#include "rdd_sql.zhh"

/* SQLBASE */

#define MAX_FIELD_NAME       64

#define SQLDD_ROWSET_INIT    256
#define SQLDD_ROWSET_RESIZE  64

#define SQLDD_FLAG_DELETED   1
#define SQLDD_FLAG_CACHED    2


typedef struct _SQLBASEAREA
{
   AREA area;

   /* SQLBASE additions to the workarea structure */

   LPDBRELINFO lpdbPendingRel;

   ZH_ULONG ulConnection;
   struct _SQLDDCONNECTION * pConnection;
   const struct _SDDNODE * pSDD;

   char * szQuery;                           /* SQL query */

   ZH_ULONG ulRecNo;                         /* Current record number */
   ZH_ULONG ulRecCount;                      /* Total records */
   ZH_ULONG ulRecMax;                        /* Size of pRow, pRowFlags buffer */

   void **   pRow;                           /* array of native pointers or cached PZH_ITEM */
   ZH_BYTE * pRowFlags;

   void *  pRecord;                          /* current record */
   ZH_BYTE bRecordFlags;

   ZH_BOOL fFetched;
   ZH_BOOL fPositioned;
   ZH_BOOL fAppend;
   ZH_BOOL fRecordChanged;

   void * pSDDData;                          /* SDD specific data */
} SQLBASEAREA, * SQLBASEAREAP;


typedef struct _SQLDDCONNECTION
{
   struct _SDDNODE * pSDD;
   unsigned int      uiAreaCount;

   void * pSDDConn;                          /* SDD specific data */
} SQLDDCONNECTION;


/* SQLMIX */

#define MIX_MAXKEYLEN      1024
#define MIX_MAXTAGNAMELEN  16


#define MIX_NODE_ORDER     2        /* >=2 */


typedef struct _MIXKEY
{
   ZH_ULONG rec;
   ZH_BYTE  notnul;
   ZH_BYTE  val[ 1 ];
} MIXKEY, * PMIXKEY;


typedef struct _MIXNODE
{
   unsigned int      Leaf;
   unsigned int      KeyCount;
   struct _MIXNODE * Parent;
   struct _MIXNODE * Child[ MIX_NODE_ORDER + 1 ];
} MIXNODE, * PMIXNODE;


typedef struct _MIXNODELEAF
{
   unsigned int      Leaf;
   unsigned int      KeyCount;
   struct _MIXNODE * Parent;
} MIXNODELEAF, * PMIXNODELEAF;


typedef struct _MIXTAG
{
   struct _MIXTAG *     pNext;
   struct _SQLMIXAREA * pArea;
   char *               szName;
   char *               szKeyExpr;
   char *               szForExpr;
   PZH_ITEM             pKeyItem;
   PZH_ITEM             pForItem;

   ZH_BYTE      bType;
   unsigned int uiKeyLen;                    /* Length of key */
   unsigned int uiTotalLen;                  /* Total length of key structure */

   ZH_BOOL fEof;
   ZH_BOOL fBof;
   ZH_BOOL fCustom;

   PMIXNODE Root;

   PMIXKEY      CurKey;
   PMIXNODE     CurNode;
   unsigned int CurPos;

   PMIXKEY HotKey;
   ZH_BOOL HotFor;

   PZH_CODEPAGE pCodepage;          /* National sort table for character key tags, NULL otherwise */
} MIXTAG, * PMIXTAG;


typedef struct _SQLMIXAREA
{
   SQLBASEAREA sqlarea;

   /*
    *  SQLMIX additions to the sqlbase workarea structure
    */

   PMIXTAG      pTagList;
   PMIXTAG      pTag;
   PZH_CODEPAGE pCodepage;
} SQLMIXAREA, * SQLMIXAREAP;


/* SQLDD */

typedef ZH_ERRCODE ( *SDDFUNC_CONNECT )( SQLDDCONNECTION * pConnection, PZH_ITEM pItem );
typedef ZH_ERRCODE ( *SDDFUNC_DISCONNECT )( SQLDDCONNECTION * pConnection );
typedef ZH_ERRCODE ( *SDDFUNC_EXECUTE )( SQLDDCONNECTION * pConnection, PZH_ITEM pItem );
typedef ZH_ERRCODE ( *SDDFUNC_OPEN )( SQLBASEAREAP pArea );
typedef ZH_ERRCODE ( *SDDFUNC_CLOSE )( SQLBASEAREAP pArea );
typedef ZH_ERRCODE ( *SDDFUNC_GOTO )( SQLBASEAREAP pArea, ZH_ULONG ulRecNo );
typedef ZH_ERRCODE ( *SDDFUNC_GETVALUE )( SQLBASEAREAP pArea, ZH_USHORT uiIndex, PZH_ITEM pItem );
typedef ZH_ERRCODE ( *SDDFUNC_GETVARLEN )( SQLBASEAREAP pArea, ZH_USHORT uiIndex, ZH_ULONG * pLength );


typedef struct _SDDNODE
{
   struct _SDDNODE * pNext;

   const char *       Name;
   SDDFUNC_CONNECT    Connect;
   SDDFUNC_DISCONNECT Disconnect;
   SDDFUNC_EXECUTE    Execute;
   SDDFUNC_OPEN       Open;
   SDDFUNC_CLOSE      Close;
   SDDFUNC_GOTO       GoTo;
   SDDFUNC_GETVALUE   GetValue;
   SDDFUNC_GETVARLEN  GetVarLen;
} SDDNODE, * PSDDNODE;


/* Error subcodes */
#define ESQLDD_NOTCONNECTED  1901
#define ESQLDD_INVALIDFIELD  1902
#define ESQLDD_INVALIDQUERY  1903
#define ESQLDD_START         1904
#define ESQLDD_COMMIT        1905
#define ESQLDD_STMTALLOC     1906
#define ESQLDD_STMTDESCR     1907
#define ESQLDD_STMTFREE      1908
#define ESQLDD_FETCH         1909
#define ESQLDD_LOWMEMORY     1910
#define ESQLDD_NULLSDD       1911
#define ESQLDD_CONNALLOC     1912
#define ESQLDD_ENVALLOC      1913
#define ESQLDD_EXECUTE       1914

ZH_EXTERN_BEGIN

extern ZH_EXPORT int zh_sddRegister( PSDDNODE pSdd );
extern ZH_EXPORT void zh_rddsqlSetError( ZH_ERRCODE errCode, const char * szError, const char * szQuery, PZH_ITEM pItem, unsigned long ulAffectedRows );

ZH_EXTERN_END

#endif
