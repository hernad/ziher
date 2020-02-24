/*
 * Header file for the Error API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#ifndef ZH_APIERR_H_
#define ZH_APIERR_H_

#include "zh_api.h"
#include "error.zhh"

ZH_EXTERN_BEGIN

/* Error codes (returned from zh_errLaunch()) */

#define E_BREAK                         0xFFFF
#define E_RETRY                         1
#define E_DEFAULT                       0

/* Error flags */

#define EF_NONE                         0
#define EF_CANRETRY                     1
#define EF_CANSUBSTITUTE                2
#define EF_CANDEFAULT                   4

/* oError:Severity */

/* ... defined in error.zhh */

/* oError:SubSystem (commonly used) */

#define ZH_ERR_SS_BASE                  "BASE"
#define ZH_ERR_SS_TERMINAL              "TERM"
#define ZH_ERR_SS_DBCMD                 "DBCMD"

/* oError:GenCode */

/* ... defined in extend.ch */

/* Internal error numbers */

#define ZH_ERR_IE_NOT_ENOUGH_MEM        1024
#define ZH_ERR_IE_ERR_RECOV_FAIL        1025
#define ZH_ERR_IE_UNREC_ERROR           1026
#define ZH_ERR_IE_GENERIC               1027

#define ZH_ERR_ARGS_BASEPARAMS          0xFFFFFFFF
#define ZH_ERR_ARGS_SELFPARAMS          0xFFFFFFFE

#define ZH_ERR_FUNCNAME                 ( ( const char * ) ( ZH_PTRUINT ) 1 )

/* Standard API */

extern ZH_EXPORT PZH_ITEM     zh_errGetCargo          ( PZH_ITEM pError );
extern ZH_EXPORT PZH_ITEM     zh_errGetArgs           ( PZH_ITEM pError );
extern ZH_EXPORT const char * zh_errGetDescription    ( PZH_ITEM pError );
extern ZH_EXPORT const char * zh_errGetFileName       ( PZH_ITEM pError );
extern ZH_EXPORT ZH_USHORT    zh_errGetFlags          ( PZH_ITEM pError );
extern ZH_EXPORT ZH_ERRCODE   zh_errGetGenCode        ( PZH_ITEM pError );
extern ZH_EXPORT const char * zh_errGetOperation      ( PZH_ITEM pError );
extern ZH_EXPORT ZH_ERRCODE   zh_errGetOsCode         ( PZH_ITEM pError );
extern ZH_EXPORT ZH_USHORT    zh_errGetSeverity       ( PZH_ITEM pError );
extern ZH_EXPORT ZH_ERRCODE   zh_errGetSubCode        ( PZH_ITEM pError );
extern ZH_EXPORT const char * zh_errGetSubSystem      ( PZH_ITEM pError );
extern ZH_EXPORT ZH_USHORT    zh_errGetTries          ( PZH_ITEM pError );
extern ZH_EXPORT ZH_USHORT    zh_errLaunch            ( PZH_ITEM pError );
extern ZH_EXPORT PZH_ITEM     zh_errNew               ( void );
extern ZH_EXPORT PZH_ITEM     zh_errPutCargo          ( PZH_ITEM pError, PZH_ITEM pCargo );
extern ZH_EXPORT PZH_ITEM     zh_errPutArgsArray      ( PZH_ITEM pError, PZH_ITEM pArgs );
extern ZH_EXPORT PZH_ITEM     zh_errPutArgs           ( PZH_ITEM pError, ZH_ULONG ulArgCount, ... );
extern ZH_EXPORT PZH_ITEM     zh_errPutDescription    ( PZH_ITEM pError, const char * szDescription );
extern ZH_EXPORT PZH_ITEM     zh_errPutFileName       ( PZH_ITEM pError, const char * szFileName );
extern ZH_EXPORT PZH_ITEM     zh_errPutFlags          ( PZH_ITEM pError, ZH_USHORT uiFlags );
extern ZH_EXPORT PZH_ITEM     zh_errPutGenCode        ( PZH_ITEM pError, ZH_ERRCODE uiGenCode );
extern ZH_EXPORT PZH_ITEM     zh_errPutOperation      ( PZH_ITEM pError, const char * szOperation );
extern ZH_EXPORT PZH_ITEM     zh_errPutOsCode         ( PZH_ITEM pError, ZH_ERRCODE uiOsCode );
extern ZH_EXPORT PZH_ITEM     zh_errPutSeverity       ( PZH_ITEM pError, ZH_USHORT uiSeverity );
extern ZH_EXPORT PZH_ITEM     zh_errPutSubCode        ( PZH_ITEM pError, ZH_ERRCODE uiSubCode );
extern ZH_EXPORT PZH_ITEM     zh_errPutSubSystem      ( PZH_ITEM pError, const char * szSubSystem );
extern ZH_EXPORT PZH_ITEM     zh_errPutTries          ( PZH_ITEM pError, ZH_USHORT uiTries );
extern ZH_EXPORT void         zh_errRelease           ( PZH_ITEM pError );

/* Ziher additions */

extern void     zh_errInit              ( void );
extern void     zh_errExit              ( void );

extern ZH_EXPORT PZH_ITEM  zh_errLaunchSubst( PZH_ITEM pError );

extern ZH_EXPORT PZH_ITEM  zh_errRT_New( ZH_USHORT uiSeverity,
                                         const char * szSubSystem,
                                         ZH_ERRCODE errGenCode,
                                         ZH_ERRCODE errSubCode,
                                         const char * szDescription,
                                         const char * szOperation,
                                         ZH_ERRCODE uiOsCode,
                                         ZH_USHORT uiFlags );

extern ZH_EXPORT PZH_ITEM  zh_errRT_New_Subst( ZH_USHORT uiSeverity,
                                         const char * szSubSystem,
                                         ZH_ERRCODE errGenCode,
                                         ZH_ERRCODE errSubCode,
                                         const char * szDescription,
                                         const char * szOperation,
                                         ZH_ERRCODE uiOsCode,
                                         ZH_USHORT uiFlags );

extern ZH_EXPORT PZH_ITEM  zh_errRT_SubstParams( const char * szSubSystem, ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation );

extern ZH_EXPORT PZH_ITEM zh_errRT_FileError( PZH_ITEM pError, const char * szSubSystem,
                                              ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode,
                                              const char * szFileName );

extern ZH_EXPORT ZH_USHORT zh_errRT_BASE        ( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_ULONG ulArgCount, ... );
extern ZH_EXPORT ZH_USHORT zh_errRT_BASE_Ext1   ( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_ERRCODE uiOsCode, ZH_USHORT uiFlags, ZH_ULONG ulArgCount, ... );
extern ZH_EXPORT PZH_ITEM  zh_errRT_BASE_Subst  ( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_ULONG ulArgCount, ... );
extern ZH_EXPORT void      zh_errRT_BASE_SubstR ( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_ULONG ulArgCount, ... );
extern ZH_EXPORT ZH_USHORT zh_errRT_TERM        ( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_ERRCODE uiOSCode, ZH_USHORT uiFlags );
extern ZH_EXPORT ZH_USHORT zh_errRT_DBCMD       ( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation );
extern ZH_EXPORT ZH_USHORT zh_errRT_DBCMD_Ext   ( ZH_ERRCODE errGenCode, ZH_ERRCODE errSubCode, const char * szDescription, const char * szOperation, ZH_USHORT uiFlags );

extern ZH_EXPORT void      zh_errInternal       ( ZH_ERRCODE errCode, const char * szText, const char * szPar1, const char * szPar2 ) ZH_NORETURN_ATTR;
extern           void      zh_errInternalRaw    ( ZH_ERRCODE errCode, const char * szText, const char * szPar1, const char * szPar2 );

/* Low-level error handling */
struct ZH_ERROR_INFO_;   /* forward declaration */
#define ZH_ERROR_HANDLE( zhfunc )   PZH_ITEM zhfunc( struct ZH_ERROR_INFO_ * ErrorInfo )
typedef ZH_ERROR_HANDLE( ZH_ERROR_HANDLER );
typedef ZH_ERROR_HANDLER * PZH_ERROR_HANDLER;

typedef struct ZH_ERROR_INFO_
{
   PZH_ERROR_HANDLER Func;
   PZH_ITEM Error;
   void * Cargo;
   struct ZH_ERROR_INFO_ * Previous;
   PZH_ITEM ErrorBlock;
} ZH_ERROR_INFO, * PZH_ERROR_INFO;

/* set/get current error handler */
extern ZH_EXPORT PZH_ERROR_INFO zh_errorHandler( PZH_ERROR_INFO pNewHandler );

/* current errorblock item */
extern ZH_EXPORT PZH_ITEM zh_errorBlock( void );

ZH_EXTERN_END

#endif /* ZH_APIERR_H_ */
