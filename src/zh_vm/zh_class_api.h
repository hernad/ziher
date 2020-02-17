/*
 * Ziher class API
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

#ifndef ZH_APICLS_H_
#define ZH_APICLS_H_

#include "zh_api.h"

ZH_EXTERN_BEGIN

#ifdef _ZH_API_INTERNAL_

#define ZH_OO_OP_PLUS         0
#define ZH_OO_OP_MINUS        1
#define ZH_OO_OP_MULT         2
#define ZH_OO_OP_DIVIDE       3
#define ZH_OO_OP_MOD          4
#define ZH_OO_OP_POWER        5
#define ZH_OO_OP_INC          6
#define ZH_OO_OP_DEC          7
#define ZH_OO_OP_EQUAL        8
#define ZH_OO_OP_EXACTEQUAL   9
#define ZH_OO_OP_NOTEQUAL     10
#define ZH_OO_OP_LESS         11
#define ZH_OO_OP_LESSEQUAL    12
#define ZH_OO_OP_GREATER      13
#define ZH_OO_OP_GREATEREQUAL 14
#define ZH_OO_OP_ASSIGN       15
#define ZH_OO_OP_INSTRING     16
#define ZH_OO_OP_INCLUDE      17
#define ZH_OO_OP_NOT          18
#define ZH_OO_OP_AND          19
#define ZH_OO_OP_OR           20
#define ZH_OO_OP_ARRAYINDEX   21
#define ZH_OO_OP_ENUMINDEX    22
#define ZH_OO_OP_ENUMBASE     23
#define ZH_OO_OP_ENUMVALUE    24
#define ZH_OO_OP_ENUMSTART    25
#define ZH_OO_OP_ENUMSKIP     26
#define ZH_OO_OP_ENUMSTOP     27
#define ZH_OO_OP_ENUMISFIRST  28
#define ZH_OO_OP_ENUMISLAST   29

#define ZH_OO_MAX_OPERATOR    29

extern void       zh_clsInit( void );           /* initialize Classy/OO system at HVM startup */
extern void       zh_clsDoInit( void );         /* initialize Classy/OO system .zh functions */
extern void       zh_clsReleaseAll( void );     /* releases all defined classes */
extern void       zh_clsIsClassRef( void );     /* classes.c - mark all class internals as used */
extern ZH_BOOL    zh_clsHasDestructor( ZH_USHORT uiClass );
extern PZH_SYMB   zh_clsMethodSym( PZH_ITEM pBaseSymbol ); /* returns the real method symbol for given stack symbol */

extern PZH_SYMB   zh_objGetMethod( PZH_ITEM pObject, PZH_SYMB pSymMsg, PZH_STACK_STATE pStack ); /* returns the method pointer of an object class */
extern ZH_BOOL    zh_objGetVarRef( PZH_ITEM pObject, PZH_SYMB pMessage, PZH_STACK_STATE pStack ); /* create object variable reference */
extern ZH_BOOL    zh_objHasOperator( PZH_ITEM pObject, ZH_USHORT uiOperator );
extern ZH_BOOL    zh_objOperatorCall( ZH_USHORT uiOperator, PZH_ITEM pResult, PZH_ITEM pObject, PZH_ITEM pMsgArg1, PZH_ITEM pMsgArg2 );
extern void       zh_objDestructorCall( PZH_ITEM pObject );
extern PZH_ITEM   zh_objCloneTo( PZH_ITEM pDest, PZH_ITEM pObject );
extern void       zh_objCloneBody( PZH_ITEM pDest, PZH_ITEM pObject, PZH_NESTED_CLONED pClonedList );

#ifndef ZH_NO_PROFILER
/* profiler for object management */
extern void       zh_mthAddTime( ZH_ULONG ulClockTicks );       /* profiler from classes.c */
#endif

#endif   /* _ZH_API_INTERNAL_ */

/* class management */
extern ZH_EXPORT const char * zh_clsName( ZH_USHORT uiClass );
extern ZH_EXPORT const char * zh_clsFuncName( ZH_USHORT uiClass );
extern ZH_EXPORT const char * zh_clsMethodName( ZH_USHORT uiClass, ZH_USHORT uiMethod );
extern ZH_EXPORT PZH_SYMB   zh_clsFuncSym( ZH_USHORT uiClass );
extern ZH_EXPORT ZH_BOOL    zh_clsIsParent( ZH_USHORT uiClass, const char * szParentName ); /* is a class handle inherited from szParentName Class ? */
extern ZH_EXPORT ZH_SIZE    zh_clsGetVarIndex( ZH_USHORT uiClass, PZH_DYNS pVarSym );
extern ZH_EXPORT ZH_USHORT  zh_clsFindClass( const char * szClass, const char * szClassFunc );

/* object management */
extern ZH_EXPORT ZH_USHORT  zh_objGetClass( PZH_ITEM pItem );      /* get object class handle */
extern ZH_EXPORT ZH_USHORT  zh_objSetClass( PZH_ITEM pItem, const char * szClass, const char * szFunc );    /* get object class handle using class name and class function name */
extern ZH_EXPORT const char * zh_objGetClsName( PZH_ITEM pObject );  /* retrieves an object class name */
extern ZH_EXPORT const char * zh_objGetRealClsName( PZH_ITEM pObject, const char * szString  ); /* retrieves an object class name for a specific message */

extern ZH_EXPORT ZH_BOOL    zh_objHasMsg( PZH_ITEM pObject, const char * szString ); /* returns ZH_TRUE/ZH_FALSE whether szString is an existing message for object */
extern ZH_EXPORT ZH_BOOL    zh_objHasMessage( PZH_ITEM pObject, PZH_DYNS pMessage );
extern ZH_EXPORT PZH_ITEM   zh_objSendMsg( PZH_ITEM pObj, const char *sMsg, ZH_ULONG ulArg, ... );
extern ZH_EXPORT PZH_ITEM   zh_objSendMessage( PZH_ITEM pObj, PZH_DYNS pMessage, ZH_ULONG ulArg, ... );

extern ZH_EXPORT PZH_ITEM   zh_objGetVarPtr( PZH_ITEM pObject, PZH_DYNS pVarMsg );

/* send message which allows to set execution context for debugger */
extern ZH_EXPORT void       zh_dbg_objSendMessage( int iProcLevel, PZH_ITEM pObject, PZH_ITEM pMessage, int iParamOffset );

extern ZH_EXPORT ZH_USHORT  zh_clsCreate( ZH_USHORT usSize, const char * szClassName );
extern ZH_EXPORT void       zh_clsAdd( ZH_USHORT usClassH, const char * szMethodName, PZH_FUNC pFuncPtr ); 
extern ZH_EXPORT void       zh_clsAssociate( ZH_USHORT usClassH ); 

ZH_EXTERN_END

#endif /* ZH_APICLS_H_ */
