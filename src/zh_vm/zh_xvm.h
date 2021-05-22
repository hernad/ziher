/*
 * Header file for the Internal Terminal API
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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

#ifndef ZH_XVM_H_
#define ZH_XVM_H_

#include "zh_api.h"
#include "zh_item_api.h"
#include "zh_vm.h"
#include "zh_stack.h"

ZH_EXTERN_BEGIN

/*
 * Functions below which return ZH_BOOL value can cause error handler
 * executing so we have to check for break request flag. The return
 * value is ZH_TRUE when break request is set.
 * other functions does not execute error handler and we do not have
 * to check ZHVM state so they are simply declared as void.
 */

extern ZH_EXPORT void    zh_xvmExitProc( void );
extern ZH_EXPORT void    zh_xvmEndProc( void );
extern ZH_EXPORT void    zh_xvmSeqBegin( void );
extern ZH_EXPORT ZH_BOOL zh_xvmSeqEnd( void );
extern ZH_EXPORT ZH_BOOL zh_xvmSeqEndTest( void );
extern ZH_EXPORT ZH_BOOL zh_xvmSeqRecover( void );
extern ZH_EXPORT void    zh_xvmSeqAlways( void );
extern ZH_EXPORT ZH_BOOL zh_xvmAlwaysBegin( void );
extern ZH_EXPORT ZH_BOOL zh_xvmAlwaysEnd( void );
extern ZH_EXPORT ZH_BOOL zh_xvmSeqBlock( void );

extern ZH_EXPORT ZH_BOOL zh_xvmEnumStart( int, int );              /* prepare FOR EACH loop */
extern ZH_EXPORT ZH_BOOL zh_xvmEnumNext( void );                   /* increment FOR EACH loop counter */
extern ZH_EXPORT ZH_BOOL zh_xvmEnumPrev( void );                   /* decrement FOR EACH loop counter */
extern ZH_EXPORT void    zh_xvmEnumEnd( void );                    /* rewind the stack after FOR EACH loop counter */

extern ZH_EXPORT void    zh_xvmWithObjectStart( void );            /* prepare WITH OBJECT statement */
extern ZH_EXPORT void    zh_xvmWithObjectEnd( void );              /* rewind the stack after normal WITH OBJECT */
extern ZH_EXPORT void    zh_xvmWithObjectMessage( PZH_SYMBOL );      /* send WITH OBJECT message to current WITH OBJECT control variable */

extern ZH_EXPORT ZH_BOOL zh_xvmSwitchGet( PZH_ITEM * );

extern ZH_EXPORT void    zh_xvmSetLine( ZH_USHORT uiLine );           /* set .zh line number information */

extern ZH_EXPORT void    zh_xvmFrame( int iLocals, int iParams );  /* increases the stack pointer for the amount of locals and params supplied */
extern ZH_EXPORT void    zh_xvmVFrame( int iLocals, int iParams ); /* increases the stack pointer for the amount of locals and variable params */
extern ZH_EXPORT void    zh_xvmSFrame( PZH_SYMBOL pSymbol );
extern ZH_EXPORT void    zh_xvmStatics( PZH_SYMBOL pSymbol, ZH_USHORT uiStatics );
extern ZH_EXPORT void    zh_xvmThreadStatics( ZH_USHORT uiStatics, const ZH_BYTE * statics );
extern ZH_EXPORT void    zh_xvmParameter( PZH_SYMBOL pSymbol, int iParams );
extern ZH_EXPORT void    zh_xvmRetValue( void );                   /* pops the latest stack value into stack.Return */
extern ZH_EXPORT void    zh_xvmRetNil( void );
extern ZH_EXPORT void    zh_xvmRetInt( ZH_LONG lValue );
extern ZH_EXPORT ZH_BOOL zh_xvmDo( ZH_USHORT uiParams );
extern ZH_EXPORT ZH_BOOL zh_xvmFunction( ZH_USHORT uiParams );
extern ZH_EXPORT ZH_BOOL zh_xvmSend( ZH_USHORT uiParams );
extern ZH_EXPORT ZH_BOOL zh_xvmPushObjectVarRef( void );
extern ZH_EXPORT void    zh_xvmPushStatic( ZH_USHORT uiStatic );
extern ZH_EXPORT void    zh_xvmPushStaticByRef( ZH_USHORT uiStatic );
extern ZH_EXPORT void    zh_xvmPopStatic( ZH_USHORT uiStatic );
extern ZH_EXPORT ZH_BOOL zh_xvmPushVariable( PZH_SYMBOL pSymbol );
extern ZH_EXPORT ZH_BOOL zh_xvmPopVariable( PZH_SYMBOL pSymbol );
extern ZH_EXPORT void    zh_xvmPushBlock( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols ); /* creates a codeblock */
extern ZH_EXPORT void    zh_xvmPushBlockShort( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols ); /* creates a codeblock */
extern ZH_EXPORT void    zh_xvmPushBlockLarge( const ZH_BYTE * pCode, PZH_SYMBOL pSymbols ); /* creates a codeblock */
extern ZH_EXPORT void    zh_xvmPushSelf( void );
extern ZH_EXPORT void    zh_xvmPushVParams( void );
extern ZH_EXPORT void    zh_xvmPushAParams( void );
extern ZH_EXPORT void    zh_xvmPushLocal( ZH_SHORT iLocal );          /* pushes the content of a local onto the stack */
extern ZH_EXPORT void    zh_xvmPushLocalByRef( ZH_SHORT iLocal );     /* pushes a local by reference onto the stack */
extern ZH_EXPORT void    zh_xvmPopLocal( ZH_SHORT iLocal );           /* pops the stack latest value onto a local */
extern ZH_EXPORT ZH_BOOL zh_xvmPushField( PZH_SYMBOL pSymbol );
extern ZH_EXPORT ZH_BOOL zh_xvmPopField( PZH_SYMBOL pSymbol );
extern ZH_EXPORT ZH_BOOL zh_xvmPushMemvar( PZH_SYMBOL pSymbol );
extern ZH_EXPORT ZH_BOOL zh_xvmPushMemvarByRef( PZH_SYMBOL pSymbol );
extern ZH_EXPORT ZH_BOOL zh_xvmPopMemvar( PZH_SYMBOL pSymbol );
extern ZH_EXPORT ZH_BOOL zh_xvmPushAliasedField( PZH_SYMBOL pSymbol );
extern ZH_EXPORT ZH_BOOL zh_xvmPopAliasedField( PZH_SYMBOL pSymbol );
extern ZH_EXPORT ZH_BOOL zh_xvmPushAliasedFieldExt( PZH_SYMBOL pAlias, PZH_SYMBOL pField );
extern ZH_EXPORT ZH_BOOL zh_xvmPopAliasedFieldExt( PZH_SYMBOL pAlias, PZH_SYMBOL pField );
extern ZH_EXPORT ZH_BOOL zh_xvmPushAliasedVar( PZH_SYMBOL pSymbol );
extern ZH_EXPORT ZH_BOOL zh_xvmPopAliasedVar( PZH_SYMBOL pSymbol );
extern ZH_EXPORT ZH_BOOL zh_xvmPushAlias( void );
extern ZH_EXPORT ZH_BOOL zh_xvmPopAlias( void );                   /* select the workarea using a given item or a substituted value */
extern ZH_EXPORT ZH_BOOL zh_xvmPopLogical( ZH_BOOL * );            /* pops the stack latest value and returns its logical value */
extern ZH_EXPORT ZH_BOOL zh_xvmSwapAlias( void );                  /* swaps items on the eval stack and pops the workarea number */
extern ZH_EXPORT ZH_BOOL zh_xvmLocalAddInt( int iLocal, ZH_LONG lAdd ); /* add integer to given local variable */
extern ZH_EXPORT ZH_BOOL zh_xvmLocalInc( int iLocal );             /* increment given local variable */
extern ZH_EXPORT ZH_BOOL zh_xvmLocalDec( int iLocal );             /* decrement given local variable */
extern ZH_EXPORT ZH_BOOL zh_xvmLocalIncPush( int iLocal );         /* increment given local variable and push it on ZHVM stack */

extern ZH_EXPORT ZH_BOOL zh_xvmAnd( void );
extern ZH_EXPORT ZH_BOOL zh_xvmOr( void );
extern ZH_EXPORT ZH_BOOL zh_xvmNot( void );
extern ZH_EXPORT ZH_BOOL zh_xvmNegate( void );
extern ZH_EXPORT void    zh_xvmDuplicate( void );
extern ZH_EXPORT void    zh_xvmDuplUnRef( void );
extern ZH_EXPORT void    zh_xvmPushUnRef( void );
extern ZH_EXPORT void    zh_xvmSwap( int iCount );
extern ZH_EXPORT ZH_BOOL zh_xvmForTest( void );
extern ZH_EXPORT void    zh_xvmFuncPtr( void );
extern ZH_EXPORT ZH_BOOL zh_xvmEqual( void );                      /* checks if the two latest values on the stack are equal, removes both and leaves result */
extern ZH_EXPORT ZH_BOOL zh_xvmExactlyEqual( void );               /* checks if the two latest values on the stack are exactly equal, removes both and leaves result */
extern ZH_EXPORT ZH_BOOL zh_xvmNotEqual( void );                   /* checks if the two latest values on the stack are not equal, removes both and leaves result */
extern ZH_EXPORT ZH_BOOL zh_xvmLess( void );                       /* checks if the latest - 1 value is less than the latest, removes both and leaves result */
extern ZH_EXPORT ZH_BOOL zh_xvmLessEqual( void );                  /* checks if the latest - 1 value is less than or equal the latest, removes both and leaves result */
extern ZH_EXPORT ZH_BOOL zh_xvmGreater( void );                    /* checks if the latest - 1 value is greater than the latest, removes both and leaves result */
extern ZH_EXPORT ZH_BOOL zh_xvmGreaterEqual( void );               /* checks if the latest - 1 value is greater than or equal the latest, removes both and leaves result */
extern ZH_EXPORT ZH_BOOL zh_xvmInstring( void );                   /* check whether string 1 is contained in string 2 */
extern ZH_EXPORT ZH_BOOL zh_xvmPlus( void );                       /* sums the latest two values on the stack, removes them and leaves the result */
extern ZH_EXPORT ZH_BOOL zh_xvmPlusEq( void );
extern ZH_EXPORT ZH_BOOL zh_xvmPlusEqPop( void );
extern ZH_EXPORT ZH_BOOL zh_xvmMinus( void );                      /* subtracts the latest two values on the stack, removes them and leaves the result */
extern ZH_EXPORT ZH_BOOL zh_xvmMinusEq( void );
extern ZH_EXPORT ZH_BOOL zh_xvmMinusEqPop( void );
extern ZH_EXPORT ZH_BOOL zh_xvmMult( void );                       /* multiplies the latest two values on the stack, removes them and leaves the result */
extern ZH_EXPORT ZH_BOOL zh_xvmMultEq( void );
extern ZH_EXPORT ZH_BOOL zh_xvmMultEqPop( void );
extern ZH_EXPORT ZH_BOOL zh_xvmDivide( void );                     /* divides the latest two values on the stack, removes them and leaves the result */
extern ZH_EXPORT ZH_BOOL zh_xvmDivEq( void );
extern ZH_EXPORT ZH_BOOL zh_xvmDivEqPop( void );
extern ZH_EXPORT ZH_BOOL zh_xvmModulus( void );                    /* calculates the modulus of latest two values on the stack, removes them and leaves the result */
extern ZH_EXPORT ZH_BOOL zh_xvmModEq( void );
extern ZH_EXPORT ZH_BOOL zh_xvmModEqPop( void );
extern ZH_EXPORT ZH_BOOL zh_xvmPower( void );
extern ZH_EXPORT ZH_BOOL zh_xvmExpEq( void );
extern ZH_EXPORT ZH_BOOL zh_xvmExpEqPop( void );
extern ZH_EXPORT ZH_BOOL zh_xvmInc( void );
extern ZH_EXPORT ZH_BOOL zh_xvmIncEq( void );
extern ZH_EXPORT ZH_BOOL zh_xvmIncEqPop( void );
extern ZH_EXPORT ZH_BOOL zh_xvmDec( void );
extern ZH_EXPORT ZH_BOOL zh_xvmDecEq( void );
extern ZH_EXPORT ZH_BOOL zh_xvmDecEqPop( void );

extern ZH_EXPORT void    zh_xvmArrayDim( ZH_USHORT uiDimensions ); /* generates an uiDimensions Array and initialize those dimensions from the stack values */
extern ZH_EXPORT void    zh_xvmArrayGen( ZH_SIZE nElements );      /* generates an nElements Array and fills it from the stack values */
extern ZH_EXPORT ZH_BOOL zh_xvmArrayPush( void );                  /* pushes an array element to the stack, removing the array and the index from the stack */
extern ZH_EXPORT ZH_BOOL zh_xvmArrayPushRef( void );               /* pushes a reference to an array element to the stack, removing the array and the index from the stack */
extern ZH_EXPORT ZH_BOOL zh_xvmArrayPop( void );                   /* pops a value from the stack */
extern ZH_EXPORT void    zh_xvmHashGen( ZH_SIZE nElements );       /* generates an nElements Hash and fills it from the stack values */

extern ZH_EXPORT void    zh_xvmLocalName( ZH_USHORT uiLocal, const char * szLocalName );
extern ZH_EXPORT void    zh_xvmStaticName( ZH_BYTE bIsGlobal, ZH_USHORT uiStatic, const char * szStaticName );
extern ZH_EXPORT void    zh_xvmModuleName( const char * szModuleName );

extern ZH_EXPORT ZH_BOOL zh_xvmMacroDo( ZH_USHORT uiArgSets );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroFunc( ZH_USHORT uiArgSets );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroSend( ZH_USHORT uiArgSets );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroArrayGen( ZH_USHORT uiArgSets );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroPush( int bFlags );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroPushRef( void );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroPushIndex( void );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroPushArg( PZH_SYMBOL pSymbol, int bFlags );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroPushList( int bFlags );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroPushAliased( int bFlags );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroPushPare( int bFlags );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroPop( int bFlags );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroPopAliased( int bFlags );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroSymbol( void );
extern ZH_EXPORT ZH_BOOL zh_xvmMacroText( void );

extern ZH_EXPORT void    zh_xvmPushStringHidden( int iMethod, const char * szText, ZH_SIZE nSize );
extern ZH_EXPORT void    zh_xvmPushDouble( double dNumber, int iWidth, int iDec );
#ifdef ZH_LONG_LONG_OFF
extern ZH_EXPORT void    zh_xvmPushLongLong( double dNumber );
#else
extern ZH_EXPORT void    zh_xvmPushLongLong( ZH_LONGLONG llNumber );
#endif

#define zh_xvmPushLogical( f )            zh_vmPushLogical( f )
#define zh_xvmPushInteger( i )            zh_vmPushInteger( i )
#define zh_xvmPushLong( l )               zh_vmPushLong( l )
#define zh_xvmPushNil()                   zh_vmPushNil()
#define zh_xvmPushStringConst( psz, ul )  zh_vmPushStringPcode( psz, ul )
#define zh_xvmPushSymbol( p )             zh_vmPushSymbol( p )
#define zh_xvmPushDate( l )               zh_vmPushDate( l )
#define zh_xvmPushTimeStamp( d, t )       zh_vmPushTimeStamp( d, t )


/*
 * additional multi PCODE operations
 */
extern ZH_EXPORT ZH_BOOL zh_xvmArrayItemPush( ZH_SIZE nIndex );
extern ZH_EXPORT ZH_BOOL zh_xvmArrayItemPop( ZH_SIZE nIndex );
extern ZH_EXPORT ZH_BOOL zh_xvmMultByInt( ZH_LONG lValue );
extern ZH_EXPORT ZH_BOOL zh_xvmDivideByInt( ZH_LONG lValue );
extern ZH_EXPORT ZH_BOOL zh_xvmModulusByInt( ZH_LONG lValue );
extern ZH_EXPORT ZH_BOOL zh_xvmAddInt( ZH_LONG lValue );
extern ZH_EXPORT void zh_xvmLocalSetInt( int iLocal, ZH_LONG lValue );
/*extern ZH_EXPORT void zh_xvmLocalSetStr( int iLocal, const char * pValue, ZH_SIZE nLen );*/
extern ZH_EXPORT void zh_xvmPushFuncSymbol( PZH_SYMBOL pSym );

extern ZH_EXPORT ZH_BOOL zh_xvmLessThenInt( ZH_LONG lValue );
extern ZH_EXPORT ZH_BOOL zh_xvmLessThenIntIs( ZH_LONG lValue, ZH_BOOL * fValue );
extern ZH_EXPORT ZH_BOOL zh_xvmLessEqualThenInt( ZH_LONG lValue );
extern ZH_EXPORT ZH_BOOL zh_xvmLessEqualThenIntIs( ZH_LONG lValue, ZH_BOOL * fValue );
extern ZH_EXPORT ZH_BOOL zh_xvmGreaterThenInt( ZH_LONG lValue );
extern ZH_EXPORT ZH_BOOL zh_xvmGreaterThenIntIs( ZH_LONG lValue, ZH_BOOL * fValue );
extern ZH_EXPORT ZH_BOOL zh_xvmGreaterEqualThenInt( ZH_LONG lValue );
extern ZH_EXPORT ZH_BOOL zh_xvmGreaterEqualThenIntIs( ZH_LONG lValue, ZH_BOOL * fValue );
extern ZH_EXPORT ZH_BOOL zh_xvmEqualInt( ZH_LONG lValue );
extern ZH_EXPORT ZH_BOOL zh_xvmEqualIntIs( ZH_LONG lValue, ZH_BOOL * fValue );
extern ZH_EXPORT ZH_BOOL zh_xvmNotEqualInt( ZH_LONG lValue );
extern ZH_EXPORT ZH_BOOL zh_xvmNotEqualIntIs( ZH_LONG lValue, ZH_BOOL * fValue );

extern ZH_EXPORT ZH_BOOL zh_xvmLocalAdd( int iLocal );
extern ZH_EXPORT ZH_BOOL zh_xvmStaticAdd( ZH_USHORT uiStatic );
extern ZH_EXPORT ZH_BOOL zh_xvmMemvarAdd( PZH_SYMBOL pSymbol );

extern ZH_EXPORT void zh_xvmCopyLocals( int iDest, int iSource );

ZH_EXTERN_END

#endif /* ZH_XVM_H_ */
