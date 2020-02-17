/*
 * Dynamic Object management and misc. Object related functions
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 * Copyright 1999 Antonio Linares <alinares@fivetech.com> (__objGetMsgList())
 * Copyright 2000 Jf. Lefebvre <jfl@mafact.com> and Ra. Cuylen <rac@mafact.com> (__objDerivedFrom())
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

#include "error.zhh"
#include "oo.zhh"

FUNCTION __objHasData( oObject, cSymbol )

   IF ! ZH_ISOBJECT( oObject ) .OR. ! ZH_ISSTRING( cSymbol )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ENDIF

   RETURN ;
      __objHasMsg( oObject, cSymbol ) .AND. ;
      __objHasMsg( oObject, "_" + cSymbol )

FUNCTION __objHasMethod( oObject, cSymbol )

   IF ! ZH_ISOBJECT( oObject ) .OR. ! ZH_ISSTRING( cSymbol )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ENDIF

   RETURN ;
      __objHasMsg( oObject, cSymbol ) .AND. ;
      ! __objHasMsg( oObject, "_" + cSymbol )

FUNCTION __objGetMsgList( oObject, lDataMethod, nClassType )

   LOCAL aInfo
   LOCAL aData
   LOCAL cName
   LOCAL nFirst

   IF ! ZH_ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ENDIF

   zh_default( @lDataMethod, .T. )

   aInfo := oObject:ClassSel( zh_defaultValue( nClasstype, ZH_MSGLISTALL ) )
   aData := {}
   nFirst := AScan( aInfo, {| n | zh_LeftEq( n, "_" ) } )

   FOR EACH cName IN aInfo

      /* Set functions begin with a leading underscore */
      IF ! zh_LeftEq( cName, "_" )

         /* Find position of matching set function in array with all symbols */
         /* If found: DATA, else: METHOD */
         IF ( AScan( aInfo, {| tmp | tmp == ( "_" + cName ) }, nFirst ) > 0 ) == lDataMethod
            AAdd( aData, cName )
         ENDIF
      ENDIF
   NEXT

   RETURN aData

FUNCTION __objGetMethodList( oObject )

   IF ! ZH_ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ENDIF

   RETURN __objGetMsgList( oObject, .F. )

FUNCTION __objGetValueList( oObject, aExcept )

   LOCAL aData
   LOCAL cSymbol

   IF ! ZH_ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ENDIF

   zh_default( @aExcept, {} )

   aData := {}
   FOR EACH cSymbol IN __objGetMsgList( oObject )
      IF zh_AScan( aExcept, cSymbol,,, .T. ) == 0
         AAdd( aData, { cSymbol, __objSendMsg( oObject, cSymbol ) } )
      ENDIF
   NEXT

   RETURN aData

FUNCTION __objSetValueList( oObject, aData )

   IF ZH_ISOBJECT( oObject )
      AEval( aData, {| aItem | __objSendMsg( oObject, "_" + aItem[ ZH_OO_DATA_SYMBOL ], aItem[ ZH_OO_DATA_VALUE ] ) } )
   ELSE
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ENDIF

   RETURN oObject

FUNCTION __objAddMethod( oObject, cSymbol, nFuncPtr )

   IF ! ZH_ISOBJECT( oObject ) .OR. ! ZH_ISSTRING( cSymbol ) .OR. ! ZH_ISSYMBOL( nFuncPtr )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ELSEIF ! __objHasMsg( oObject, cSymbol )
      __clsAddMsg( oObject:ClassH, cSymbol, nFuncPtr, ZH_OO_MSG_METHOD, , 1 )
   ENDIF

   RETURN oObject

FUNCTION __objAddInline( oObject, cSymbol, bInline )

   IF ! ZH_ISOBJECT( oObject ) .OR. ! ZH_ISSTRING( cSymbol )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ELSEIF ! __objHasMsg( oObject, cSymbol )
      __clsAddMsg( oObject:ClassH, cSymbol, bInline, ZH_OO_MSG_INLINE, , 1 )
   ENDIF

   RETURN oObject

FUNCTION __objAddData( oObject, cSymbol )

   LOCAL nSeq, hClass

   IF ! ZH_ISOBJECT( oObject ) .OR. ! ZH_ISSTRING( cSymbol )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ELSEIF ! __objHasMsg( oObject, cSymbol ) .AND. ! __objHasMsg( oObject, "_" + cSymbol )
      hClass := oObject:ClassH
      nSeq   := __cls_IncData( hClass )         // Allocate new Seq#
      __clsAddMsg( hClass,       cSymbol, nSeq, ZH_OO_MSG_ACCESS, , 1 )
      __clsAddMsg( hClass, "_" + cSymbol, nSeq, ZH_OO_MSG_ASSIGN, , 1 )
   ENDIF

   RETURN oObject

FUNCTION __objModMethod( oObject, cSymbol, nFuncPtr )

   IF ! ZH_ISOBJECT( oObject ) .OR. ! ZH_ISSTRING( cSymbol ) .OR. ! ZH_ISSYMBOL( nFuncPtr )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ELSEIF __objHasMethod( oObject, cSymbol )
      __clsModMsg( oObject:ClassH, cSymbol, nFuncPtr )
   ENDIF

   RETURN oObject

FUNCTION __objModInline( oObject, cSymbol, bInline )

   IF ! ZH_ISOBJECT( oObject ) .OR. ! ZH_ISSTRING( cSymbol ) .OR. ! ZH_ISBLOCK( bInline )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ELSEIF __objHasMethod( oObject, cSymbol )
      __clsModMsg( oObject:ClassH, cSymbol, bInline )
   ENDIF

   RETURN oObject

FUNCTION __objDelMethod( oObject, cSymbol )

   IF ! ZH_ISOBJECT( oObject ) .OR. ! ZH_ISSTRING( cSymbol )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ELSEIF __objHasMethod( oObject, cSymbol )
      __clsDelMsg( oObject:ClassH, cSymbol )
   ENDIF

   RETURN oObject

FUNCTION __objDelInline( oObject, cSymbol )
   RETURN __objDelMethod( oObject, cSymbol )

FUNCTION __objDelData( oObject, cSymbol )

   IF ! ZH_ISOBJECT( oObject ) .OR. ! ZH_ISSTRING( cSymbol )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ELSEIF __objHasData( oObject, cSymbol )
      __clsDelMsg( oObject:ClassH, cSymbol )
      __clsDelMsg( oObject:ClassH, "_" + cSymbol )
      __cls_DecData( oObject:ClassH )         // Decrease wData
   ENDIF

   RETURN oObject

FUNCTION __objDerivedFrom( oObject, xSuper )

   LOCAL cClassName

   IF ! ZH_ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ENDIF

   DO CASE
   CASE ZH_ISOBJECT( xSuper )
      cClassName := xSuper:ClassName()
   CASE ZH_ISSTRING( xSuper )
      cClassName := zh_asciiUpper( xSuper )
   OTHERWISE
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ENDCASE

   RETURN __clsParent( oObject:ClassH, cClassName )

FUNCTION __objGetProperties( oObject, lAllExported )

   LOCAL msg
   LOCAL aMsgList := __clsGetProperties( oObject:classH, lAllExported )

   FOR EACH msg IN aMsgList
      msg := { msg, __objSendMsg( oObject, msg ) }
   NEXT

   RETURN aMsgList
