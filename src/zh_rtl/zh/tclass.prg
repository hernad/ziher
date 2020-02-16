/*
 * Base Class for internal handling of class creation
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 2000 J. Lefebvre <jfl@mafact.com> and RA. Cuylen <rac@mafact.com>
 *    Multiple inheritance
 *    Support shared class DATA
 *    scoping (hidden, protected, readOnly)
 *    Use of __cls_param function to allow multiple superclass declaration
 *    Suppress of SetType and SetInit not needed anymore
 *    Delegation and forwarding
 *    Preparing the InitClass class method (not working!)
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    Support for inheritance, default DATA values
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

/* NOTE: This .prg is also used by the debugger subsystem,
         therefore we need this switch to avoid an infinite
         loop when launching it. [vszakats] */
#pragma -b-

/* Ziher Class HBClass to build classes */

#include "oo.zhh"

REQUEST HBObject

FUNCTION HBClass()

   STATIC s_hClass  /* NOTE: Automatically defaults to NIL */

   LOCAL hClass

   IF s_hClass == NIL .AND. __clsLockDef( @s_hClass )

      BEGIN SEQUENCE

#if 0
         hClass := __clsNew( "HBCLASS", 17,, @HBClass() )
#else
         hClass := __clsNew( "HBCLASS", 16,, @HBClass() )
#endif

         __clsAddMsg( hClass, "New"            , @New()            , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "Create"         , @Create()         , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "AddData"        , @AddData()        , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "AddMultiData"   , @AddMultiData()   , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "AddClassData"   , @AddClassData()   , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "AddMultiClsData", @AddMultiClsData(), ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "AddInline"      , @AddInline()      , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "AddMethod"      , @AddMethod()      , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "AddClsMethod"   , @AddClsMethod()   , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "AddVirtual"     , @AddVirtual()     , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "AddDelegate"    , @AddDelegate()    , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "AddFriendFunc"  , @AddFriendFunc()  , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "AddFriendClass" , @AddFriendClass() , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "Instance"       , @Instance()       , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "SetOnError"     , @SetOnError()     , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "SetDestructor"  , @SetDestructor()  , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "InitClass"      , @InitClass()      , ZH_OO_MSG_METHOD )
         __clsAddMsg( hClass, "cSuper"         , {| Self | iif( Empty( ::asSuper ), NIL, ::asSuper[ 1 ]:name ) }, ZH_OO_MSG_INLINE )
         __clsAddMsg( hClass, "hClass"         ,  1, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_hClass"        ,  1, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "cName"          ,  2, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_cName"         ,  2, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "aDatas"         ,  3, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_aDatas"        ,  3, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "aMethods"       ,  4, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_aMethods"      ,  4, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "aClsDatas"      ,  5, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_aClsDatas"     ,  5, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "aClsMethods"    ,  6, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_aClsMethods"   ,  6, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "aInlines"       ,  7, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_aInlines"      ,  7, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "aVirtuals"      ,  8, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_aVirtuals"     ,  8, ZH_OO_MSG_ASSIGN )

         __clsAddMsg( hClass, "aDelegates"     ,  9, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_aDelegates"    ,  9, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "asSuper"        , 10, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_asSuper"       , 10, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "sOnError"       , 11, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_sOnError"      , 11, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "sDestructor"    , 12, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_sDestructor"   , 12, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "lModFriendly"   , 13, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_lModFriendly"  , 13, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "asFriendClass"  , 14, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_asFriendClass" , 14, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "asFriendFunc"   , 15, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_asFriendFunc"  , 15, ZH_OO_MSG_ASSIGN )
         __clsAddMsg( hClass, "sClassFunc"     , 16, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_sClassFunc"    , 16, ZH_OO_MSG_ASSIGN )
#if 0
         __clsAddMsg( hClass, "class"          , 17, ZH_OO_MSG_ACCESS )
         __clsAddMsg( hClass, "_class"         , 17, ZH_OO_MSG_ASSIGN )
#endif

      ALWAYS

         __clsUnlockDef( @s_hClass, hClass )

      END SEQUENCE

   ENDIF

   RETURN __clsInst( s_hClass )

/* xSuper is used here as the new preprocessor file (hbclass.ch) send here
   always an array (if no superclass, this will be an empty one)
   In case of direct class creation (without the help of preprocessor) xSuper can be
   either NIL or contain the name of the superclass. */

STATIC FUNCTION New( cClassName, xSuper, sClassFunc, lModuleFriendly )

   LOCAL Self := QSelf()
   LOCAL i

   DO CASE
   CASE ZH_ISSYMBOL( xSuper )
      ::asSuper := { xSuper }
   CASE Empty( xSuper )
      ::asSuper := {}
   CASE ZH_ISSTRING( xSuper )
      ::asSuper := { __dynsN2Sym( xSuper ) }
   CASE ZH_ISARRAY( xSuper )
      ::asSuper := {}
      FOR EACH i IN xSuper
         DO CASE
         CASE ZH_ISSYMBOL( i )
            AAdd( ::asSuper, i )
         CASE ZH_ISSTRING( i ) .AND. ! Empty( i )
            AAdd( ::asSuper, __dynsN2Sym( i ) )
         ENDCASE
      NEXT
   ENDCASE

   ::cName         := zh_asciiUpper( cClassName )
   ::sClassFunc    := sClassFunc
   ::lModFriendly  := zh_defaultValue( lModuleFriendly, .F. )

   ::aDatas        := {}
   ::aMethods      := {}
   ::aClsDatas     := {}
   ::aClsMethods   := {}
   ::aInlines      := {}
   ::aVirtuals     := {}
   ::aDelegates    := {}
   ::asFriendClass := {}
   ::asFriendFunc  := {}

   RETURN QSelf()

STATIC PROCEDURE Create( /* MetaClass */ )

   LOCAL Self := QSelf()
   LOCAL n
   LOCAL nClassBegin
   LOCAL hClass
   LOCAL ahSuper := {}

#if 0
   ::Class := MetaClass
#endif

   FOR EACH n IN ::asSuper
      IF ( hClass := __clsInstSuper( n ) ) != 0  /* Super handle available */
         AAdd( ahSuper, hClass )
      ENDIF
   NEXT

   ::hClass := hClass := __clsNew( ::cName, Len( ::aDatas ), ahSuper, ::sClassFunc, ::lModFriendly )

   IF ! Empty( ahSuper ) .AND. ahSuper[ 1 ] != 0
      __clsAddMsg( hClass, "SUPER"  , 0, ZH_OO_MSG_SUPER, ahSuper[ 1 ], ZH_OO_CLSTP_EXPORTED + ZH_OO_CLSTP_NONVIRTUAL )
      __clsAddMsg( hClass, "__SUPER", 0, ZH_OO_MSG_SUPER, ahSuper[ 1 ], ZH_OO_CLSTP_EXPORTED + ZH_OO_CLSTP_NONVIRTUAL )
   ENDIF
   __clsAddMsg( hClass, "REALCLASS", 0, ZH_OO_MSG_REALCLASS, 0, ZH_OO_CLSTP_EXPORTED )

#if 0
   /* We will work here on the MetaClass object to add the Class Method
      as needed */
   FOR EACH n IN ::aClsMethods
      // do it
   NEXT
#endif

   /* Local messages */

   FOR EACH n IN ::aDatas
      __clsAddMsg( hClass, n[ ZH_OO_DATA_SYMBOL ]       , n:__enumIndex(), ;
                   ZH_OO_MSG_ACCESS, n[ ZH_OO_DATA_VALUE ], n[ ZH_OO_DATA_SCOPE ] )
      __clsAddMsg( hClass, "_" + n[ ZH_OO_DATA_SYMBOL ] , n:__enumIndex(), ;
                   ZH_OO_MSG_ASSIGN, n[ ZH_OO_DATA_TYPE ] , n[ ZH_OO_DATA_SCOPE ] )
   NEXT

   FOR EACH n IN ::aMethods
      __clsAddMsg( hClass, n[ ZH_OO_MTHD_SYMBOL ], n[ ZH_OO_MTHD_PFUNCTION ], ;
                   ZH_OO_MSG_METHOD, , n[ ZH_OO_MTHD_SCOPE ] )
   NEXT

   nClassBegin := __cls_CntClsData( hClass )
   FOR EACH n IN ::aClsDatas
      __clsAddMsg( hClass, n[ ZH_OO_CLSD_SYMBOL ]      , n:__enumIndex() + nClassBegin, ;
                   ZH_OO_MSG_CLSACCESS, n[ ZH_OO_CLSD_VALUE ], n[ ZH_OO_CLSD_SCOPE ] )
      __clsAddMsg( hClass, "_" + n[ ZH_OO_CLSD_SYMBOL ], n:__enumIndex() + nClassBegin, ;
                   ZH_OO_MSG_CLSASSIGN,                      , n[ ZH_OO_CLSD_SCOPE ] )
   NEXT

   FOR EACH n IN ::aInlines
      __clsAddMsg( hClass, n[ ZH_OO_MTHD_SYMBOL ], n[ ZH_OO_MTHD_PFUNCTION ], ;
                   ZH_OO_MSG_INLINE, , n[ ZH_OO_MTHD_SCOPE ] )
   NEXT

   FOR EACH n IN ::aVirtuals
      __clsAddMsg( hClass, n, n:__enumIndex(), ZH_OO_MSG_VIRTUAL )
   NEXT

   FOR EACH n IN ::aDelegates
      __clsAddMsg( ::hClass, n[ ZH_OO_DELEG_SYMBOL ], n[ ZH_OO_DELEG_MESSAGE ], ;
                   ZH_OO_MSG_DELEGATE, n[ ZH_OO_DELEG_OBJECT ], ;
                   n[ ZH_OO_DELEG_SCOPE ] )
   NEXT

   IF ::sOnError != NIL
      __clsAddMsg( hClass, "__OnError", ::sOnError, ZH_OO_MSG_ONERROR )
   ENDIF

   IF ::sDestructor != NIL
      __clsAddMsg( hClass, "__Destructor", ::sDestructor, ZH_OO_MSG_DESTRUCTOR )
   ENDIF

   /* Friend Classes */
   FOR EACH n IN ::asFriendClass
      __clsAddFriend( ::hClass, n )
   NEXT

   /* Friend Functions */
   FOR EACH n IN ::asFriendFunc
      __clsAddFriend( ::hClass, n )
   NEXT

   RETURN

STATIC FUNCTION Instance()
   RETURN __clsInst( QSelf():hClass )

STATIC PROCEDURE AddData( cData, xInit, cType, nScope, lNoinit )

   /* Default Init for Logical and numeric */
   IF ! zh_defaultValue( lNoInit, .F. ) .AND. ;
      cType != NIL .AND. xInit == NIL

      SWITCH Asc( cType )
      CASE Asc( "L" )   /* Logical */
      CASE Asc( "l" )   /* Logical */
         xInit := .F.
         EXIT
      CASE Asc( "I" )   /* Numeric or Integer */
      CASE Asc( "i" )   /* Numeric or Integer */
      CASE Asc( "N" )   /* Numeric or Integer */
      CASE Asc( "n" )   /* Numeric or Integer */
         xInit := 0
         EXIT
      CASE Asc( "D" )   /* Date */
      CASE Asc( "d" )   /* Date */
         xInit := zh_SToD()
         EXIT
      CASE Asc( "T" )   /* Timestamp */
      CASE Asc( "t" )   /* Timestamp */
         xInit := zh_SToT()
         EXIT
      ENDSWITCH
   ENDIF

   AAdd( QSelf():aDatas, { cData, xInit, cType, zh_defaultValue( nScope, ZH_OO_CLSTP_EXPORTED ) } )

   RETURN

STATIC PROCEDURE AddMultiData( cType, xInit, nScope, aData, lNoInit )

   LOCAL cData

   FOR EACH cData IN aData
      IF ZH_ISSTRING( cData )
         QSelf():AddData( cData, xInit, cType, nScope, lNoInit )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE AddClassData( cData, xInit, cType, nScope, lNoInit )

   /* Default Init for Logical and numeric */
   IF ! zh_defaultValue( lNoInit, .F. ) .AND. ;
      cType != NIL .AND. xInit == NIL

      SWITCH Asc( cType )
      CASE Asc( "L" )   /* Logical */
      CASE Asc( "l" )   /* Logical */
         xInit := .F.
         EXIT
      CASE Asc( "I" )   /* Numeric or Integer */
      CASE Asc( "i" )   /* Numeric or Integer */
      CASE Asc( "N" )   /* Numeric or Integer */
      CASE Asc( "n" )   /* Numeric or Integer */
         xInit := 0
         EXIT
      CASE Asc( "D" )   /* Date */
      CASE Asc( "d" )   /* Date */
         xInit := zh_SToD()
         EXIT
      CASE Asc( "T" )   /* Timestamp */
      CASE Asc( "t" )   /* Timestamp */
         xInit := zh_SToT()
         EXIT
      ENDSWITCH
   ENDIF

   AAdd( QSelf():aClsDatas, { cData, xInit, cType, ;
                              zh_bitOr( zh_defaultValue( nScope, ZH_OO_CLSTP_EXPORTED ), ;
                                        ZH_OO_CLSTP_CLASS ) } )

   RETURN

STATIC PROCEDURE AddMultiClsData( cType, xInit, nScope, aData, lNoInit )

   LOCAL cData

   FOR EACH cData IN aData
      IF ZH_ISSTRING( cData )
         QSelf():AddClassData( cData, xInit, cType, nScope, lNoInit )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE AddInline( cMethod, bCode, nScope )

   AAdd( QSelf():aInlines, { cMethod, bCode, zh_defaultValue( nScope, ZH_OO_CLSTP_EXPORTED ) } )

   RETURN

STATIC PROCEDURE AddMethod( cMethod, sFuncSym, nScope )

   AAdd( QSelf():aMethods, { cMethod, sFuncSym, zh_defaultValue( nScope, ZH_OO_CLSTP_EXPORTED ) } )

   RETURN

STATIC PROCEDURE AddClsMethod( cMethod, sFuncSym, nScope )

   AAdd( QSelf():aClsMethods, { cMethod, sFuncSym, ;
                                zh_bitOr( zh_defaultValue( nScope, ZH_OO_CLSTP_EXPORTED ), ;
                                          ZH_OO_CLSTP_CLASS ) } )

   RETURN

STATIC PROCEDURE AddVirtual( cMethod )

   AAdd( QSelf():aVirtuals, cMethod )

   RETURN

STATIC PROCEDURE AddDelegate( xMethod, cDelegMsg, cObject, nScope )

   LOCAL mth

   DO CASE
   CASE ZH_ISSTRING( xMethod )
      AAdd( QSelf():aDelegates, { xMethod, cDelegMsg, cObject, nScope } )
   CASE ZH_ISARRAY( xMethod )
      FOR EACH mth IN xMethod
         AAdd( QSelf():aDelegates, { mth, cDelegMsg, cObject, nScope } )
      NEXT
   ENDCASE

   RETURN

STATIC PROCEDURE AddFriendClass( ... )

   LOCAL Self := QSelf()

   AEval( zh_AParams(), {| sClass | AAdd( ::asFriendClass, sClass ) } )

   RETURN

STATIC PROCEDURE AddFriendFunc( ... )

   LOCAL Self := QSelf()

   AEval( zh_AParams(), {| sFunc | AAdd( ::asFriendFunc, sFunc ) } )

   RETURN

STATIC PROCEDURE SetOnError( sFuncPtr )

   QSelf():sOnError := sFuncPtr

   RETURN

STATIC PROCEDURE SetDestructor( sFuncPtr )

   QSelf():sDestructor := sFuncPtr

   RETURN

STATIC FUNCTION InitClass()
   RETURN QSelf()
