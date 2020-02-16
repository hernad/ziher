/*
 * Get Class helpers
 *
 * Copyright 2000 Ron Pinkas <Ron@Profit-Master.com>
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

REQUEST zh_PValue

FUNCTION __Get( bSetGet, cVarName, cPicture, bValid, bWhen )

   LOCAL oGet

   IF ! ZH_ISSTRING( cVarName )
      RETURN NIL
   ENDIF

   IF ! ZH_ISEVALITEM( bSetGet )
      IF FieldPos( cVarName ) > 0
         bSetGet := FieldWBlock( cVarName, Select() )
      ELSEIF ( bSetGet := MemVarBlock( cVarName ) ) == NIL
         /* If cVarName is not a field name in current workarea then
          * CA-Cl*pper always tries to create SET/GET block for memvar.
          * If it cannot (i.e. cVarName is complex expression) then it
          * macro-compile simple SET/GET block for it. [druzus]
          */
         bSetGet := zh_macroBlock( "iif(ZH_PValue(1)==NIL," + cVarName + "," + cVarName + ":=zh_PValue(1))" )
      ENDIF
   ENDIF

   /* The Eval() below is executed to force the same RTE as in
    * CA-Cl*pper so user can create memvar dynamically in his
    * custom error handler. [druzus]
    */
   Eval( bSetGet )

   oGet := GetNew( ,, bSetGet, cVarName, cPicture )

   oGet:PreBlock := bWhen
   oGet:PostBlock := bValid

   RETURN oGet

FUNCTION __GetA( bGetArray, cVarName, cPicture, bValid, bWhen, aIndex )

   LOCAL oGet

   IF ! ZH_ISSTRING( cVarName ) .OR. ! ZH_ISARRAY( aIndex )
      RETURN NIL
   ENDIF

   IF ! ZH_ISEVALITEM( bGetArray )
      /* CA-Cl*pper creates standard SET/GET block here */
      IF FieldPos( cVarName ) > 0
         bGetArray := FieldWBlock( cVarName, Select() )
      ELSE
         DO WHILE ( bGetArray := MemVarBlock( cVarName ) ) == NIL
            __mvGet( cVarName )
         ENDDO
      ENDIF
   ENDIF

   IF ! ValType( Eval( bGetArray ) ) $ "AH"
      RETURN NIL
   ENDIF

   oGet := GetNew( ,, bGetArray, cVarName, cPicture )
   oGet:SubScript := aIndex

   oGet:PreBlock := bWhen
   oGet:PostBlock := bValid

   RETURN oGet
