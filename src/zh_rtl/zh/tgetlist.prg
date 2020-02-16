/*
 * HBGetList Class
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 2001 Luiz Rafael Culik (Support for CA-Cl*pper 5.3 GET-system)
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

#include "hbclass.ch"

#include "button.ch"
#include "getexit.ch"
#include "inkey.zhh"
#include "setcurs.ch"
#include "tbrowse.ch"

#define SCORE_ROW       0
#define SCORE_COL       60

#define _GET_INSERT_ON  7
#define _GET_INSERT_OFF 8
#define _GET_INVD_DATE  9

#define K_UNDO          K_CTRL_U

#define MSGFLAG         1
#define MSGROW          2
#define MSGLEFT         3
#define MSGRIGHT        4
#define MSGCOLOR        5

CREATE CLASS HBGetList

   EXPORTED:

   VAR HasFocus        AS LOGICAL   INIT .F.


   METHOD ReadModal( nPos, oMenu, nMsgRow, nMsgLeft, nMsgRight, cMsgColor )
   METHOD Settle( nPos, lInit )
   METHOD Reader( oMenu, aMsg )
   METHOD GetApplyKey( nKey, oGet, oMenu, aMsg )
   METHOD GetPreValidate( oGet, aMsg )
   METHOD GetPostValidate( oGet, aMsg )
   METHOD GetDoSetKey( bKeyBlock, oGet )
   METHOD PostActiveGet()
   METHOD GetReadVar()
   METHOD SetFormat( bFormat )
   METHOD KillRead( lKill )
   METHOD GetActive( oGet )
   METHOD DateMsg()
   METHOD ShowScoreBoard()
   METHOD ReadUpdated( lUpdated )
   METHOD ReadVar( cNewVarName )
   METHOD SetFocus()
   METHOD Updated()                                  // returns ::lUpdated
   METHOD Get()                                      // returns ::oGet

   METHOD GUIReader( oGet, oMenu, aMsg )
   METHOD GUIApplyKey( oGet, oGUI, nKey, oMenu, aMsg )
   METHOD GUIPreValidate( oGet, oGUI, aMsg )
   METHOD GUIPostValidate( oGet, oGUI, aMsg )
   METHOD TBApplyKey( oGet, oTB, nKey, oMenu, aMsg )
   METHOD TBReader( oGet, oMenu, aMsg )
   METHOD Accelerator( nKey, aMsg )
   METHOD hitTest( nMRow, nMCol, aMsg )
   METHOD ReadStats( nElement, xNewValue )
   METHOD ShowGetMsg( oGet, aMsg )
   METHOD EraseGetMsg( aMsg )

   METHOD New( GetList )

   PROTECTED:

   VAR oGet
   VAR aGetList

   VAR lUpdated        AS LOGICAL   INIT .F.
   VAR bFormat
   VAR lKillRead       AS LOGICAL   INIT .F.
   VAR lBumpTop        AS LOGICAL   INIT .F.
   VAR lBumpBot        AS LOGICAL   INIT .F.
   VAR nLastExitState               INIT 0
   VAR nLastPos        AS NUMERIC   INIT 0
   VAR oActiveGet
   VAR xReadVar
   VAR cVarName
   VAR cReadProcName   AS CHARACTER INIT ""
   VAR nReadProcLine                INIT 0
   VAR nNextGet                     INIT 0
   VAR nHitCode        AS NUMERIC   INIT 0
   VAR nPos            AS NUMERIC   INIT 1
   VAR cMsgSaveS
   VAR nMenuID
   VAR nSaveCursor

ENDCLASS

/* --- */


METHOD ReadModal( nPos, oMenu, nMsgRow, nMsgLeft, nMsgRight, cMsgColor ) CLASS HBGetList

   LOCAL lMsgFlag
   LOCAL aMsg

   ::nSaveCursor   := SetCursor( SC_NONE )
   ::cReadProcName := ProcName( 2 )
   ::nReadProcLine := ProcLine( 2 )

   ::nPos := ::Settle( zh_defaultValue( nPos, 0 ), .T. )

   IF ( lMsgFlag := ZH_ISNUMERIC( nMsgRow ) .AND. ;
                    ZH_ISNUMERIC( nMsgLeft ) .AND. ;
                    ZH_ISNUMERIC( nMsgRight ) )

      IF ! ZH_ISSTRING( cMsgColor )
         cMsgColor := GetClrPair( SetColor(), 1 )
      ENDIF

      Scroll( nMsgRow, nMsgLeft, nMsgRow, nMsgRight )

      ::cMsgSaveS := SaveScreen( nMsgRow, nMsgLeft, nMsgRow, nMsgRight )
   ENDIF

   ::nNextGet := 0
   ::nHitCode := 0
   ::nMenuID := 0

   aMsg := { lMsgFlag, nMsgRow, nMsgLeft, nMsgRight, cMsgColor, , , , , }

   DO WHILE ::nPos != 0

      ::oGet := ::aGetList[ ::nPos ]
      ::PostActiveGet()

      IF ZH_ISEVALITEM( ::oGet:reader )
         Eval( ::oGet:reader, ::oGet, Self, oMenu, aMsg )
      ELSE
         ::Reader( oMenu, aMsg )
      ENDIF

      ::nPos := ::Settle( ::nPos )

   ENDDO

   IF lMsgFlag
      RestScreen( nMsgRow, nMsgLeft, nMsgRow, nMsgRight, ::cMsgSaveS )
   ENDIF

   SetCursor( ::nSaveCursor )

   RETURN Self

METHOD Updated() CLASS HBGetList
   RETURN ::lUpdated

METHOD Get() CLASS HBGetList
   RETURN ::oGet

METHOD SetFocus() CLASS HBGetList

   __GetListSetActive( Self )
   __GetListLast( Self )
   ::aGetList[ ::nPos ]:setFocus()

   RETURN Self

METHOD Reader( oMenu, aMsg ) CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL nRow
   LOCAL nCol
   LOCAL nOldCursor
   LOCAL nKey

   IF ::nLastExitState == GE_SHORTCUT .OR.;
      ::nLastExitState == GE_MOUSEHIT .OR.;
      ::GetPreValidate( oGet, aMsg )

      ::ShowGetMsg( oGet, aMsg )

      ::nHitCode := 0
      ::nLastExitState := 0
      oGet:setFocus()

      DO WHILE oGet:exitState == GE_NOEXIT .AND. ! ::lKillRead
         IF oGet:typeOut
            oGet:exitState := GE_ENTER
         ENDIF

//       IF oGet:buffer == NIL
//          oGet:exitState := GE_ENTER
//       ENDIF

         DO WHILE oGet:exitState == GE_NOEXIT .AND. ! ::lKillRead
            SetCursor( iif( ::nSaveCursor == SC_NONE, SC_NORMAL, ::nSaveCursor ) )
            nKey := Inkey( 0, zh_bitOr( Set( _SET_EVENTMASK ), ZH_INKEY_EXT ) )
            SetCursor( SC_NONE )
            ::GetApplyKey( nKey, oGet, oMenu, aMsg )
            nRow := Row()
            nCol := Col()
            ::ShowGetMsg( oGet, aMsg )
            SetPos( nRow, nCol )
         ENDDO

         IF ! ::nLastExitState == GE_SHORTCUT .AND. ;
            ! ::nLastExitState == GE_MOUSEHIT .AND. ;
            ! ::GetPostValidate( oGet, aMsg )
            oGet:exitState := GE_NOEXIT
         ENDIF
      ENDDO

      nRow := Row()
      nCol := Col()
      nOldCursor := SetCursor()
      oGet:killFocus()
      SetCursor( nOldCursor )
      SetPos( nRow, nCol )

      ::EraseGetMsg( aMsg )
   ENDIF

   RETURN Self

METHOD GetApplyKey( nKey, oGet, oMenu, aMsg ) CLASS HBGetList

   LOCAL cKey
   LOCAL bKeyBlock
   LOCAL nMRow
   LOCAL nMCol
   LOCAL nButton
   LOCAL nHotItem

   LOCAL nKeyStd := zh_keyStd( nKey )

   zh_default( @oGet, ::oGet )

   IF ( bKeyBlock := SetKey( nKey ) ) != NIL .OR. ;
      ( bKeyBlock := SetKey( nKeyStd ) ) != NIL
      IF ::GetDoSetKey( bKeyBlock, oGet )
         RETURN Self
      ENDIF
   ENDIF

   IF ::aGetList != NIL .AND. ( nHotItem := ::Accelerator( nKey, aMsg ) ) != 0

      oGet:exitState := GE_SHORTCUT
      ::nNextGet := nHotItem
      ::nLastExitState := GE_SHORTCUT
   ELSEIF ! ZH_ISOBJECT( oMenu )
   ELSEIF ( nHotItem := oMenu:getAccel( nKey ) ) != 0
      ::nMenuID := MenuModal( oMenu, nHotItem, aMsg[ MSGROW ], aMsg[ MSGLEFT ], aMsg[ MSGRIGHT ], aMsg[ MSGCOLOR ] )
      nKeyStd := 0
   ELSEIF IsShortcut( oMenu, nKey )
      nKeyStd := 0
   ENDIF

   SWITCH nKeyStd
   CASE K_UP
      oGet:exitState := GE_UP
      EXIT

   CASE K_SH_TAB
      oGet:exitState := GE_UP
      EXIT

   CASE K_DOWN
      oGet:exitState := GE_DOWN
      EXIT

   CASE K_TAB
      oGet:exitState := GE_DOWN
      EXIT

   CASE K_ENTER
      oGet:exitState := GE_ENTER
      EXIT

   CASE K_ESC
      IF Set( _SET_ESCAPE )
         oGet:undo()
         oGet:exitState := GE_ESCAPE
      ENDIF
      EXIT

   CASE K_PGUP
      oGet:exitState := GE_WRITE
      EXIT

   CASE K_PGDN
      oGet:exitState := GE_WRITE
      EXIT

   CASE K_CTRL_HOME
      oGet:exitState := GE_TOP
      EXIT

#ifdef CTRL_END_SPECIAL
   CASE K_CTRL_END
      oGet:exitState := GE_BOTTOM
      EXIT
#else
   CASE K_CTRL_W
      oGet:exitState := GE_WRITE
      EXIT
#endif


   CASE K_LBUTTONDOWN
   CASE K_LDBLCLK

      nMRow := MRow()
      nMCol := MCol()

      IF ! ZH_ISOBJECT( oMenu )
         nButton := 0
      ELSEIF ! oMenu:ClassName() == "TOPBARMENU"
         nButton := 0
      ELSEIF ( nButton := oMenu:hitTest( nMRow, nMCol ) ) != 0
         ::nMenuID := MenuModal( oMenu, nHotItem, aMsg[ MSGROW ], aMsg[ MSGLEFT ], aMsg[ MSGRIGHT ], aMsg[ MSGCOLOR ] )
         nButton := 1
      ENDIF

      IF nButton != 0
      ELSEIF ( nButton := oGet:hitTest( nMRow, nMCol ) ) == HTCLIENT

         DO WHILE oGet:col + oGet:pos - 1 > nMCol
            oGet:left()

            // Handle editing buffer if first character is non-editable:
            IF oGet:typeOut
               // reset typeout:
               oGet:home()
               EXIT
            ENDIF

         ENDDO

         DO WHILE oGet:col + oGet:pos - 1 < nMCol
            oGet:right()

            // Handle editing buffer if last character is non-editable:
            IF oGet:typeOut
               // reset typeout:
               oGet:end()
               EXIT
            ENDIF

         ENDDO

      ELSEIF nButton != HTNOWHERE
      ELSEIF ::aGetList != NIL .AND. ::hitTest( nMRow, nMCol, aMsg ) != 0
         oGet:exitState := GE_MOUSEHIT
         ::nLastExitState := GE_MOUSEHIT
      ELSE
         oGet:exitState := GE_NOEXIT
      ENDIF
      EXIT

   CASE K_UNDO
      oGet:undo()
      EXIT

   CASE K_HOME
      oGet:home()
      EXIT

   CASE K_END
      oGet:end()
      EXIT

   CASE K_RIGHT
      oGet:right()
      EXIT

   CASE K_LEFT
      oGet:left()
      EXIT

   CASE K_CTRL_RIGHT
      oGet:wordRight()
      EXIT

   CASE K_CTRL_LEFT
      oGet:wordLeft()
      EXIT

   CASE K_BS
      oGet:backSpace()
      EXIT

   CASE K_DEL
      oGet:delete()
      EXIT

   CASE K_CTRL_T
      oGet:delWordRight()
      EXIT

   CASE K_CTRL_Y
      oGet:delEnd()
      EXIT

   CASE K_CTRL_BS
      oGet:delWordLeft()
      EXIT

   CASE K_INS
      Set( _SET_INSERT, ! Set( _SET_INSERT ) )
      ::ShowScoreboard()
      EXIT

   OTHERWISE

      IF ! ( cKey := zh_keyChar( nKeyStd ) ) == ""
         IF oGet:type == "N" .AND. ( cKey == "." .OR. cKey == "," )
            oGet:toDecPos()
         ELSE
            IF Set( _SET_INSERT )
               oGet:insert( cKey )
            ELSE
               oGet:overStrike( cKey )
            ENDIF

            IF oGet:typeOut
               IF Set( _SET_BELL )
                  QQOut( Chr( 7 ) )
               ENDIF
               IF ! Set( _SET_CONFIRM )
                  oGet:exitState := GE_ENTER
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDSWITCH

   RETURN Self

METHOD GetPreValidate( oGet, aMsg ) CLASS HBGetList

   LOCAL lUpdated
   LOCAL lWhen := .T.

   zh_default( @oGet, ::oGet )

   IF oGet:preBlock != NIL

      lUpdated := ::lUpdated

      lWhen := Eval( oGet:preBlock, oGet, aMsg )

      IF ! ZH_ISOBJECT( oGet:control ) .AND. ! lWhen
         oGet:display()
      ENDIF

      ::ShowScoreBoard()

      ::lUpdated := lUpdated

      __GetListLast( Self )
   ENDIF

   IF ::lKillRead
      lWhen := .F.
      oGet:exitState := GE_ESCAPE
   ELSEIF ! lWhen
      oGet:exitState := GE_WHEN
   ELSE
      oGet:exitState := GE_NOEXIT
   ENDIF

   RETURN lWhen

METHOD GetPostValidate( oGet, aMsg ) CLASS HBGetList

   LOCAL lUpdated
   LOCAL lValid := .T.
   LOCAL nOldCursor

   zh_default( @oGet, ::oGet )

   IF oGet:exitState == GE_ESCAPE
      RETURN .T.
   ENDIF

   IF oGet:badDate
      oGet:home()
      ::DateMsg()
      ::ShowScoreboard()
      RETURN .F.
   ENDIF

   IF oGet:changed
      oGet:assign()
      ::lUpdated := .T.
   ENDIF


   nOldCursor := SetCursor()
   oGet:reset()
   SetCursor( nOldCursor )

   IF oGet:postBlock != NIL

      lUpdated := ::lUpdated

      IF ZH_ISSTRING( oGet:buffer )
         SetPos( oGet:row, oGet:col + zh_ULen( oGet:buffer ) )
      ENDIF
      lValid := Eval( oGet:postBlock, oGet, aMsg )
      SetPos( oGet:row, oGet:col )

      ::ShowScoreBoard()
      oGet:updateBuffer()

      ::lUpdated := iif( oGet:changed, .T., lUpdated )

      __GetListLast( Self )

      IF ::lKillRead
         oGet:exitState := GE_ESCAPE
         lValid := .T.
      ENDIF
   ENDIF

   RETURN lValid

METHOD GetDoSetKey( bKeyBlock, oGet ) CLASS HBGetList

   LOCAL lUpdated
   LOCAL lSetKey

   zh_default( @oGet, ::oGet )

   IF oGet:changed
      oGet:assign()
      ::lUpdated := .T.
   ENDIF

   lUpdated := ::lUpdated

   lSetKey := Eval( bKeyBlock, ::cReadProcName, ::nReadProcLine, ::ReadVar() )

   ::ShowScoreboard()
   oGet:updateBuffer()

   ::lUpdated := lUpdated

   __GetListLast( Self )

   IF ::lKillRead
      oGet:exitState := GE_ESCAPE
   ENDIF

   RETURN zh_defaultValue( lSetKey, .T. )

METHOD Settle( nPos, lInit ) CLASS HBGetList

   LOCAL nExitState

   zh_default( @nPos, ::nPos )

   IF nPos == 0
      nExitState := GE_DOWN
   ELSEIF nPos > 0 .AND. zh_defaultValue( lInit, .F. )  /* NOTE: Never .T. in C5.2 mode. */
      nExitState := GE_NOEXIT
   ELSE
      nExitState := ::aGetList[ nPos ]:exitState
   ENDIF

   IF nExitState == GE_ESCAPE .OR. nExitState == GE_WRITE
      RETURN 0
   ENDIF

   IF nExitState != GE_WHEN
      ::nLastPos := nPos
      ::lBumpTop := .F.
      ::lBumpBot := .F.
   ELSE
      IF ::nLastExitState != 0
         nExitState := ::nLastExitState
      ELSEIF ::nNextGet < ::nLastPos
         nExitState := GE_UP
      ELSE
         nExitState := GE_DOWN
      ENDIF

   ENDIF

   SWITCH nExitState
   CASE GE_UP
      nPos--
      EXIT

   CASE GE_DOWN
      nPos++
      EXIT

   CASE GE_TOP
      nPos := 1
      ::lBumpTop := .T.
      nExitState := GE_DOWN
      EXIT

   CASE GE_BOTTOM
      nPos := Len( ::aGetList )
      ::lBumpBot := .T.
      nExitState := GE_UP
      EXIT

   CASE GE_ENTER
      nPos++
      EXIT

   CASE GE_SHORTCUT
      RETURN ::nNextGet

   CASE GE_MOUSEHIT
      RETURN ::nNextGet

   ENDSWITCH

   IF nPos == 0
      IF ! Set( _SET_EXIT ) .AND. ! ::lBumpBot
         ::lBumpTop := .T.
         nPos       := ::nLastPos
         nExitState := GE_DOWN
      ENDIF

   ELSEIF nPos == Len( ::aGetList ) + 1
      IF ! Set( _SET_EXIT ) .AND. nExitState != GE_ENTER .AND. ! ::lBumpTop
         ::lBumpBot := .T.
         nPos       := ::nLastPos
         nExitState := GE_UP
      ELSE
         nPos := 0
      ENDIF
   ENDIF

   ::nLastExitState := nExitState

   IF nPos != 0
      ::aGetList[ nPos ]:exitState := nExitState
   ENDIF

   RETURN nPos

METHOD PostActiveGet() CLASS HBGetList

   ::GetActive( ::oGet )
   ::ReadVar( ::GetReadVar() )
   ::ShowScoreBoard()

   RETURN Self

METHOD GetReadVar() CLASS HBGetList
   RETURN zh_GetReadVar( ::oGet )

METHOD SetFormat( bFormat ) CLASS HBGetList

   LOCAL bSavFormat := ::bFormat

   IF ZH_ISEVALITEM( bFormat )
      ::bFormat := bFormat
   ENDIF

   RETURN bSavFormat

METHOD KillRead( lKill ) CLASS HBGetList

   LOCAL lSavKill := ::lKillRead

   IF PCount() > 0
      ::lKillRead := lKill
   ENDIF

   RETURN lSavKill

METHOD GetActive( oGet ) CLASS HBGetList

   LOCAL oOldGet := ::oActiveGet

   IF PCount() > 0
      ::oActiveGet := oGet
   ENDIF

   RETURN oOldGet

METHOD ShowScoreboard() CLASS HBGetList

   IF Set( _SET_SCOREBOARD )

      zh_DispOutAt( SCORE_ROW, SCORE_COL, iif( Set( _SET_INSERT ), ;
         __natMsg( _GET_INSERT_ON ), ;
         iif( zh_ULen( __natMsg( _GET_INSERT_OFF ) ) == zh_ULen( __natMsg( _GET_INSERT_ON ) ), ;
            __natMsg( _GET_INSERT_OFF ), ;
            Space( zh_ULen( __natMsg( _GET_INSERT_ON ) ) ) ) ) )

   ENDIF

   RETURN Self

METHOD DateMsg() CLASS HBGetList

   LOCAL nKey

   IF Set( _SET_SCOREBOARD )

      zh_DispOutAt( SCORE_ROW, SCORE_COL, __natMsg( _GET_INVD_DATE ) )

      DO WHILE ( nKey := Inkey( 0, zh_bitOr( Set( _SET_EVENTMASK ), ZH_INKEY_EXT ) ) ) == 0
      ENDDO
      zh_keyIns( nKey )

      zh_DispOutAt( SCORE_ROW, SCORE_COL, Space( zh_ULen( __natMsg( _GET_INVD_DATE ) ) ) )

   ENDIF

   RETURN Self

METHOD ReadVar( cNewVarName ) CLASS HBGetList

   LOCAL cOldName := ::cVarName

   IF ZH_ISSTRING( cNewVarName )
      ::cVarName := cNewVarName
   ENDIF

   RETURN cOldName

METHOD ReadUpdated( lUpdated ) CLASS HBGetList

   LOCAL lSavUpdated := ::lUpdated

   IF PCount() > 0
      ::lUpdated := lUpdated
   ENDIF

   RETURN lSavUpdated

METHOD GUIReader( oGet, oMenu, aMsg ) CLASS HBGetList

   LOCAL oGUI
   LOCAL nKey

   IF ZH_ISOBJECT( oGet:control ) .AND. ;
      ::nLastExitState == GE_SHORTCUT .OR. ;
      ::nLastExitState == GE_MOUSEHIT .OR. ;
      ::GetPreValidate( oGet, aMsg )

      ::ShowGetMsg( oGet, aMsg )

      ::nLastExitState := 0

      // Activate the GET for reading
      oGUI := oGet:control
      oGUI:Select( oGet:varGet() )
      oGUI:setFocus()

      IF oGet:exitState == GE_NOEXIT  // Added.
         DO CASE
         CASE ::nHitCode > 0
            oGUI:Select( ::nHitCode )
         CASE ::nHitCode == HTCAPTION
            oGUI:Select()
         CASE ::nHitCode == HTCLIENT
            oGUI:Select( K_LBUTTONDOWN )
         CASE ::nHitCode == HTDROPBUTTON
            oGUI:Open()
         CASE ::nHitCode >= HTSCROLLFIRST .AND. ;
              ::nHitCode <= HTSCROLLLAST
            oGUI:Scroll( ::nHitCode )
         ENDCASE
      ENDIF

      ::nHitCode := 0

      DO WHILE oGet:exitState == GE_NOEXIT .AND. ! ::lKillRead

         // Check for initial typeout (no editable positions)
         IF oGUI:typeOut
            oGet:exitState := GE_ENTER
         ENDIF

         // Apply keystrokes until exit
         DO WHILE oGet:exitState == GE_NOEXIT .AND. ! ::lKillRead
            nKey := Inkey( 0, zh_bitOr( Set( _SET_EVENTMASK ), ZH_INKEY_EXT ) )

            ::GUIApplyKey( oGet, oGUI, nKey, oMenu, aMsg )

            ::ShowGetMsg( oGet, aMsg )
         ENDDO

         IF ::nLastExitState != GE_SHORTCUT .AND. ;
            ::nLastExitState != GE_MOUSEHIT .AND. ;
            ! ::GetPostValidate( oGet, aMsg )
            oGet:exitState := GE_NOEXIT
         ENDIF
      ENDDO

      // De-activate the GET
      SWITCH oGUI:ClassName()
      CASE "LISTBOX"
      CASE "RADIOGROUP"
         IF ZH_ISNUMERIC( oGet:varGet() )
            oGet:varPut( oGUI:value )
            EXIT
         ENDIF
         /* fallthrough */
      OTHERWISE
         oGet:varPut( oGUI:buffer )
      ENDSWITCH
      oGUI:killFocus()

      ::EraseGetMsg( aMsg )

      IF oGUI:ClassName() == "LISTBOX" .AND. ;
         oGUI:dropDown .AND. ;
         oGUI:isOpen

         oGUI:Close()
      ENDIF

   ENDIF

   RETURN Self

METHOD GUIApplyKey( oGet, oGUI, nKey, oMenu, aMsg ) CLASS HBGetList

   LOCAL bKeyBlock
   LOCAL oTheClass
   LOCAL nHotItem
   LOCAL lClose
   LOCAL nMRow
   LOCAL nMCol
   LOCAL nButton
   LOCAL cKey

   LOCAL nKeyStd := zh_keyStd( nKey )

   // Check for SET KEY first
   IF ( bKeyBlock := SetKey( nKey ) ) != NIL .OR. ;
      ( bKeyBlock := SetKey( nKeyStd ) ) != NIL
      IF ::GetDoSetKey( bKeyBlock, oGet )
         RETURN Self
      ENDIF
   ENDIF

   IF ( nHotItem := ::Accelerator( nKey, aMsg ) ) != 0
      oGet:exitState := GE_SHORTCUT
      ::nNextGet := nHotItem
   ELSEIF ! ZH_ISOBJECT( oMenu )
   ELSEIF ( nHotItem := oMenu:getAccel( nKey ) ) != 0
      ::nMenuID := MenuModal( oMenu, nHotItem, aMsg[ MSGROW ], aMsg[ MSGLEFT ], aMsg[ MSGRIGHT ], aMsg[ MSGCOLOR ] )
      nKeyStd := 0
   ELSEIF IsShortcut( oMenu, nKey )
      nKeyStd := 0
   ENDIF

   IF nKeyStd == 0
   ELSEIF ( oTheClass := oGUI:ClassName() ) == "RADIOGROUP"
      IF nKeyStd == K_UP
         oGUI:PrevItem()
         nKeyStd := 0

      ELSEIF nKeyStd == K_DOWN
         oGUI:NextItem()
         nKeyStd := 0

      ELSEIF ( nHotItem := oGUI:getAccel( nKey ) ) != 0
         oGUI:Select( nHotItem )

      ENDIF

      IF ZH_ISNUMERIC( oGet:varGet() )
         oGet:varPut( oGUI:Value )
      ENDIF

   ELSEIF oTheClass == "CHECKBOX"
      IF nKeyStd == K_SPACE
         oGUI:Select()
      ENDIF

   ELSEIF oTheClass == "PUSHBUTTON"
      IF nKeyStd == K_SPACE
         oGUI:Select( K_SPACE )

      ELSEIF nKeyStd == K_ENTER
         oGUI:Select()
         nKeyStd := 0

      ENDIF

   ELSEIF oTheClass == "LISTBOX"
      IF nKeyStd == K_UP
         oGUI:PrevItem()
         nKeyStd := 0

      ELSEIF nKeyStd == K_DOWN
         oGUI:NextItem()
         nKeyStd := 0

      ELSEIF nKeyStd == K_SPACE
         IF ! oGUI:DropDown
         ELSEIF ! oGUI:IsOpen
            oGUI:Open()
            nKeyStd := 0
         ENDIF

      ELSEIF ! ( cKey := zh_keyChar( nKeyStd ) ) == "" .AND. ;
             ( nButton := oGUI:FindText( cKey, oGUI:Value + 1, .F., .F. ) ) != 0
         oGUI:Select( nButton )

      ENDIF

      IF ZH_ISNUMERIC( oGet:varGet() )
         oGet:varPut( oGUI:Value )
      ENDIF

   ENDIF

   SWITCH nKeyStd
   CASE K_UP
      oGet:exitState := GE_UP
      EXIT

   CASE K_SH_TAB
      oGet:exitState := GE_UP
      EXIT

   CASE K_DOWN
      oGet:exitState := GE_DOWN
      EXIT

   CASE K_TAB
      oGet:exitState := GE_DOWN
      EXIT

   CASE K_ENTER
      oGet:exitState := GE_ENTER
      EXIT

   CASE K_ESC
      IF Set( _SET_ESCAPE )
         oGet:exitState := GE_ESCAPE
      ENDIF
      EXIT

   CASE K_PGUP
      oGet:exitState := GE_WRITE
      EXIT

   CASE K_PGDN
      oGet:exitState := GE_WRITE
      EXIT

   CASE K_CTRL_HOME
      oGet:exitState := GE_TOP
      EXIT


#ifdef CTRL_END_SPECIAL

   // Both ^W and ^End go to the last GET
   CASE K_CTRL_END
      oGet:exitState := GE_BOTTOM
      EXIT

#else

   // Both ^W and ^End terminate the READ (the default)
   CASE K_CTRL_W
      oGet:exitState := GE_WRITE
      EXIT

#endif

   CASE K_LBUTTONDOWN
   CASE K_LDBLCLK

      nMRow := MRow()
      nMCol := MCol()

      IF ! ZH_ISOBJECT( oMenu )
         nButton := 0
      ELSEIF ! oMenu:ClassName() == "TOPBARMENU"
         nButton := 0
      ELSEIF ( nButton := oMenu:hitTest( nMRow, nMCol ) ) != 0
         ::nMenuID := MenuModal( oMenu, nHotItem, aMsg[ MSGROW ], aMsg[ MSGLEFT ], aMsg[ MSGRIGHT ], aMsg[ MSGCOLOR ] )
         nButton := 1
      ENDIF

      lClose := .T.

      IF nButton != 0
      ELSEIF ( nButton := oGUI:hitTest( nMRow, nMCol ) ) == HTNOWHERE
         IF ::HitTest( nMRow, nMCol, aMsg ) != 0
            oGet:exitState := GE_MOUSEHIT
            ::nLastExitState := GE_MOUSEHIT
         ELSE
            oGet:exitState := GE_NOEXIT
         ENDIF

      ELSEIF nButton >= HTCLIENT
         oGUI:Select( nButton )

      ELSEIF nButton == HTDROPBUTTON
         IF ! oGUI:IsOpen
            oGUI:Open()
            lClose := .F.
         ENDIF

      ELSEIF nButton >= HTSCROLLFIRST .AND. nButton <= HTSCROLLLAST
         oGUI:Scroll( nButton )
         lClose := .F.

      ENDIF

      IF ! lClose
      ELSEIF ! oTheClass == "LISTBOX"
      ELSEIF ! oGUI:DropDown
      ELSEIF oGUI:IsOpen
         oGUI:Close()
         oGUI:Display()
      ENDIF

      EXIT

   ENDSWITCH

   RETURN Self

METHOD GUIPreValidate( oGet, oGUI, aMsg ) CLASS HBGetList

   LOCAL lUpdated
   LOCAL lWhen := .T.

   zh_default( @oGet, ::oGet )

   IF oGet:preBlock != NIL

      lUpdated := ::lUpdated

      lWhen := Eval( oGet:preBlock, oGet, aMsg )

      IF ! oGUI:ClassName() == "TBROWSE"
         oGet:display()
      ENDIF

      ::ShowScoreBoard()
      ::lUpdated := lUpdated

      __GetListLast( Self )
   ENDIF

   IF ::lKillRead
      lWhen := .F.
      oGet:exitState := GE_ESCAPE
   ELSEIF ! lWhen
      oGet:exitState := GE_WHEN
   ELSE
      oGet:exitState := GE_NOEXIT
   ENDIF

   RETURN lWhen

METHOD GUIPostValidate( oGet, oGUI, aMsg ) CLASS HBGetList

   LOCAL lUpdated
   LOCAL lValid := .T.
   LOCAL xOldValue
   LOCAL xNewValue

   zh_default( @oGet, ::oGet )

   IF oGet:exitState == GE_ESCAPE
      RETURN .T.                   // NOTE
   ENDIF

   IF ! oGUI:ClassName() == "TBROWSE"
      xOldValue := oGet:varGet()
      SWITCH oGUI:ClassName()
      CASE "LISTBOX"
      CASE "RADIOGROUP"
         IF ZH_ISNUMERIC( oGet:varGet() )
            xNewValue := oGUI:value
            EXIT
         ENDIF
         /* fallthrough */
      OTHERWISE
         xNewValue := oGUI:buffer
      ENDSWITCH
   ENDIF

   IF ! xOldValue == xNewValue
      oGet:varPut( xNewValue )
      ::lUpdated := .T.
   ENDIF

   // Check VALID condition if specified
   IF oGet:postBlock != NIL

      lUpdated := ::lUpdated

      lValid := Eval( oGet:postBlock, oGet, aMsg )

      // Reset S'87 compatibility cursor position
      SetPos( oGet:row, oGet:col )

      ::ShowScoreBoard()
      IF ! oGUI:ClassName() == "TBROWSE"
         oGUI:Select( oGet:varGet() )
      ENDIF

      ::lUpdated := lUpdated

      __GetListLast( Self )

      IF ::lKillRead
         oGet:exitState := GE_ESCAPE  // Provokes ReadModal() exit
         lValid := .T.
      ENDIF

   ENDIF

   RETURN lValid

METHOD TBApplyKey( oGet, oTB, nKey, oMenu, aMsg ) CLASS HBGetList

   LOCAL bKeyBlock
   LOCAL nMRow
   LOCAL nMCol
   LOCAL nButton
   LOCAL nHotItem

   LOCAL nKeyStd := zh_keyStd( nKey )

   // Check for SET KEY first
   IF ( bKeyBlock := SetKey( nKey ) ) != NIL .OR. ;
      ( bKeyBlock := SetKey( nKeyStd ) ) != NIL
      IF ::GetDoSetKey( bKeyBlock, oGet )
         RETURN Self
      ENDIF
   ENDIF

   IF ( nHotItem := ::Accelerator( nKey, aMsg ) ) != 0
      oGet:exitState := GE_SHORTCUT
      ::nNextGet := nHotItem
   ELSEIF ! ZH_ISOBJECT( oMenu )
   ELSEIF ( nHotItem := oMenu:getAccel( nKey ) ) != 0
      ::nMenuID := MenuModal( oMenu, nHotItem, aMsg[ MSGROW ], aMsg[ MSGLEFT ], aMsg[ MSGRIGHT ], aMsg[ MSGCOLOR ] )
      nKeyStd := 0
   ELSEIF IsShortcut( oMenu, nKey )
      nKeyStd := 0
   ENDIF

   SWITCH nKeyStd
   CASE K_TAB
      oGet:exitState := GE_DOWN
      EXIT

   CASE K_SH_TAB
      oGet:exitState := GE_UP
      EXIT

   CASE K_ENTER
      IF ! oTb:Stable
         oTb:ForceStable()
      ENDIF
      oGet:exitState := GE_ENTER
      EXIT

   CASE K_ESC
      IF Set( _SET_ESCAPE )
         oGet:exitState := GE_ESCAPE
      ENDIF
      EXIT

#ifdef CTRL_END_SPECIAL

   // Both ^W and ^End go to the last GET
   CASE K_CTRL_END
      oGet:exitState := GE_BOTTOM
      EXIT

#else

   // Both ^W and ^End terminate the READ (the default)
   CASE K_CTRL_W
      oGet:exitState := GE_WRITE
      EXIT

#endif

   CASE K_LBUTTONDOWN
   CASE K_LDBLCLK

      nMRow := MRow()
      nMCol := MCol()

      IF ! ZH_ISOBJECT( oMenu )
         nButton := 0
      ELSEIF ! oMenu:ClassName() == "TOPBARMENU"
         nButton := 0
      ELSEIF ( nButton := oMenu:hitTest( nMRow, nMCol ) ) != 0
         ::nMenuID := MenuModal( oMenu, nHotItem, aMsg[ MSGROW ], aMsg[ MSGLEFT ], aMsg[ MSGRIGHT ], aMsg[ MSGCOLOR ] )
         nButton := 1
      ENDIF

      IF nButton != 0
      ELSEIF oTB:hitTest( nMRow, nMCol ) == HTNOWHERE
         IF ::hitTest( nMRow, nMCol, aMsg ) != 0
            oGet:exitState := GE_MOUSEHIT
            ::nLastExitState := GE_MOUSEHIT
         ELSE
            oGet:exitState := GE_NOEXIT
         ENDIF
      ENDIF

      EXIT

   ENDSWITCH

   RETURN Self

METHOD TBReader( oGet, oMenu, aMsg ) CLASS HBGetList

   LOCAL oTB
   LOCAL nKey
   LOCAL lAutoLite
   LOCAL nSaveCursor
   LOCAL nProcessed
// LOCAL oGUI := oGet:control

   // Read the GET if the WHEN condition is satisfied
   IF ZH_ISOBJECT( oGet:control ) .AND. ;
      ::nLastExitState == GE_SHORTCUT .OR. ;
      ::nLastExitState == GE_MOUSEHIT .OR. ;
      ::GetPreValidate( oGet, aMsg )

      ::ShowGetMsg( oGet, aMsg )
      ::nLastExitState := 0

      nSaveCursor := SetCursor( SC_NONE )

      // Activate the GET for reading
      oTB := oGet:control

      lAutoLite := oTB:Autolite
      oTB:Autolite := .T.
      oTB:Hilite()

      IF oGet:exitState == GE_NOEXIT
         IF ::nHitcode == HTCELL
            // Replaces call to TBMouse( oTB, MRow(), MCol() ):
            oTB:RowPos := oTb:mRowPos
            oTB:ColPos := oTb:mColPos
            oTB:Invalidate()
         ENDIF
      ENDIF

      ::nHitcode := 0

      DO WHILE oGet:exitState == GE_NOEXIT .AND. ! ::lKillRead

         // Apply keystrokes until exit
         DO WHILE oGet:exitState == GE_NOEXIT .AND. ! ::lKillRead
            nKey := 0

            DO WHILE ! oTB:Stabilize() .AND. nKey == 0
               nKey := Inkey(, zh_bitOr( Set( _SET_EVENTMASK ), ZH_INKEY_EXT ) )
            ENDDO

            IF nKey == 0
               nKey := Inkey( 0, zh_bitOr( Set( _SET_EVENTMASK ), ZH_INKEY_EXT ) )
            ENDIF

            nProcessed := oTB:ApplyKey( nKey )
            IF nProcessed == TBR_EXIT
               oGet:exitState := GE_ESCAPE
               EXIT

            ELSEIF nProcessed == TBR_EXCEPTION
               ::TBApplyKey( oGet, oTB, nKey, oMenu, aMsg )

               ::ShowGetMsg( oGet, aMsg )

            ENDIF

         ENDDO

         // Disallow exit if the VALID condition is not satisfied
         IF ::nLastExitState == GE_SHORTCUT
         ELSEIF ::nLastExitState == GE_MOUSEHIT
         ELSEIF ! ::GetPostValidate( oGet, aMsg )
            oGet:exitState := GE_NOEXIT
         ENDIF

      ENDDO

      // De-activate the GET
      oTB:Autolite := lAutoLite
      oTB:DeHilite()

      ::EraseGetMsg( aMsg )

      SetCursor( nSaveCursor )
   ENDIF

   RETURN Self

METHOD Accelerator( nKey, aMsg ) CLASS HBGetList

   LOCAL nGet
   LOCAL oGet
   LOCAL nHotPos
   LOCAL cKey
   LOCAL cCaption
   LOCAL nStart
   LOCAL nEnd
   LOCAL nIteration
   LOCAL lGUI

   LOCAL nKeyStd

   IF zh_bitAnd( zh_keyMod( nKey ), ZH_KF_CTRL ) != 0 .AND. ! zh_keyChar( nKey ) == ""
      cKey := zh_keyChar( nKey )
   ELSE
      nKeyStd := zh_keyStd( nKey )
      DO CASE
      CASE nKeyStd >= K_ALT_Q .AND. nKey <= K_ALT_P ; cKey := zh_BSubStr( "qwertyuiop", nKey - K_ALT_Q + 1, 1 )
      CASE nKeyStd >= K_ALT_A .AND. nKey <= K_ALT_L ; cKey := zh_BSubStr( "asdfghjkl", nKey - K_ALT_A + 1, 1 )
      CASE nKeyStd >= K_ALT_Z .AND. nKey <= K_ALT_M ; cKey := zh_BSubStr( "zxcvbnm", nKey - K_ALT_Z + 1, 1 )
      CASE nKeyStd >= K_ALT_1 .AND. nKey <= K_ALT_0 ; cKey := zh_BSubStr( "1234567890", nKey - K_ALT_1 + 1, 1 )
      OTHERWISE
         RETURN 0
      ENDCASE
   ENDIF

   nStart := ::nPos + 1
   nEnd   := Len( ::aGetList )

   FOR nIteration := 1 TO 2
      FOR nGet := nStart TO nEnd

         oGet  := ::aGetList[ nGet ]

         IF ZH_ISOBJECT( oGet:control ) .AND. ;
            ! oGet:Control:ClassName() == "TBROWSE"

            cCaption := oGet:control:caption
         ELSE
            cCaption := oGet:caption
         ENDIF

         IF ( nHotPos := zh_UAt( "&", cCaption ) ) == 0
         ELSEIF nHotPos == zh_ULen( cCaption )
         ELSEIF Lower( zh_USubStr( cCaption, nHotPos + 1, 1 ) ) == cKey

            // Test the current GUI-GET or Get PostValidation:
            lGUI := ZH_ISOBJECT( ::aGetList[ ::nPos ]:control )

            IF lGUI .AND. ! ::GUIPostValidate( ::aGetList[ ::nPos ], ::aGetList[ ::nPos ]:control, aMsg )
               RETURN 0

            ELSEIF ! lGUI .AND. ! ::GetPostValidate( ::aGetList[ ::nPos ], aMsg )
               RETURN 0

            ENDIF

            // Test the next GUI-GET or Get PreValidation:
            lGUI := ZH_ISOBJECT( oGet:control )

            IF lGUI .AND. ! ::GUIPreValidate( oGet, oGet:control, aMsg )
               // RETURN 0  // Commented out.
               RETURN nGet  // Changed.

            ELSEIF ! lGUI .AND. ! ::GetPreValidate( oGet, aMsg )
               // RETURN 0  // Commented out.
               RETURN nGet  // Changed.

            ENDIF

            RETURN nGet
         ENDIF
      NEXT

      nStart := 1
      nEnd   := ::nPos - 1
   NEXT

   RETURN 0

METHOD HitTest( nMRow, nMCol, aMsg ) CLASS HBGetList

   LOCAL oGet
   LOCAL lGUI

   ::nNextGet := 0

   FOR EACH oGet IN ::aGetList
      IF ( ::nHitCode := oGet:hitTest( nMRow, nMCol ) ) != HTNOWHERE
         ::nNextGet := oGet:__enumIndex()
         EXIT
      ENDIF
   NEXT

   // DO WHILE ::nNextGet != 0  // Commented out.

   IF ::nNextGet != 0  // Changed.

      // Test the current GUI-GET or Get PostValidation:
      lGUI := ZH_ISOBJECT( ::aGetList[ ::nPos ]:control )

      IF lGUI .AND. ! ::GUIPostValidate( ::aGetList[ ::nPos ], ::aGetList[ ::nPos ]:control, aMsg )

         ::nNextGet := 0
         // EXIT  // Commented out.
         RETURN 0  // Changed.

      ELSEIF ! lGUI .AND. ! ::GetPostValidate( ::aGetList[ ::nPos ], aMsg )

         ::nNextGet := 0
         // EXIT  // Commented out.
         RETURN 0  // Changed.

      ENDIF

      // Test the next GUI-GET or Get PreValidation:
      lGUI := ZH_ISOBJECT( ::aGetList[ ::nNextGet ]:control )

      IF lGUI .AND. ! ::GUIPreValidate( ::aGetList[ ::nNextGet ], ::aGetList[ ::nNextGet ]:control, aMsg )

         ::nNextGet := 0
         // EXIT  // Commented out.
         RETURN ::nNextGet  // Changed.

      ELSEIF ! lGUI .AND. ! ::GetPreValidate( ::aGetList[ ::nNextGet ], aMsg )

         ::nNextGet := 0
         // EXIT  // Commented out.
         RETURN ::nNextGet  // Changed.

      ENDIF

      // EXIT  // Commented out.
      RETURN ::nNextGet  // Changed.
   // ENDDO  // Commented out.

   ENDIF

   // RETURN ::nNextGet != 0  // Commented out.
   RETURN 0


#define SLUPDATED       1
#define SBFORMAT        2
#define SLKILLREAD      3
#define SLBUMPTOP       4
#define SLBUMPBOT       5
#define SNLASTEXIT      6
#define SNLASTPOS       7
#define SOACTIVEGET     8
#define SXREADVAR       9
#define SCREADPROCNAME  10
#define SNREADPROCLINE  11
#define SNNEXTGET       12
#define SNHITCODE       13
#define SNPOS           14
#define SCSCRSVMSG      15
#define SNMENUID        16
#define SNSVCURSOR      17

METHOD ReadStats( nElement, xNewValue ) CLASS HBGetList

   LOCAL xRetVal

   SWITCH nElement
   CASE SLUPDATED      ; xRetVal := ::lUpdated       ; EXIT
   CASE SBFORMAT       ; xRetVal := ::bFormat        ; EXIT
   CASE SLKILLREAD     ; xRetVal := ::lKillRead      ; EXIT
   CASE SLBUMPTOP      ; xRetVal := ::lBumpTop       ; EXIT
   CASE SLBUMPBOT      ; xRetVal := ::lBumpBot       ; EXIT
   CASE SNLASTEXIT     ; xRetVal := ::nLastExitState ; EXIT
   CASE SNLASTPOS      ; xRetVal := ::nLastPos       ; EXIT
   CASE SOACTIVEGET    ; xRetVal := ::oActiveGet     ; EXIT
   CASE SXREADVAR      ; xRetVal := ::cVarName       ; EXIT
   CASE SCREADPROCNAME ; xRetVal := ::cReadProcName  ; EXIT
   CASE SNREADPROCLINE ; xRetVal := ::nReadProcLine  ; EXIT
   CASE SNNEXTGET      ; xRetVal := ::nNextGet       ; EXIT
   CASE SNHITCODE      ; xRetVal := ::nHitCode       ; EXIT
   CASE SNPOS          ; xRetVal := ::nPos           ; EXIT
   CASE SCSCRSVMSG     ; xRetVal := ::cMsgSaveS      ; EXIT
   CASE SNMENUID       ; xRetVal := ::nMenuID        ; EXIT
   CASE SNSVCURSOR     ; xRetVal := ::nSaveCursor    ; EXIT
   OTHERWISE           ; xRetVal := NIL
   ENDSWITCH

   IF PCount() > 1

      SWITCH nElement
      CASE SLUPDATED      ; ::lUpdated       := xNewValue ; EXIT
      CASE SBFORMAT       ; ::bFormat        := xNewValue ; EXIT
      CASE SLKILLREAD     ; ::lKillRead      := xNewValue ; EXIT
      CASE SLBUMPTOP      ; ::lBumpTop       := xNewValue ; EXIT
      CASE SLBUMPBOT      ; ::lBumpBot       := xNewValue ; EXIT
      CASE SNLASTEXIT     ; ::nLastExitState := xNewValue ; EXIT
      CASE SNLASTPOS      ; ::nLastPos       := xNewValue ; EXIT
      CASE SOACTIVEGET    ; ::oActiveGet     := xNewValue ; EXIT
      CASE SXREADVAR      ; ::xReadVar       := xNewValue ; EXIT
      CASE SCREADPROCNAME ; ::cReadProcName  := xNewValue ; EXIT
      CASE SNREADPROCLINE ; ::nReadProcLine  := xNewValue ; EXIT
      CASE SNNEXTGET      ; ::nNextGet       := xNewValue ; EXIT
      CASE SNHITCODE      ; ::nHitCode       := xNewValue ; EXIT
      CASE SNPOS          ; ::nPos           := xNewValue ; EXIT
      CASE SCSCRSVMSG     ; ::cMsgSaveS      := xNewValue ; EXIT
      CASE SNMENUID       ; ::nMenuID        := xNewValue ; EXIT
      CASE SNSVCURSOR     ; ::nSaveCursor    := xNewValue ; EXIT
      ENDSWITCH
   ENDIF

   RETURN xRetVal

METHOD ShowGetMsg( oGet, aMsg ) CLASS HBGetList

   LOCAL cMsg
   LOCAL lMOldState

   IF ! Empty( aMsg ) .AND. aMsg[ MSGFLAG ]

      zh_default( @oGet, ::oGet )

      cMsg := iif( ZH_ISOBJECT( oGet:control ), oGet:control:message, oGet:message )

      IF ! Empty( cMsg )
         lMOldState := MSetCursor( .F. )
         zh_DispOutAt( aMsg[ MSGROW ], aMsg[ MSGLEFT ], zh_UPadC( cMsg, aMsg[ MSGRIGHT ] - aMsg[ MSGLEFT ] + 1 ), aMsg[ MSGCOLOR ] )
         MSetCursor( lMOldState )
      ENDIF
   ENDIF


   RETURN Self

METHOD EraseGetMsg( aMsg ) CLASS HBGetList

   LOCAL nRow := Row()
   LOCAL nCol := Col()
   LOCAL lMOldState

   IF ! Empty( aMsg ) .AND. aMsg[ MSGFLAG ]
      lMOldState := MSetCursor( .F. )
      RestScreen( aMsg[ MSGROW ], aMsg[ MSGLEFT ], aMsg[ MSGROW ], aMsg[ MSGRIGHT ], ::cMsgSaveS )
      MSetCursor( lMOldState )
   ENDIF

   SetPos( nRow, nCol )


   RETURN Self

/* --- */

METHOD New( GetList ) CLASS HBGetList

   ::aGetList := GetList

   IF ZH_ISARRAY( GetList ) .AND. Len( GetList ) >= 1
      ::oGet := GetList[ 1 ]
   ENDIF

   RETURN Self
