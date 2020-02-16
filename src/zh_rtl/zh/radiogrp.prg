/*
 * RadioGroup class
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#pragma -gc0

#include "hbclass.ch"

#include "box.ch"
#include "button.ch"
#include "color.ch"
#include "setcurs.ch"

CREATE CLASS RadioGroup FUNCTION HBRadioGroup

   EXPORTED:

   VAR cargo

   METHOD addItem( oRadioButton )
   METHOD delItem( nPos )
   METHOD display()
   METHOD getAccel( xKey )
   METHOD getItem( nPos )
   METHOD hitTest( nMRow, nMCol )
   METHOD insItem( nPos, oRadioButton )
   METHOD killFocus()
   METHOD nextItem()
   METHOD prevItem()
   METHOD select( xValue )
   METHOD setColor( cColorSpec )
   METHOD setFocus()
   METHOD setStyle( cStyle )

   METHOD bottom( nBottom ) SETGET
   METHOD buffer() SETGET
   METHOD capCol( nCapCol ) SETGET
   METHOD capRow( nCapRow ) SETGET
   METHOD caption( cCaption ) SETGET
   METHOD coldBox( cColdBox ) SETGET
   METHOD colorSpec( cColorSpec ) SETGET
   METHOD fBlock( bFBlock ) SETGET
   METHOD hasFocus() SETGET
   METHOD hotBox( cHotBox ) SETGET
   METHOD itemCount() SETGET
   METHOD left( nLeft ) SETGET
   METHOD message( cMessage ) SETGET
   METHOD right( nRight ) SETGET
   METHOD textValue() SETGET                   /* NOTE: Undocumented CA-Cl*pper var. */
   METHOD top( nTop ) SETGET
   METHOD typeOut() SETGET
   METHOD value() SETGET                       /* NOTE: Undocumented CA-Cl*pper var. */

   METHOD New( nTop, nLeft, nBottom, nRight )  /* NOTE: This method is a Ziher extension [vszakats] */

   PROTECTED:

   VAR nBottom
   VAR xBuffer
   VAR nCapCol
   VAR nCapRow
   VAR cCaption   INIT ""
   VAR cColdBox   INIT ZH_B_SINGLE_UNI
   VAR cColorSpec
   VAR bFBlock
   VAR lHasFocus  INIT .F.
   VAR cHotBox    INIT ZH_B_DOUBLE_UNI
   VAR nItemCount INIT 0
   VAR nLeft
   VAR cMessage   INIT ""
   VAR nRight
   VAR cTextValue INIT ""
   VAR nTop
   VAR nValue     INIT 0

   VAR aItems     INIT {}
   VAR nCursor    INIT 0

   METHOD changeButton( nUnselect, nSelect )

ENDCLASS

METHOD addItem( oRadioButton ) CLASS RadioGroup

   IF ZH_ISOBJECT( oRadioButton ) .AND. oRadioButton:ClassName() == "RADIOBUTTN"
      AAdd( ::aItems, oRadioButton )
      ::nItemCount++
   ENDIF

   RETURN Self

METHOD delItem( nPos ) CLASS RadioGroup

   IF nPos >= 1 .AND. nPos <= ::nItemCount
      zh_ADel( ::aItems, nPos, .T. )
      ::nItemCount--
   ENDIF

   IF ::lHasFocus .AND. ::nItemCount < ::nValue
      ::nValue := ::nItemCount
      ::cTextValue := ::aItems[ ::nValue ]:data
      ::xBuffer := iif( ZH_ISNUMERIC( ::xBuffer ), ::nValue, ::cTextValue )
   ENDIF

   RETURN Self

METHOD display() CLASS RadioGroup

   LOCAL cSelBox
   LOCAL cUnSelBox
   LOCAL cCaption
   LOCAL nPos

   DispBegin()

   IF ::lHasFocus
      cSelBox := ::cHotBox
      cUnSelBox := ::cColdbox
   ELSE
      cSelBox := ::cColdbox
      cUnSelBox := ::cHotBox
   ENDIF

   DO CASE
   CASE ! Empty( cSelBox )
      zh_DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, cSelBox, zh_ColorIndex( ::cColorSpec, 0 ) )
   CASE ! Empty( cUnSelBox )
      zh_DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, cUnSelBox, zh_ColorIndex( ::cColorSpec, 0 ) )
   ENDCASE

   IF ! Empty( cCaption := ::cCaption )

      IF ( nPos := zh_UAt( "&", cCaption ) ) > 0
         IF nPos == zh_ULen( cCaption )
            nPos := 0
         ELSE
            cCaption := zh_UStuff( cCaption, nPos, 1, "" )
         ENDIF
      ENDIF

      zh_DispOutAt( ::nCapRow, ::nCapCol, cCaption, zh_ColorIndex( ::cColorSpec, 1 ) )

      IF nPos > 0
         zh_DispOutAt( ::nCapRow, ::nCapCol + nPos - 1, zh_USubStr( cCaption, nPos, 1 ), zh_ColorIndex( ::cColorSpec, 2 ) )
      ENDIF
   ENDIF

   AEval( ::aItems, {| o | o:display() } )

   DispEnd()

   RETURN Self

METHOD getAccel( xKey ) CLASS RadioGroup

   LOCAL cKey

   DO CASE
   CASE ZH_ISSTRING( xKey )
      cKey := xKey
   CASE ZH_ISNUMERIC( xKey )
      cKey := zh_keyChar( xKey )
   OTHERWISE
      RETURN 0
   ENDCASE

   IF ! cKey == ""
      cKey := Lower( cKey )
      RETURN AScan( ::aItems, {| o | o:isAccel( cKey ) } )
   ENDIF

   RETURN 0

METHOD getItem( nPos ) CLASS RadioGroup
   RETURN iif( nPos >= 1 .AND. nPos <= ::nItemCount, ::aItems[ nPos ], NIL )

METHOD hitTest( nMRow, nMCol ) CLASS RadioGroup

   LOCAL nLen
   LOCAL nPos
   LOCAL item

   DO CASE
   CASE Empty( ::cColdbox + ::cHotBox )
   CASE nMRow == ::nTop
      DO CASE
      CASE nMCol == ::nLeft
         RETURN HTTOPLEFT
      CASE nMCol == ::nRight
         RETURN HTTOPRIGHT
      CASE nMCol >= ::nLeft .AND. nMCol <= ::nRight
         RETURN HTTOP
      ENDCASE
   CASE nMRow == ::nBottom
      DO CASE
      CASE nMCol == ::nLeft
         RETURN HTBOTTOMLEFT
      CASE nMCol == ::nRight
         RETURN HTBOTTOM
      CASE nMCol >= ::nLeft .AND. nMCol <= ::nRight
         RETURN HTBOTTOMRIGHT
      ENDCASE
   CASE nMCol == ::nLeft
      IF nMRow >= ::nTop .AND. ;
         nMRow <= ::nBottom
         RETURN HTLEFT
      ELSE
         RETURN HTNOWHERE
      ENDIF
   CASE nMCol == ::nRight
      IF nMRow >= ::nTop .AND. ;
         nMRow <= ::nBottom
         RETURN HTRIGHT
      ELSE
         RETURN HTNOWHERE
      ENDIF
   ENDCASE

   nLen := zh_ULen( ::cCaption )

   IF ( nPos := zh_UAt( "&", ::cCaption ) ) > 0 .AND. nPos < nLen
      nLen--
   ENDIF

   DO CASE
   CASE Empty( ::cCaption )
   CASE nMRow != ::nCapRow
   CASE nMCol < ::nCapCol
   CASE nMCol < ::nCapCol + nLen
      RETURN HTCAPTION
   ENDCASE

   DO CASE
   CASE nMRow < ::nTop
   CASE nMRow > ::nBottom
   CASE nMCol < ::nLeft
   CASE nMCol <= ::nRight
      FOR EACH item IN ::aItems
         IF item:hitTest( nMRow, nMCol ) != HTNOWHERE
            RETURN item:__enumIndex()
         ENDIF
      NEXT
      RETURN HTCLIENT
   ENDCASE

   RETURN HTNOWHERE

METHOD insItem( nPos, oRadioButton ) CLASS RadioGroup

   IF ZH_ISOBJECT( oRadioButton ) .AND. oRadioButton:ClassName() == "RADIOBUTTN" .AND. ;
      nPos < ::nItemCount

      zh_AIns( ::aItems, nPos, oRadioButton, .T. )
      ::nItemCount++
   ENDIF

   RETURN ::aItems[ nPos ]

METHOD killFocus() CLASS RadioGroup

   LOCAL item

   LOCAL nOldMCur

   IF ::lHasFocus

      ::lHasFocus := .F.

      IF ZH_ISEVALITEM( ::bFBlock )
         Eval( ::bFBlock )
      ENDIF

      nOldMCur := MSetCursor( .F. )

      DispBegin()

      FOR EACH item IN ::aItems
         item:killFocus()
      NEXT

      ::display()

      DispEnd()

      MSetCursor( nOldMCur )
      SetCursor( ::nCursor )
   ENDIF

   RETURN Self

METHOD setFocus() CLASS RadioGroup

   LOCAL item

   LOCAL nOldMCur

   IF ! ::lHasFocus

      ::nCursor := SetCursor( SC_NONE )
      ::lHasFocus := .T.

      nOldMCur := MSetCursor( .F. )

      DispBegin()

      FOR EACH item IN ::aItems
         item:setFocus()
      NEXT

      ::display()

      DispEnd()

      MSetCursor( nOldMCur )

      IF ZH_ISEVALITEM( ::bFBlock )
         Eval( ::bFBlock )
      ENDIF
   ENDIF

   RETURN Self

METHOD nextItem() CLASS RadioGroup

   LOCAL nValue

   IF ::lHasFocus .AND. ::nItemCount > 0
      ::changeButton( nValue := ::nValue, iif( nValue == ::nItemCount, 1, nValue + 1 ) )
   ENDIF

   RETURN Self

METHOD prevItem() CLASS RadioGroup

   LOCAL nValue
   LOCAL nPos

   IF ::lHasFocus .AND. ::nItemCount > 0

      nValue := ::nValue

      DO CASE
      CASE nValue == 0 ; nPos := 1
      CASE nValue == 1 ; nPos := ::nItemCount
      OTHERWISE        ; nPos := nValue - 1
      ENDCASE

      ::changeButton( nValue, nPos )
   ENDIF

   RETURN Self

METHOD select( xValue ) CLASS RadioGroup

   LOCAL nPos
   LOCAL nLen

   SWITCH ValType( xValue )
   CASE "C"

      nLen := ::nItemCount
      FOR nPos := 1 TO nLen
         IF ::aItems[ nPos ]:data == xValue

            IF ::xBuffer == NIL
               ::xBuffer := ""
            ENDIF

            ::changeButton( ::nValue, nPos )

            EXIT
         ENDIF
      NEXT

      IF nPos > nLen
         ::xBuffer := xValue
      ENDIF
      EXIT

   CASE "N"

      IF xValue >= 1 .AND. xValue <= ::nItemCount
         IF ::xBuffer == NIL
            ::xBuffer := 0
         ENDIF

         ::changeButton( ::nValue, xValue )
      ENDIF
      EXIT

   ENDSWITCH

   RETURN Self

METHOD setColor( cColorSpec ) CLASS RadioGroup

   LOCAL item

   FOR EACH item IN ::aItems
      item:colorSpec := cColorSpec
   NEXT

   RETURN Self

METHOD setStyle( cStyle ) CLASS RadioGroup

   LOCAL item

   FOR EACH item IN ::aItems
      item:style := cStyle
   NEXT

   RETURN Self

METHOD changeButton( nUnselect, nSelect ) CLASS RadioGroup

   LOCAL nOldMCur := MSetCursor( .F. )

   IF nUnselect != nSelect

      DispBegin()

      IF nUnselect > 0
         ::aItems[ nUnselect ]:select( .F. )
         ::aItems[ nUnselect ]:display()
      ENDIF
      IF nSelect > 0
         ::aItems[ nSelect ]:select( .T. )
         ::aItems[ nSelect ]:display()
      ENDIF

      DispEnd()

      ::nValue := nSelect
      ::cTextValue := ::aItems[ nSelect ]:data
      ::xBuffer := iif( ZH_ISNUMERIC( ::xBuffer ), nSelect, ::cTextValue )
   ENDIF

   MSetCursor( nOldMCur )

   RETURN Self

METHOD bottom( nBottom ) CLASS RadioGroup

   IF nBottom != NIL
      ::nBottom := checkVariableTypeAndValidBlock( Self, "BOTTOM", nBottom, "N", 1001 )
   ENDIF

   RETURN ::nBottom

METHOD buffer() CLASS RadioGroup
   RETURN ::xBuffer

METHOD capCol( nCapCol ) CLASS RadioGroup

   IF nCapCol != NIL
      ::nCapCol := checkVariableTypeAndValidBlock( Self, "CAPCOL", nCapCol, "N", 1001 )
   ENDIF

   RETURN ::nCapCol

METHOD capRow( nCapRow ) CLASS RadioGroup

   IF nCapRow != NIL
      ::nCapRow := checkVariableTypeAndValidBlock( Self, "CAPROW", nCapRow, "N", 1001 )
   ENDIF

   RETURN ::nCapRow

METHOD caption( cCaption ) CLASS RadioGroup

   IF cCaption != NIL
      ::cCaption := checkVariableTypeAndValidBlock( Self, "CAPTION", cCaption, "C", 1001 )
   ENDIF

   RETURN ::cCaption

METHOD coldBox( cColdBox ) CLASS RadioGroup

   IF cColdBox != NIL
      ::cColdBox := checkVariableTypeAndValidBlock( Self, "COLDBOX", cColdBox, "C", 1001, {|| cColdBox == "" .OR. zh_ULen( cColdBox ) == 8 } )
   ENDIF

   RETURN ::cColdBox

METHOD colorSpec( cColorSpec ) CLASS RadioGroup

   IF cColorSpec != NIL
      ::cColorSpec := checkVariableTypeAndValidBlock( Self, "COLORSPEC", cColorSpec, "C", 1001, ;
         {|| ! Empty( zh_ColorIndex( cColorSpec, 2 ) ) .AND. Empty( zh_ColorIndex( cColorSpec, 3 ) ) } )
   ENDIF

   RETURN ::cColorSpec

METHOD fBlock( bFBlock ) CLASS RadioGroup

   IF PCount() > 0
      ::bFBlock := iif( bFBlock == NIL, NIL, checkVariableTypeAndValidBlock( Self, "FBLOCK", bFBlock, "B", 1001 ) )
   ENDIF

   RETURN ::bFBlock

METHOD hasFocus() CLASS RadioGroup
   RETURN ::lHasFocus

METHOD hotBox( cHotBox ) CLASS RadioGroup

   IF cHotBox != NIL
      ::cHotBox := checkVariableTypeAndValidBlock( Self, "HOTBOX", cHotBox, "C", 1001, {|| cHotBox == "" .OR. zh_ULen( cHotBox ) == 8 } )
   ENDIF

   RETURN ::cHotBox

METHOD itemCount() CLASS RadioGroup
   RETURN ::nItemCount

METHOD left( nLeft ) CLASS RadioGroup

   IF nLeft != NIL
      ::nLeft := checkVariableTypeAndValidBlock( Self, "LEFT", nLeft, "N", 1001 )
   ENDIF

   RETURN ::nLeft

METHOD message( cMessage ) CLASS RadioGroup

   IF cMessage != NIL
      ::cMessage := checkVariableTypeAndValidBlock( Self, "MESSAGE", cMessage, "C", 1001 )
   ENDIF

   RETURN ::cMessage

METHOD right( nRight ) CLASS RadioGroup

   IF nRight != NIL
      ::nRight := checkVariableTypeAndValidBlock( Self, "RIGHT", nRight, "N", 1001 )
   ENDIF

   RETURN ::nRight

METHOD textValue() CLASS RadioGroup
   RETURN ::cTextValue

METHOD top( nTop ) CLASS RadioGroup

   IF nTop != NIL
      ::nTop := checkVariableTypeAndValidBlock( Self, "TOP", nTop, "N", 1001 )
   ENDIF

   RETURN ::nTop

METHOD typeOut() CLASS RadioGroup
   RETURN ::nItemCount == 0 .OR. ::nValue > ::nItemCount

METHOD value() CLASS RadioGroup
   RETURN ::nValue

METHOD New( nTop, nLeft, nBottom, nRight ) CLASS RadioGroup

   LOCAL cColor

   IF ! ZH_ISNUMERIC( nTop ) .OR. ;
      ! ZH_ISNUMERIC( nLeft ) .OR. ;
      ! ZH_ISNUMERIC( nBottom ) .OR. ;
      ! ZH_ISNUMERIC( nRight )
      RETURN NIL
   ENDIF

   ::nTop    := nTop
   ::nLeft   := nLeft
   ::nBottom := nBottom
   ::nRight  := nRight
   ::nCapCol := nLeft + 2
   ::nCapRow := nTop

   IF IsDefColor()
      ::cColorSpec := "W/N,W/N,W+/N"
   ELSE
      cColor := SetColor()
      ::cColorSpec := ;
         zh_ColorIndex( cColor, CLR_BORDER ) + "," + ;
         zh_ColorIndex( cColor, CLR_STANDARD ) + "," + ;
         zh_ColorIndex( cColor, CLR_BACKGROUND )
   ENDIF

   RETURN Self

FUNCTION RadioGroup( nTop, nLeft, nBottom, nRight )
   RETURN HBRadioGroup():New( nTop, nLeft, nBottom, nRight )

FUNCTION _RadioGrp_( nTop, nLeft, nBottom, nRight, xValue, aItems, cCaption, cMessage, cColorSpec, bFBlock )

   LOCAL o

   IF ( o := RadioGroup( nTop, nLeft, nBottom, nRight ) ) != NIL

      o:caption := cCaption
      o:message := cMessage
      o:colorSpec := cColorSpec
      o:fBlock := bFBlock

      AEval( aItems, {| aItem | o:AddItem( aItem ) } )

      o:select( xValue )
   ENDIF

   RETURN o
