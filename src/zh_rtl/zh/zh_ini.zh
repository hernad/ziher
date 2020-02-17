/*
 * Handling .ini files
 *
 * Copyright 2002 Giancarlo Niccolai <gian@niccolai.ws>
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

/*
 * This small procedure reads a .ini file in the standard .ini format into
 * an hash array:
 *    ; A line starting with a ';' is a comment
 *    # Also, a '#' marks a comment up to the end of the line
 *    [NewSection]
 *    Variable=Value
 *    OtherVariable: Value
 *
 * You can pass a list of "potential" .ini files in a ';' separated path;
 * the first readable file will be loaded.
 *
 * On error, the function returns NIL. On success, you will have an hash
 * array of this form:
 *
 *    { "MAIN" => { "Key1" => "Val1", ... , "KeyN" => "ValN" },
 *      "Section1" => { "Key1" => "Val1", ... , "KeyN" => "ValN" },
 *      ...
 *      "SectionN" => { "Key1" => "Val1", ... , "KeyN" => "ValN" }
 *    }
 *
 * 'MAIN' is the default section (variables that are declared without a section).
 *
 */

#include "fileio.ch"

STATIC s_cLineComment := ";"
STATIC s_cHalfLineComment := "#"

PROCEDURE zh_iniSetComment( cLc, cHlc )

   IF ZH_ISSTRING( cLc )
      s_cLineComment := cLc
   ENDIF

   IF ZH_ISSTRING( cHlc )
      s_cHalfLineComment := cHlc
   ENDIF

   RETURN

FUNCTION zh_iniNew( lAutoMain )

   LOCAL hIni := { => }

   IF zh_defaultValue( lAutoMain, .T. )
      hIni[ "MAIN" ] := { => }
   ENDIF

   RETURN hIni

FUNCTION zh_iniRead( cFileSpec, lKeyCaseSens, cSplitters, lAutoMain )
   RETURN zh_iniReadStr( iif( ZH_ISSTRING( cFileSpec ), zh_iniFileLow( cFileSpec ), "" ), lKeyCaseSens, cSplitters, lAutoMain )

FUNCTION zh_iniReadStr( cData, lKeyCaseSens, cSplitters, lAutoMain )

   LOCAL hIni := { => }

   /* Default case sensitiveness for keys */
   zh_default( @lKeyCaseSens, .T. )
   zh_default( @lAutoMain, .T. )

   zh_HCaseMatch( hIni, lKeyCaseSens )

   IF lAutoMain
      hIni[ "MAIN" ] := { => }
   ENDIF

   RETURN zh_iniStringLow( hIni, ;
      zh_defaultValue( cData, "" ), ;
      lKeyCaseSens, ;
      zh_defaultValue( cSplitters, "=" ), ;
      lAutoMain )

STATIC FUNCTION zh_iniFileLow( cFileSpec )

   LOCAL cFile, nLen
   LOCAL hFile
   LOCAL cData
   LOCAL aFiles := zh_ATokens( cFileSpec, zh_osPathListSeparator() )

   IF Empty( aFiles )
      aFiles := { cFileSpec }
   ENDIF

   hFile := NIL
   FOR EACH cFile IN aFiles
      IF ! cFile == "" .AND. zh_vfExists( cFile )
         IF ( hFile := zh_vfOpen( cFile, FO_READ ) ) != NIL
            EXIT
         ENDIF
      ENDIF
   NEXT

   IF hFile == NIL
      RETURN ""
   ENDIF

   /* we'll read the whole file, then we'll break it in lines. */
   cData := Space( zh_vfSize( hFile ) )
   zh_vfSeek( hFile, 0, FS_SET )
   nLen := zh_vfRead( hFile, @cData, zh_BLen( cData ) )
   cData := zh_BLeft( cData, nLen )
   zh_vfClose( hFile )

   RETURN cData

STATIC FUNCTION zh_iniStringLow( hIni, cData, lKeyCaseSens, cSplitters, lAutoMain )

   LOCAL aKeyVal, hCurrentSection
   LOCAL cLine
   LOCAL reComment, reInclude, reSection, reSplitters

   reComment := zh_regexComp( s_cHalfLineComment + "|^[ \t]*" + s_cLineComment )
   reInclude := zh_regexComp( "include (.*)" )
   reSection := zh_regexComp( "[[](.*)[]]" )
   reSplitters := zh_regexComp( cSplitters )

   /* Always begin with the 'MAIN' section */
   hCurrentSection := iif( lAutoMain, hIni[ "MAIN" ], hIni )

   cLine := ""
   FOR EACH cData IN zh_ATokens( cData, .T. )
      cLine += AllTrim( cData )

      /* Sum up lines terminating with "<space>||" ...*/
      IF Right( cLine, 3 ) == " ||"
         cLine := zh_StrShrink( cLine, 2 )
         /* ... but proceed if stream over */
         IF ! cData:__enumIsLast()
            LOOP
         ENDIF
      ENDIF

      /* Skip void lines */
      IF Empty( cLine )
         LOOP
      ENDIF

      /* remove eventual comments */
      IF ! Empty( aKeyVal := zh_regexSplit( reComment, cLine ) )
         IF Empty( cLine := AllTrim( aKeyVal[ 1 ] ) )
            /* Skip all comment lines */
            LOOP
         ENDIF
      ENDIF

      /* Is it an "INCLUDE" statement ? */
      IF ! Empty( aKeyVal := zh_regex( reInclude, cLine ) )
         /* ignore void includes */
         aKeyVal[ 2 ] := AllTrim( aKeyVal[ 2 ] )
         IF aKeyVal[ 2 ] == ""
            LOOP
         ENDIF
         zh_iniStringLow( hIni, zh_iniFileLow( aKeyVal[ 2 ] ), lKeyCaseSens, cSplitters, lAutoMain )
      /* Is it a NEW section? */
      ELSEIF ! Empty( aKeyVal := zh_regex( reSection, cLine ) )
         cLine := AllTrim( aKeyVal[ 2 ] )
         IF ! cLine == ""
            hCurrentSection := { => }
            IF ! lKeyCaseSens
               cLine := Upper( cLine )
            ENDIF
            hIni[ cLine ] := hCurrentSection
         ENDIF
      /* Is it a valid key */
      ELSEIF Len( aKeyVal := zh_regexSplit( reSplitters, cLine,,, 1 ) ) == 1
         /* TODO: Signal error */
      ELSE
         /* If not case sensitive, use upper keys */
         IF ! lKeyCaseSens
            aKeyVal[ 1 ] := Upper( aKeyVal[ 1 ] )
         ENDIF
         hCurrentSection[ AllTrim( aKeyVal[ 1 ] ) ] := AllTrim( aKeyVal[ 2 ] )
      ENDIF

      cLine := ""
   NEXT

   RETURN hIni

FUNCTION zh_iniWrite( xFileName, hIni, cCommentBegin, cCommentEnd, lAutoMain )

   LOCAL hFile
   LOCAL lClose
   LOCAL cBuffer

   cBuffer := zh_iniWriteStr( hIni, cCommentBegin, cCommentEnd, lAutoMain )

   IF ! ZH_ISSTRING( cBuffer )
      RETURN .F.
   ENDIF

   DO CASE
   CASE ZH_ISSTRING( xFileName )
      IF ( hFile := zh_vfOpen( xFileName, FO_CREAT + FO_TRUNC + FO_WRITE + FO_EXCLUSIVE ) ) == NIL
         RETURN .F.
      ENDIF
      lClose := .T.
   CASE ZH_ISPOINTER( xFileName )
      hFile := xFileName
      lClose := .F.
   OTHERWISE
      RETURN .F.
   ENDCASE

   IF zh_vfWrite( hFile, cBuffer ) != zh_BLen( cBuffer )
      IF lClose
         zh_vfClose( hFile )
      ENDIF
      RETURN .F.
   ENDIF

   IF lClose
      zh_vfClose( hFile )
   ENDIF

   RETURN .T.

FUNCTION zh_iniWriteStr( hIni, cCommentBegin, cCommentEnd, lAutoMain )

   LOCAL cEOL := Set( _SET_EOL )
   LOCAL cSection
   LOCAL cBuffer := ""

   IF ! ZH_ISHASH( hIni )
      RETURN NIL
   ENDIF

   IF ZH_ISSTRING( cCommentBegin ) .AND. ! Empty( cCommentBegin )
      cBuffer += cCommentBegin + cEOL
   ENDIF

   zh_default( @lAutoMain, .T. )

   // Fix if lAutoMain is .T. but I have no 'MAIN' section
   IF lAutoMain .AND. ! "MAIN" $ hIni
      lAutoMain := .F.
   ENDIF

   /* Write top-level section */
   IF lAutoMain
      /* When lAutoMain is on, write the 'main' section */
      zh_HEval( hIni[ "MAIN" ], {| cKey, xVal | ;
         cBuffer += zh_CStr( cKey ) + "=" + zh_CStr( xVal ) + cEOL } )
   ELSE
      /* When lAutoMain is off, just write all the top-level variables. */
      zh_HEval( hIni, {| cKey, xVal | iif( ZH_ISHASH( xVal ), /* nothing */, ;
         cBuffer += zh_CStr( cKey ) + "=" + zh_CStr( xVal ) + cEOL ) } )
   ENDIF

   FOR EACH cSection IN hIni

      /* Avoid re-processing 'MAIN' section */
      IF lAutoMain
         /* When lAutoMain is on, skip section named 'MAIN' */
         IF cSection:__enumKey == "MAIN"
            LOOP
         ENDIF
      ELSE
         /* When lAutoMain is off, skip all the top-level variables. */
         IF ! ZH_ISHASH( cSection )
            LOOP
         ENDIF
      ENDIF

      cBuffer += cEOL + "[" + zh_CStr( cSection:__enumKey ) + "]" + cEOL

      zh_HEval( cSection, ;
         {| cKey, xVal | cBuffer += zh_CStr( cKey ) + "=" + ;
         zh_CStr( xVal ) + cEOL } )
   NEXT

   IF ZH_ISSTRING( cCommentEnd ) .AND. ! Empty( cCommentEnd )
      cBuffer += cCommentEnd + cEOL
   ENDIF

   RETURN iif( Empty( cBuffer ), NIL, cBuffer )
