/*
 * __TypeFile() function
 *
 * Copyright 2000 Luiz Rafael Culik <Culik@sl.conex.net>
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
#include "fileio.ch"

#define BUFFER_LENGTH  8192

PROCEDURE __TypeFile( cFile, lPrint )

   LOCAL hFile
   LOCAL cBuffer
   LOCAL nRead
   LOCAL nHasRead
   LOCAL nSize
   LOCAL nBuffer
   LOCAL oErr
   LOCAL nRetries
   LOCAL aSaveSet[ 2 ]
   LOCAL cDir, cName, cExt
   LOCAL cTmp
   LOCAL i

   zh_default( @lPrint, .F. )

   IF ! ZH_ISSTRING( cFile )
      oErr := ErrorNew()
      oErr:severity    := ES_ERROR
      oErr:genCode     := EG_ARG
      oErr:subSystem   := "BASE"
      oErr:SubCode     := 2009
      oErr:Description := ProcName()
      Eval( ErrorBlock(), oErr )
   ENDIF

   /* If no drive/dir specified, search the SET DEFAULT and PATH directories */

   zh_FNameSplit( cFile, @cDir, @cName, @cExt )

   IF cDir == ""

      cTmp := StrTran( Set( _SET_DEFAULT ) + ";" + Set( _SET_PATH ), ",", ";" )

      i := zh_ULen( cTmp )
      DO WHILE zh_USubStr( cTmp, i, 1 ) == ";"        /* remove last ";" */
         cTmp := zh_ULeft( cTmp, --i )
      ENDDO

      FOR EACH cDir IN zh_ATokens( cTmp, ";" )
         IF zh_vfExists( cTmp := zh_FNameMerge( cDir, cName, cExt ) )
            cFile := cTmp
            EXIT
         ENDIF
      NEXT
   ENDIF

   nRetries := 0
   DO WHILE ( hFile := zh_vfOpen( cFile, FO_READWRITE ) ) == NIL
      oErr := ErrorNew()
      oErr:severity    := ES_ERROR
      oErr:genCode     := EG_OPEN
      oErr:subSystem   := "BASE"
      oErr:SubCode     := 2011
      oErr:canDefault  := .T.
      oErr:canRetry    := .T.
      oErr:fileName    := cFile
      oErr:OsCode      := FError()
      oErr:tries       := ++nRetries
      IF ! zh_defaultValue( Eval( ErrorBlock(), oErr ), .T. )  /* user select "Default" */
         RETURN
      ENDIF
   ENDDO

   /* NOTE: the NG say you should explicitly SET CONSOLE OFF if you wish to
            suppress output to screen. [ckedem] */

   IF lPrint
      aSaveSet[ 1 ] := Set( _SET_DEVICE, "PRINTER" )
      aSaveSet[ 2 ] := Set( _SET_PRINTER, .T. )
   ENDIF

   nSize   := zh_vfSize( hFile )
   nBuffer := Min( nSize, BUFFER_LENGTH )

   zh_vfSeek( hFile, 0 )  /* go top */

   /* Here we try to read a line at a time but I think we could just
      display the whole buffer since it said:
      "without any headings or formatting" */

   nHasRead := 0
   cBuffer := Space( nBuffer )
   QOut()  /* starting a new line */
   DO WHILE ( nRead := zh_vfRead( hFile, @cBuffer, nBuffer ) ) > 0
      nHasRead += nRead
      QQOut( cBuffer )
      nBuffer := Min( nSize - nHasRead, nBuffer )
      cBuffer := Space( nBuffer )
   ENDDO

   zh_vfClose( hFile )

   IF lPrint
      Set( _SET_DEVICE,  aSaveSet[ 1 ] )
      Set( _SET_PRINTER, aSaveSet[ 2 ] )
   ENDIF

   RETURN
