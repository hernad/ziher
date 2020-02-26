/*
 * Converts a .dbf file into a Postgres table
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
 * (The original file was ported from mysql and changed by Rodrigo Moreno rodrigo_moreno@yahoo.com)
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#require "hbpgsql"

#include "file_io.zhh"
#include "inkey.zhh"

#include "hbextcdp.zhh"

PROCEDURE Main()

   LOCAL cHostName
   LOCAL cUser
   LOCAL cPassword
   LOCAL cDatabase := "postgres", cTable, cFile
   LOCAL i
   LOCAL lCreateTable := .F.
   LOCAL oServer, oTable, oRecord
   LOCAL cField
   LOCAL cTypeDB
   LOCAL cTypePG
   LOCAL cValue
   LOCAL nCommit := 100
   LOCAL hFile
   LOCAL nCount := 0
   LOCAL nRecno := 0
   LOCAL lTruncate := .F.
   LOCAL lUseTrans := .F.
   LOCAL cPath := "public"

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   Set( _SET_DELETED, .T. )

   /* Scan parameters and setup workings */
   FOR i := 1 TO PCount()

      SWITCH zh_PValue( i )
      CASE "-h" ; cHostName := zh_PValue( ++i ) ; EXIT
      CASE "-d" ; cDatabase := zh_PValue( ++i ) ; EXIT
      CASE "-t" ; cTable := AllTrim( zh_PValue( ++i ) ) ; EXIT
      CASE "-f" ; cFile := zh_PValue( ++i ) ; EXIT
      CASE "-u" ; cUser := zh_PValue( ++i ) ; EXIT
      CASE "-p" ; cPassword := zh_PValue( ++i ) ; EXIT
      CASE "-c" ; lCreateTable := .T. ; EXIT
      CASE "-x" ; lTruncate := .T. ; EXIT
      CASE "-s" ; lUseTrans := .T. ; EXIT
      CASE "-m" ; nCommit := Val( zh_PValue( ++i ) ) ; EXIT
      CASE "-r" ; nRecno := Val( zh_PValue( ++i ) ) ; EXIT
      CASE "-e" ; cPath := zh_PValue( ++i ) ; EXIT
      CASE "-cp" ; Set( _SET_DBCODEPAGE, zh_PValue( ++i ) ) ; EXIT
      OTHERWISE
         help()
         RETURN
      ENDSWITCH
   NEXT

   IF Empty( cTable ) .OR. ZH_ISNULL( cFile )
      help()
      RETURN
   ENDIF

   // create log file
   IF ( hFile := zh_vfOpen( cTable + ".log", FO_CREAT + FO_TRUNC + FO_WRITE ) ) == NIL
      ? "Cannot create log file"
      RETURN
   ENDIF

   USE ( cFile ) SHARED READONLY

   oServer := TPQServer():New( cHostName, cDatabase, cUser, cPassword, , cPath )
   IF oServer:NetErr()
      ? oServer:ErrorMsg()
      RETURN
   ENDIF

   oServer:lallCols := .F.

   IF lCreateTable
      IF oServer:TableExists( cTable )
         oServer:DeleteTable( cTable )
         IF oServer:NetErr()
            ? oServer:ErrorMsg()
            zh_vfWrite( hFile, "Error: " + oServer:ErrorMsg() + zh_eol() )
            zh_vfClose( hFile )
            RETURN
         ENDIF
      ENDIF

      oServer:CreateTable( cTable, dbStruct() )
      IF oServer:NetErr()
         ? oServer:ErrorMsg()
         zh_vfWrite( hFile, "Error: " + oServer:ErrorMsg() + zh_eol() )
         zh_vfClose( hFile )
         RETURN
      ENDIF
   ENDIF

   IF lTruncate
      oServer:Execute( "truncate table " + cTable )
      IF oServer:NetErr()
         ? oServer:ErrorMsg()
         zh_vfWrite( hFile, "Error: " + oServer:ErrorMsg() + zh_eol() )
         zh_vfClose( hFile )
         RETURN
      ENDIF
   ENDIF

   oTable := oServer:Query( "SELECT * FROM " + cTable + " LIMIT 1" )
   IF oTable:NetErr()
      ? oTable:ErrorMsg()
      zh_vfWrite( hFile, "Error: " + oTable:ErrorMsg() + zh_eol() )
      zh_vfClose( hFile )
      RETURN
   ENDIF

   IF lUseTrans
      oServer:StartTransaction()
   ENDIF

   zh_vfWrite( hFile, "Start: " + Time() + zh_eol() )

   ? "Start:", Time()
   ?

   IF nRecno != 0
      dbGoto( nRecno )
   ENDIF

   DO WHILE ! Eof() .AND. zh_keyStd( Inkey() ) != K_ESC .AND. ( nRecno == 0 .OR. nRecno == RecNo() )
      oRecord := oTable:GetBlankRow()

      FOR i := 1 TO oTable:FCount()
         cField := Lower( oTable:FieldName( i ) )
         cTypeDB := Left( zh_FieldType( FieldPos( cField ) ), 1 )
         cTypePG := oRecord:FieldType( i )
         cValue := FieldGet( FieldPos( cField ) )

         IF cValue != NIL
            IF cTypePG != cTypeDB
               DO CASE
               CASE cTypePG == "C" .AND. cTypeDB $ "NIYF8BZ24" ; cValue := zh_ntos( cValue )
               CASE cTypePG == "C" .AND. cTypeDB == "D" ; cValue := DToC( cValue )
               CASE cTypePG == "C" .AND. cTypeDB $ "T@" ; cValue := zh_TToC( cValue )
               CASE cTypePG == "C" .AND. cTypeDB == "L" ; cValue := iif( cValue, "S", "N" )
               CASE cTypePG == "N" .AND. cTypeDB $ "CQ" ; cValue := Val( cValue )
               CASE cTypePG == "N" .AND. cTypeDB == "D" ; cValue := Val( DToS( cValue ) )
               CASE cTypePG == "N" .AND. cTypeDB == "L" ; cValue := iif( cValue, 1, 0 )
               CASE cTypePG == "D" .AND. cTypeDB $ "CQ" ; cValue := CToD( cValue )
               CASE cTypePG == "D" .AND. cTypeDB $ "NIYF8BZ24" ; cValue := zh_SToD( zh_ntos( cValue ) )
               CASE cTypePG == "L" .AND. cTypeDB $ "NIYF8BZ24" ; cValue := ! Empty( cValue )
               CASE cTypePG == "L" .AND. cTypeDB $ "CQ" ; cValue := AllTrim( cValue ) $ "YySs1"
               ENDCASE
            ENDIF

            IF cValue != NIL
               IF oRecord:FieldType( i ) == "C" .OR. oRecord:FieldType( i ) == "M"
                  oRecord:FieldPut( i, zh_StrToUTF8( cValue ) )
               ELSE
                  oRecord:FieldPut( i, cValue )
               ENDIF
            ENDIF
         ENDIF
      NEXT

      oTable:Append( oRecord )

      IF oTable:NetErr()
         ?
         ? "Error Record:", RecNo(), Left( oTable:ErrorMsg(), 70 )
         ?
         zh_vfWrite( hFile, "Error at record: " + zh_ntos( RecNo() ) + " Description: " + oTable:ErrorMsg() + zh_eol() )
      ELSE
         nCount++
      ENDIF

      dbSkip()

      IF nCount % nCommit == 0
         DevPos( Row(), 1 )
         DevOut( "imported recs:", zh_ntos( nCount ) )

         IF lUseTrans
            oServer:commit()
            oServer:StartTransaction()
         ENDIF
      ENDIF
   ENDDO

   IF nCount % nCommit != 0 .AND. lUseTrans
      oServer:commit()
   ENDIF

   zh_vfWrite( hFile, "End: " + Time() + ", records in dbf: " + zh_ntos( RecNo() ) + ", imported recs: " + zh_ntos( nCount ) + zh_eol() )

   ? "End:", Time()

   zh_vfClose( hFile )

   dbCloseAll()

   oTable:Destroy()
   oServer:Destroy()

   RETURN

STATIC PROCEDURE Help()

   ? "dbf2pg - dbf file to PostgreSQL table conversion utility"
   ? "-h hostname"
   ? "-u user"
   ? "-p password"
   ? "-d name of database to use (default: postgres)"
   ? "-t name of table to add records to (required)"
   ? "-c delete existing table and create a new one"
   ? "-f name of .dbf file to import (required)"
   ? "-x truncate table before append records"
   ? "-s use transaction"
   ? "-m commit interval"
   ? "-r insert only record number"
   ? "-e search path"
   ?

   RETURN
