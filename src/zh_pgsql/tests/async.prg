/* This sample show howto use asynchronous/nonblocking queries */

#require "hbpgsql"

#include "inkey.zhh"

PROCEDURE Main( cHost, cDatabase, cUser, cPass )

   LOCAL conn

   CLS

   ? "Connect", conn := PQconnectdb( ;
      "dbname = '" + zh_defaultValue( cDatabase, "postgres" ) + "' " + ;
      "host = '" + zh_defaultValue( cHost, "localhost" ) + "' " + ;
      "user = '" + zh_defaultValue( cUser, zh_UserName() ) + "' " + ;
      "password = '" + zh_defaultValue( cPass, "" ) + "' " + ;
      "port = 5432" )

   ? "Conection status", PQerrorMessage( conn ), PQstatus( conn )

   Query( conn, "SELECT codigo, descri FROM client limit 100", .F. )
   Query( conn, "SELECT codigo, descri FROM fornec limit 100", .F. )
   Query( conn, "SELECT pedido, vlrped FROM pedido", .T. )

   RETURN

STATIC PROCEDURE Query( conn, cQuery, lCancel )

   LOCAL pCancel, cErrMsg := ""
   LOCAL res, x, y, cTime

   ? "PQSendQuery", PQsendQuery( conn, cQuery )

   cTime := Time()
   CLEAR TYPEAHEAD

   DO WHILE zh_keyStd( Inkey() ) != K_ESC
      DevPos( Row(), 20 )
      DevOut( "Processing:", ElapTime( cTime, Time() ) )

      Inkey( 1 )

      IF lCancel
         pCancel := PQgetCancel( conn )
         ? "Canceled:", PQcancel( pCancel, @cErrMsg ), cErrMsg
         pCancel := NIL
      ENDIF

      IF PQconsumeInput( conn )
         IF ! PQisBusy( conn )
            EXIT
         ENDIF
      ENDIF
   ENDDO

   IF zh_keyStd( Inkey() ) != K_ESC
      ? "PQgetResult", zh_ValToExp( res := PQgetResult( conn ) )

      IF ! Empty( res )
         FOR x := 1 TO PQlastrec( res )
            ?
            FOR y := 1 TO PQfcount( res )
               ?? PQgetvalue( res, x, y ), " "
            NEXT
         NEXT
      ENDIF
   ELSE
      ? "Canceling Query", PQrequestCancel( conn )
   ENDIF

   RETURN
