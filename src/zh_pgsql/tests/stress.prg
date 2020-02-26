/* VERY IMPORTANT: Don't use this querys as sample, they are used for stress tests !!! */

#require "hbpgsql"

PROCEDURE Main( cHost, cDatabase, cUser, cPass )

   LOCAL conn, res, i, x

   LOCAL cQuery

   CLS

   ? "Connecting..."
   conn := PQconnectdb( ;
      "dbname = '" + zh_defaultValue( cDatabase, "postgres" ) + "' " + ;
      "host = '" + zh_defaultValue( cHost, "localhost" ) + "' " + ;
      "user = '" + zh_defaultValue( cUser, zh_UserName() ) + "' " + ;
      "password = '" + zh_defaultValue( cPass, "" ) + "' " + ;
      "port = 5432" )

   ? PQstatus( conn ), PQerrorMessage( conn )

   IF PQstatus( conn ) != CONNECTION_OK
      RETURN
   ENDIF

   ? "Dropping table..."

   PQexec( conn, "DROP TABLE test" )

   ? "Creating test table..."
   cQuery := ;
      "CREATE TABLE test(" + ;
      "   Code integer not null primary key," + ;
      "   dept Integer," + ;
      "   Name Varchar(40)," + ;
      "   Sales boolean," + ;
      "   Tax Float4," + ;
      "   Salary Double Precision," + ;
      "   Budget Numeric(12,2)," + ;
      "   Discount Numeric(5,2)," + ;
      "   Creation Date," + ;
      "   Description text )"

   PQexec( conn, cQuery )
   PQexec( conn, "SELECT code, dept, name, sales, salary, creation FROM test" )
   PQexec( conn, "BEGIN" )

   ?
   FOR i := 1 TO 10000
      @ 15, 0 SAY "Inserting values... " + zh_ntos( i )

      cQuery := "INSERT INTO test(code, dept, name, sales, salary, creation) " + ;
         "VALUES( " + zh_ntos( i ) + "," + zh_ntos( i + 1 ) + ", 'DEPARTMENT NAME " + StrZero( i ) + "', 'y', " + zh_ntos( 300.49 + i ) + ", '2003-12-28' )"

      PQexec( conn, cQuery )

      IF i % 100 == 0
         ? PQexec( conn, "COMMIT" )
         ? PQexec( conn, "BEGIN" )
      ENDIF
   NEXT

   FOR i := 5000 TO 7000
      @ 16, 0 SAY "Deleting values... " + zh_ntos( i )

      cQuery := "DELETE FROM test WHERE code = " + zh_ntos( i )
      PQexec( conn, cQuery )

      IF i % 100 == 0
         PQexec( conn, "COMMIT" )
         PQexec( conn, "BEGIN" )
      ENDIF
   NEXT

   FOR i := 2000 TO 3000
      @ 17, 0 SAY "Updating values... " + zh_ntos( i )

      cQuery := "UPDATE FROM test SET salary = 400 WHERE code = " + zh_ntos( i )
      PQexec( conn, cQuery )

      IF i % 100 == 0
         PQexec( conn, "COMMIT" )
         PQexec( conn, "BEGIN" )
      ENDIF
   NEXT

   res := PQexec( conn, "SELECT sum(salary) as sum_salary FROM test WHERE code between 1 and 4000" )

   IF PQresultStatus( res ) == PGRES_TUPLES_OK
      @ 18, 0 SAY "Sum values... " + PQgetvalue( res, 1, 1 )
   ENDIF

   x := 0
   FOR i := 1 TO 4000
      res := PQexec( conn, "SELECT salary FROM test WHERE code = " + zh_ntos( i ) )

      IF PQresultStatus( res ) == PGRES_TUPLES_OK
         x += Val( PQgetvalue( res, 1, 1 ) )

         @ 19, 0 SAY "Sum values... " + zh_ntos( x )
      ENDIF
   NEXT

   ? "Closing..."

   RETURN
