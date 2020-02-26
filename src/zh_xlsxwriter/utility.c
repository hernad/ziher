/*****************************************************************************
 * utility - Utility functions for libxlsxwriter.
 *
 * Used in conjunction with the libxlsxwriter library.
 *
 * Copyright 2014-2019, John McNamara, jmcnamara@cpan.org. See LICENSE.txt.
 *
 */
/*
 * Wrapped for Ziher by Riztan Gutierrez, riztan@gmail.com
 *
 */

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include "xlsxwriter.h"
#include "xlsxwriter/third_party/tmpfileplus.h"

#include "zh_api.h"

/*
char *
lxw_strerror(lxw_error error_num)
*/
ZH_FUNC( LXW_STRERROR )
{
   lxw_error error_num = zh_parni( 1 );
   zh_retc( lxw_strerror( error_num ) ) ;
}



/*
 * Convert Excel A-XFD style column name to zero based number.
 */
/*
void
lxw_col_to_name(char *col_name, lxw_col_t col_num, uint8_t absolute)
*/
ZH_FUNC( LXW_COL_TO_NAME )
{ 
   char *col_name = zh_param( 1, ZH_IT_STRING ) ;
   lxw_col_t col_num = zh_parni( 2 ) ;
   uint8_t absolute = zh_parni( 3 ) ;

   lxw_col_to_name(col_name, col_num, absolute) ; 
}





/*
 * Convert zero indexed row and column to an Excel style A1 cell reference.
 */
/*
void
lxw_rowcol_to_cell(char *cell_name, lxw_row_t row, lxw_col_t col)
*/
ZH_FUNC( LXW_ROWCOL_TO_CELL )
{ 
   char *cell_name = zh_param( 1, ZH_IT_STRING ) ;
   lxw_row_t row = zh_parni( 2 ) ;
   lxw_col_t col = zh_parni( 3 ) ;

   lxw_rowcol_to_cell(cell_name, row, col) ; 
}





/*
 * Convert zero indexed row and column to an Excel style $A$1 cell with
 * an absolute reference.
 */
/*
void
lxw_rowcol_to_cell_abs(char *cell_name, lxw_row_t row, lxw_col_t col,
   uint8_t abs_row, uint8_t abs_col)
*/
ZH_FUNC( LXW_ROWCOL_TO_CELL_ABS )
{ 
   char *cell_name = zh_param( 1, ZH_IT_STRING ) ;
   lxw_row_t row = zh_parni( 2 ) ;
   lxw_col_t col = zh_parni( 3 ) ;
   uint8_t abs_row = zh_parni( 4 ) ;
   uint8_t abs_col = zh_parni( 5 ) ;

   lxw_rowcol_to_cell_abs(cell_name, row, col, abs_row, abs_col) ; 
}





/*
 * Convert zero indexed row and column pair to an Excel style A1:C5
 * range reference.
 */
/*
void
lxw_rowcol_to_range(char *range,
   lxw_row_t first_row, lxw_col_t first_col,
   lxw_row_t last_row, lxw_col_t last_col)
*/
ZH_FUNC( LXW_ROWCOL_TO_RANGE )
{ 
   char *range = zh_param( 1, ZH_IT_STRING ) ;
   lxw_row_t first_row = zh_parni( 2 ) ;
   lxw_col_t first_col = zh_parni( 3 ) ;
   lxw_row_t last_row = zh_parni( 4 ) ;
   lxw_col_t last_col = zh_parni( 5 ) ;

   lxw_rowcol_to_range(range, first_row, first_col, last_row, last_col) ; 
}





/*
 * Convert zero indexed row and column pairs to an Excel style $A$1:$C$5
 * range reference with absolute values.
 */
/*
void
lxw_rowcol_to_range_abs(char *range,
   lxw_row_t first_row, lxw_col_t first_col,
   lxw_row_t last_row, lxw_col_t last_col)
*/
ZH_FUNC( LXW_ROWCOL_TO_RANGE_ABS )
{ 
   char *range = zh_param( 1, ZH_IT_STRING ) ;
   lxw_row_t first_row = zh_parni( 2 ) ;
   lxw_col_t first_col = zh_parni( 3 ) ;
   lxw_row_t last_row = zh_parni( 4 ) ;
   lxw_col_t last_col = zh_parni( 5 ) ;

   lxw_rowcol_to_range_abs(range, first_row, first_col, last_row, last_col) ; 
}





/*
 * Convert sheetname and zero indexed row and column pairs to an Excel style
 * Sheet1!$A$1:$C$5 formula reference with absolute values.
 */
/*
void
lxw_rowcol_to_formula_abs(char *formula, const char *sheetname,
   lxw_row_t first_row, lxw_col_t first_col,
   lxw_row_t last_row, lxw_col_t last_col)
*/
ZH_FUNC( LXW_ROWCOL_TO_FORMULA_ABS )
{ 
   char *formula = zh_param( 1, ZH_IT_STRING ) ;
   const char *sheetname = zh_parcx( 2 ) ;
   lxw_row_t first_row = zh_parni( 3 ) ;
   lxw_col_t first_col = zh_parni( 4 ) ;
   lxw_row_t last_row = zh_parni( 5 ) ;
   lxw_col_t last_col = zh_parni( 6 ) ;

   lxw_rowcol_to_formula_abs(formula, sheetname, first_row, first_col, last_row, last_col) ; 
}





/*
 * Convert an Excel style A1 cell reference to a zero indexed row number.
 */
/*
lxw_row_t
lxw_name_to_row(const char *row_str)
*/
ZH_FUNC( LXW_NAME_TO_ROW )
{ 
   const char *row_str = zh_parcx( 1 ) ;

   zh_retni( lxw_name_to_row(row_str) ); 
}





/*
 * Convert an Excel style A1 cell reference to a zero indexed column number.
 */
/*
lxw_col_t
lxw_name_to_col(const char *col_str)
*/
ZH_FUNC( LXW_NAME_TO_COL )
{ 
   const char *col_str = zh_parcx( 1 ) ;

   zh_retni( lxw_name_to_col(col_str) ); 
}





/*
 * Convert the second row of an Excel range ref to a zero indexed number.
 */
/*
uint32_t
lxw_name_to_row_2(const char *row_str)
*/
ZH_FUNC( LXW_NAME_TO_ROW_2 )
{ 
   const char *row_str = zh_parcx( 1 ) ;

   zh_retni( lxw_name_to_row_2(row_str) ); 
}





/*
 * Convert the second column of an Excel range ref to a zero indexed number.
 */
/*
uint16_t
lxw_name_to_col_2(const char *col_str)
*/
ZH_FUNC( LXW_NAME_TO_COL_2 )
{ 
   const char *col_str = zh_parcx( 1 ) ;

   zh_retnl( lxw_name_to_col_2(col_str) ); 
}





/*
 * Convert a lxw_datetime struct to an Excel serial date.
 */
/*
double
lxw_datetime_to_excel_date(lxw_datetime *datetime, uint8_t date_1904)
*/
/*
ZH_FUNC( LXW_DATETIME_TO_EXCEL_DATE )
{ 
   lxw_datetime *datetime = zh_parXX( 1 ) ;
   uint8_t date_1904 = zh_parni( 2 ) ;

   zh_retnl( lxw_datetime_to_excel_date(datetime, date_1904) ); 
}
*/




/* Create a quoted version of the worksheet name, or return an unmodified
 * copy if it doesn't required quoting. 
 *
char *
lxw_quote_sheetname(const char *str)
*/
ZH_FUNC( LXW_QUOTE_SHEETNAME )
{ 
   const char *str = zh_parcx( 1 ) ;

   zh_retc( lxw_quote_sheetname(str) ); 
}






/*
 * Thin wrapper for tmpfile() so it can be over-ridden with a user defined
 * version if required for safety or portability.
 */
/*
FILE *
lxw_tmpfile(char *tmpdir)
*/
ZH_FUNC( LXW_TMPFILE )
{ 
   char *tmpdir = zh_param( 1, ZH_IT_STRING ) ;

   zh_retptr( lxw_tmpfile(tmpdir) ); 
}






/*
 * Retrieve runtime library version
 */
/*
const char *
lxw_version(void)
*/
ZH_FUNC( LXW_VERSION )
{ 
   const char *str = lxw_version() ;
   zh_retc( str ); 
}


//eof
