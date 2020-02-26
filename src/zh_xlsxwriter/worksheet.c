/*****************************************************************************
 * worksheet - A library for creating Excel XLSX worksheet files.
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


#include "xlsxwriter/xmlwriter.h"
#include "xlsxwriter/worksheet.h"
#include "xlsxwriter/format.h"
#include "xlsxwriter/utility.h"
#include "xlsxwriter/relationships.h"

#include "zh_api.h"
#include "zh_error_api.h"
#include "zh_item_api.h"
#include "zh_set.h"
#include "zh_date.h"

/*
#define LXW_STR_MAX   32767
#define LXW_BUFFER_SIZE   4096
#define LXW_PRINT_ACROSS   1
#define LXW_VALIDATION_MAX_TITLE_LENGTH  32
#define LXW_VALIDATION_MAX_STRING_LENGTH 255
*/


/*
 * Find but don't create a row object for a given row number.
 *
 * lxw_row *
 * lxw_worksheet_find_row(lxw_worksheet *self, lxw_row_t row_num)
 * 
 */
ZH_FUNC( LXW_WORKSHEET_FIND_ROW )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;

   zh_retptr( lxw_worksheet_find_row( self, row_num) ); 
}

/*
 * Find but don't create a cell object for a given row object and col number.
 *
 * lxw_cell *
 * lxw_worksheet_find_cell(lxw_row *row, lxw_col_t col_num)
 *
 */
ZH_FUNC( LXW_WORKSHEET_FIND_CELL )
{ 
   lxw_row *row = zh_parptr(1 ) ;
   lxw_col_t col_num = zh_parni( 2 ) ;

   zh_retptr( lxw_worksheet_find_cell( row, col_num) ); 
}



/*
 * Create a new worksheet object.
 *
 * lxw_worksheet *
 * lxw_worksheet_new(lxw_worksheet_init_data *init_data)
 *
 */
ZH_FUNC( LXW_WORKSHEET_NEW )
{ 
   lxw_worksheet_init_data *init_data = zh_parptr( 1 ) ;
   if ZH_ISNIL( 1 )
   {
      zh_retptr( lxw_worksheet_new( NULL ) ); 
   }
   else
   {
      zh_retptr( lxw_worksheet_new( init_data ) ); 
   }
}



/*
 * Free a worksheet object.
 *
 * void
 * lxw_worksheet_free(lxw_worksheet *worksheet)
 *
 */
ZH_FUNC( LXW_WORKSHEET_FREE )
{ 
   lxw_worksheet *worksheet = zh_parptr( 1 ) ;

   lxw_worksheet_free( worksheet ) ; 
}



/*
 * Simple replacement for libgen.h basename() for compatibility with MSVC. It
 * handles forward and back slashes. It doesn't copy exactly the return
 * format of basename().
 *
 * char *
 * lxw_basename(const char *path)
 *
 */
/*
ZH_FUNC( LXW_BASENAME )
{ 
   const char *path = zh_parcx( 1 ) ;

   zh_retc( lxw_basename( path ) ); 
}
*/



/*
 * Function to count the total concatenated length of the strings in a
 * validation list array, including commas. 
 *
 * size_t
 * _validation_list_length(char **list)
 *
 */
/*
ZH_FUNC( _VALIDATION_LIST_LENGTH )
{ 
   char **list = zh_parptr(1 ) ;

   zh_retnl( _validation_list_length( list ) ); 
}
*/



/*
 * Function to convert an array of strings into a CSV string for data
 * validation lists. 
 *
 * char *
 * _validation_list_to_csv(char **list)
 *
 */
/*
ZH_FUNC( _VALIDATION_LIST_TO_CSV )
{ 
   char **list = zh_parcx( 1 ) ;

   zh_retc( _validation_list_to_csv( list ) ); 
}
*/



/*
 *
 * XML functions.
 *
 ****************************************************************************/


/*
 * Set up image/drawings.
 *
 * void
 * lxw_worksheet_prepare_image(lxw_worksheet *self,
 *    uint16_t image_ref_id, uint16_t drawing_id,
 *    lxw_image_options *image_data)
 *
 */
ZH_FUNC( LXW_WORKSHEET_PREPARE_IMAGE )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   uint16_t image_ref_id = zh_parnl( 2 ) ;
   uint16_t drawing_id = zh_parnl( 3 ) ;
   lxw_image_options *image_data = zh_parptr(4 ) ; //REVISAR (RIGC 2019-05-24)

   lxw_worksheet_prepare_image( self, image_ref_id, drawing_id, image_data ); 
}


/*
 * Set up chart/drawings.
 *
 * void
 * lxw_worksheet_prepare_chart(lxw_worksheet *self,
 *    uint16_t chart_ref_id,
 *    uint16_t drawing_id,
 *    lxw_image_options *image_data,
 *    uint8_t is_chartsheet)
 *
 */
ZH_FUNC( LXW_WORKSHEET_PREPARE_CHART )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   uint16_t chart_ref_id = zh_parnl( 2 ) ;
   uint16_t drawing_id = zh_parnl( 3 ) ;
   lxw_image_options *image_data = zh_parptr(4 ) ;
   uint8_t is_chartsheet = zh_parni( 5 ) ;

   lxw_worksheet_prepare_chart( self, chart_ref_id, drawing_id, image_data, is_chartsheet) ; 
}




/*****************************************************************************
 *
 * XML file assembly functions.
 *
 ****************************************************************************/


/*
 * Write out the worksheet data as a single row with cells. This method is
 * used when memory optimization is on. A single row is written and the data
 * array is reset. That way only one row of data is kept in memory at any one
 * time. We don't write span data in the optimized case since it is optional.
 *
 * void
 * lxw_worksheet_write_single_row(lxw_worksheet *self)
 */
ZH_FUNC( LXW_WORKSHEET_WRITE_SINGLE_ROW )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   lxw_worksheet_write_single_row( self ); 
}



/*
 * External functions to call intern XML methods shared with chartsheet.
 *
 * void
 * lxw_worksheet_write_sheet_views(lxw_worksheet *self)
 */
ZH_FUNC( LXW_WORKSHEET_WRITE_SHEET_VIEWS )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   lxw_worksheet_write_sheet_views( self ); 
}



/*
 * Assemble and write the XML file.
 *
 * void
 * lxw_worksheet_assemble_xml_file(lxw_worksheet *self)
 */
ZH_FUNC( LXW_WORKSHEET_ASSEMBLE_XML_FILE )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   lxw_worksheet_assemble_xml_file( self ); 
}




/*
 *
 * Public functions.
 *
 ****************************************************************************/




/*
 * Write a number to a cell in Excel.
 *
 * lxw_error
 * worksheet_write_number(lxw_worksheet *self,
 *    lxw_row_t row_num,
 *    lxw_col_t col_num, double value, lxw_format *format)
 *
 */
ZH_FUNC( WORKSHEET_WRITE_NUMBER )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   lxw_col_t col_num = zh_parni( 3 ) ;
   double value = zh_parnd(4 ) ;
   lxw_format *format = zh_parptr(5 ) ;

   zh_retni( worksheet_write_number( self, row_num, col_num, value, format ) ); 
}






/*
 * Write a string to an Excel file.
 *
 * lxw_error
 * worksheet_write_string(lxw_worksheet *self,
 *    lxw_row_t row_num,
 *    lxw_col_t col_num, const char *string,
 *    lxw_format *format)
 *
 */
ZH_FUNC( WORKSHEET_WRITE_STRING )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num   = zh_parni( 2 ) ;
   lxw_col_t col_num   = zh_parni( 3 ) ;
   const char *string  = zh_parcx( 4 ) ;
   lxw_format *format  = zh_parptr(5 ) ;

   zh_retni( worksheet_write_string( self, row_num, col_num, string, format) ); 
}





/*
 * Write a formula with a numerical result to a cell in Excel.
 *
 * lxw_error
 * worksheet_write_formula_num(lxw_worksheet *self,
 *    lxw_row_t row_num,
 *    lxw_col_t col_num,
 *    const char *formula, 
 *    lxw_format *format, double result)
 */
ZH_FUNC( WORKSHEET_WRITE_FORMULA_NUM )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   lxw_col_t col_num = zh_parni( 3 ) ;
   const char *formula = zh_parcx( 4 ) ;
   lxw_format *format = zh_parptr(5 ) ;
   double result = zh_parnd(6 ) ;

   zh_retni( worksheet_write_formula_num( self, row_num, col_num, formula, format, result ) ); 
}




/*
 * Write a formula with a default result to a cell in Excel .
 *
 * lxw_error
 * worksheet_write_formula(lxw_worksheet *self,
 *    lxw_row_t row_num,
 *    lxw_col_t col_num, const char *formula,
 *    lxw_format *format)
 */
ZH_FUNC( WORKSHEET_WRITE_FORMULA )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   lxw_col_t col_num = zh_parni( 3 ) ;
   const char *formula = zh_parcx( 4 ) ;
   lxw_format *format = zh_parptr( 5 ) ;

   zh_retni( worksheet_write_formula( self, row_num, col_num, formula, format ) ); 
}




/*
 * Write a formula with a numerical result to a cell in Excel.
 *
 * lxw_error
 * worksheet_write_array_formula_num(lxw_worksheet *self,
 *    lxw_row_t first_row,
 *    lxw_col_t first_col,
 *    lxw_row_t last_row,
 *    lxw_col_t last_col,
 *    const char *formula,
 *    lxw_format *format, double result)
 */
ZH_FUNC( WORKSHEET_WRITE_ARRAY_FORMULA_NUM )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t first_row = zh_parni( 2 ) ;
   lxw_col_t first_col = zh_parni( 3 ) ;
   lxw_row_t last_row = zh_parni( 4 ) ;
   lxw_col_t last_col = zh_parni( 5 ) ;
   const char *formula = zh_parcx( 6 ) ;
   lxw_format *format = zh_parptr(7 ) ;
   double result = zh_parnd(8 ) ;

   zh_retni( worksheet_write_array_formula_num( self, first_row, first_col, last_row, last_col, formula, format, result ) ); 
}


/*
 * Write an array formula with a default result to a cell in Excel .
 *
 * lxw_error
 * worksheet_write_array_formula(lxw_worksheet *self,
 *    lxw_row_t first_row,
 *    lxw_col_t first_col,
 *    lxw_row_t last_row,
 *    lxw_col_t last_col,
 *    const char *formula, lxw_format *format)
 */
ZH_FUNC( WORKSHEET_WRITE_ARRAY_FORMULA )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t first_row = zh_parni( 2 ) ;
   lxw_col_t first_col = zh_parni( 3 ) ;
   lxw_row_t last_row = zh_parni( 4 ) ;
   lxw_col_t last_col = zh_parni( 5 ) ;
   const char *formula = zh_parcx( 6 ) ;
   lxw_format *format = zh_parptr(7 ) ;

   zh_retni( worksheet_write_array_formula( self, first_row, first_col, last_row, last_col, formula, format ) ); 
}




/*
 * Write a blank cell with a format to a cell in Excel.
 *
 * lxw_error
 * worksheet_write_blank(lxw_worksheet *self,
 *    lxw_row_t row_num, lxw_col_t col_num,
 *    lxw_format *format)
 */
ZH_FUNC( WORKSHEET_WRITE_BLANK )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   lxw_col_t col_num = zh_parni( 3 ) ;
   lxw_format *format = zh_parptr(4 ) ;

   zh_retni( worksheet_write_blank( self, row_num, col_num, format ) ); 
}



/*
 * Write a boolean cell with a format to a cell in Excel.
 *
 * lxw_error
 * worksheet_write_boolean(lxw_worksheet *self,
 *    lxw_row_t row_num, lxw_col_t col_num,
 *    int value, lxw_format *format)
 */
ZH_FUNC( WORKSHEET_WRITE_BOOLEAN )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   lxw_col_t col_num = zh_parni( 3 ) ;
   int value = zh_parni( 4 ) ;
   lxw_format *format = zh_parptr(5 ) ;

   zh_retni( worksheet_write_boolean( self, row_num, col_num, value, format ) ); 
}




/*
 * Write a date and or time to a cell in Excel.
 *
 * lxw_error
 * worksheet_write_datetime(lxw_worksheet *self,
 *    lxw_row_t row_num,
 *    lxw_col_t col_num, lxw_datetime *datetime,
 *    lxw_format *format)
 */


ZH_FUNC( WORKSHEET_WRITE_DATETIME )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   lxw_col_t col_num = zh_parni( 3 ) ;
   lxw_datetime datetime; 

   long lDate, lTime;

   if( zh_partdt( &lDate, &lTime, 4 ) )
   {
       int iYear, iMonth, iDay ;
       int iHour, iMin, iSec, iMSec ;

       zh_timeDecode( lTime, &iHour, &iMin, &iSec, &iMSec );
       zh_dateDecode( lDate, &iYear, &iMonth, &iDay );

       datetime.year = iYear;
       datetime.month = iMonth;
       datetime.day = iDay;
       datetime.hour = iHour;
       datetime.min = iMin;
       datetime.sec = iSec;
   }

   lxw_format *format = zh_parptr(5 ) ;

   zh_retni( worksheet_write_datetime( self, row_num, col_num, &datetime, format ) ); 
}




/*
 * Write a hyperlink/url to an Excel file.
 *
 * lxw_error
 * worksheet_write_url_opt(lxw_worksheet *self,
 *    lxw_row_t row_num,
 *    lxw_col_t col_num, const char *url,
 *    lxw_format *format, const char *string,
 *    const char *tooltip)
 */
ZH_FUNC( WORKSHEET_WRITE_URL_OPT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   lxw_col_t col_num = zh_parni( 3 ) ;
   const char *url = zh_parcx( 4 ) ;
   lxw_format *format = zh_parptr(5 ) ;
   const char *string = zh_parcx( 6 ) ;
   const char *tooltip = zh_parcx( 7 ) ;

   zh_retni( worksheet_write_url_opt( self, row_num, col_num, url, format, string, tooltip ) ); 
}




/*
 * Write a hyperlink/url to an Excel file.
 *
 * lxw_error
 * worksheet_write_url(lxw_worksheet *self,
 *    lxw_row_t row_num,
 *    lxw_col_t col_num, const char *url, lxw_format *format)
 */
ZH_FUNC( WORKSHEET_WRITE_URL )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   lxw_col_t col_num = zh_parni( 3 ) ;
   const char *url = zh_parcx( 4 ) ;
   lxw_format *format = zh_parptr(5 ) ;

   zh_retni( worksheet_write_url( self, row_num, col_num, url, format ) ); 
}




/*
 * Write a rich string to an Excel file.
 *
 * Rather than duplicate several of the styles.c font xml methods of styles.c
 * and write the data to a memory buffer this function creates a temporary
 * styles object and uses it to write the data to a file. It then reads that
 * data back into memory and closes the file.
 *
 * lxw_errorwork
 * sheet_write_rich_string(lxw_worksheet *self,
 *    lxw_row_t row_num,
 *    lxw_col_t col_num,
 *    lxw_rich_string_tuple *rich_strings[],
 *    lxw_format *format)
 */
ZH_FUNC( WORKSHEET_WRITE_RICH_STRING )
{
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num   = zh_parni( 2 ) ;
   lxw_col_t col_num   = zh_parni( 3 ) ;
   PZH_ITEM pArray     = zh_param( 4, ZH_IT_ARRAY );
   lxw_format *format  = zh_parptr( 5 );

   if( pArray && ZH_IS_ARRAY( pArray ) && zh_arrayLen( pArray ) > 0 )
   {
      ZH_SIZE nLen = zh_arrayLen( pArray );
      lxw_rich_string_tuple **rich_strings = ( lxw_rich_string_tuple **) 
                                               zh_xalloc( sizeof( lxw_rich_string_tuple ) 
                                                          * nLen );

      ZH_SIZE nPos = 0;
      while( ++nPos <= nLen )
      {

         PZH_ITEM pHash = zh_arrayGetItemPtr( pArray, nPos ) ;
         lxw_rich_string_tuple *tuple = ( lxw_rich_string_tuple *) 
                                          zh_xalloc( sizeof( lxw_rich_string_tuple ) );

         if( pHash && zh_hashLen(pHash) > 0 )
         {

            ZH_SIZE  nLenHash = zh_hashLen( pHash ), nPosHash = 0;

            while( ++nPosHash <= nLenHash  ){

               PZH_ITEM pKey     = zh_hashGetKeyAt( pHash, nPosHash );
               PZH_ITEM pValue   = zh_hashGetValueAt( pHash, nPosHash );

               char *key = (char *)zh_itemGetC( pKey );

               if( zh_stricmp( key, "format" ) == 0 ){
                  tuple->format = (lxw_format *) zh_itemGetPtr( pValue ) ;

               }
               if( zh_stricmp( key, "string" ) == 0 ){
                  tuple->string = (char *) zh_itemGetC( pValue ) ;
               }

            }
            rich_strings[ nPos-1 ] = ( lxw_rich_string_tuple *) tuple;

         }
         else{
            zh_errRT_BASE( EG_ARG, 0, "Not LXW_RICH_STRING Hash ", ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
	    return;
         }
      }
      if ( nLen>0 ){

         zh_retni( worksheet_write_rich_string( self, row_num, col_num, rich_strings, format ) ); 

	 nPos = 0;
	 while( rich_strings[ nPos ] ){
            zh_xfree( rich_strings[ nPos ] );
	    nPos++;
	 }


         zh_xfree( rich_strings );
      }
      else{
         zh_errRT_BASE( EG_ARG, 0, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
      }

   }
   else{
      zh_errRT_BASE( EG_ARG, 0, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }

}





/*
 * Set the properties of a single column or a range of columns with options.
 *
 * lxw_error
 * worksheet_set_column_opt(lxw_worksheet *self,
 *    lxw_col_t firstcol,
 *    lxw_col_t lastcol,
 *    double width,
 *    lxw_format *format,
 *    lxw_row_col_options *user_options)
 *
 */
ZH_FUNC( WORKSHEET_SET_COLUMN_OPT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_col_t firstcol = zh_parni( 2 ) ;
   lxw_col_t lastcol = zh_parni( 3 ) ;
   double width = zh_parnd( 4 ) ;
   lxw_format *format = zh_parptr(5 ) ;
   lxw_row_col_options *user_options = zh_parptr(6 ) ;

   zh_retni( worksheet_set_column_opt( self, firstcol, lastcol, width, format, user_options) ); 
}





/*
 * Set the properties of a single column or a range of columns.
 *
 * lxw_error
 * worksheet_set_column(lxw_worksheet *self,
 *    lxw_col_t firstcol,
 *    lxw_col_t lastcol, double width, lxw_format *format)
 */
ZH_FUNC( WORKSHEET_SET_COLUMN )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_col_t firstcol = zh_parni( 2 ) ;
   lxw_col_t lastcol = zh_parni( 3 ) ;
   double width = zh_parnd( 4 ) ;
   lxw_format *format = zh_parptr(5 ) ;

   zh_retni( worksheet_set_column( self, firstcol, lastcol, width, format) ); 
}




/*
 * Set the properties of a row with options.
 *
 * lxw_error
 * worksheet_set_row_opt(lxw_worksheet *self,
 *    lxw_row_t row_num,
 *    double height,
 *    lxw_format *format, lxw_row_col_options *user_options)
 */
ZH_FUNC( WORKSHEET_SET_ROW_OPT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   double height = zh_parnd( 3 ) ;
   lxw_format *format = zh_parptr(4 ) ;
   lxw_row_col_options *user_options = zh_parptr(5 ) ;

   zh_retni( worksheet_set_row_opt( self, row_num, height, format, user_options) ); 
}




/*
 * Set the properties of a row.
 *
 * lxw_error
 * worksheet_set_row(lxw_worksheet *self,
 *    lxw_row_t row_num, double height, lxw_format *format)
 *
 */
ZH_FUNC( WORKSHEET_SET_ROW )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   double height = zh_parnd( 3 ) ;
   lxw_format *format = zh_parptr(4 ) ;

   zh_retni( worksheet_set_row( self, row_num, height, format) ); 
}




/*
 * Merge a range of cells. The first cell should contain the data and the others
 * should be blank. All cells should contain the same format.
 *
 * lxw_error
 * worksheet_merge_range(lxw_worksheet *self, lxw_row_t first_row,
 *    lxw_col_t first_col, lxw_row_t last_row,
 *    lxw_col_t last_col, const char *string,
 *    lxw_format *format)
 */
ZH_FUNC( WORKSHEET_MERGE_RANGE )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t first_row = zh_parni( 2 ) ;
   lxw_col_t first_col = zh_parni( 3 ) ;
   lxw_row_t last_row = zh_parni( 4 ) ;
   lxw_col_t last_col = zh_parni( 5 ) ;
   const char *string = zh_parcx( 6 ) ;
   lxw_format *format = zh_parptr(7 ) ;

   zh_retni( worksheet_merge_range( self, first_row, first_col, last_row, last_col, string, format) ); 
}




/*
 * Set the autofilter area in the worksheet.
 *
 * lxw_error
 * worksheet_autofilter(lxw_worksheet *self, lxw_row_t first_row,
 *    lxw_col_t first_col, lxw_row_t last_row,
 *    lxw_col_t last_col)
 */
ZH_FUNC( WORKSHEET_AUTOFILTER )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t first_row = zh_parni( 2 ) ;
   lxw_col_t first_col = zh_parni( 3 ) ;
   lxw_row_t last_row  = zh_parni( 4 ) ;
   lxw_col_t last_col  = zh_parni( 5 ) ;

   zh_retni( worksheet_autofilter( self, first_row, first_col, last_row, last_col ) ); 
}




/*
 * Set this worksheet as a selected worksheet, i.e. the worksheet has its tab
 * highlighted.
 *
 * void
 * worksheet_select(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_SELECT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_select( self ) ; 
}




/*
 * Set this worksheet as the active worksheet, i.e. the worksheet that is
 * displayed when the workbook is opened. Also set it as selected.
 *
 * void
 * worksheet_activate(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_ACTIVATE )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_activate( self ); 
}




/*
 * Set this worksheet as the first visible sheet. This is necessary
 * when there are a large number of worksheets and the activated
 * worksheet is not visible on the screen.
 *
 * void
 * worksheet_set_first_sheet(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_SET_FIRST_SHEET )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_set_first_sheet( self ); 
}




/*
 * Hide this worksheet.
 *
 * void
 * worksheet_hide(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_HIDE )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_hide( self ); 
}




/*
 * Set which cell or cells are selected in a worksheet.
 *
 * void
 * worksheet_set_selection(lxw_worksheet *self,
 *    lxw_row_t first_row, lxw_col_t first_col,
 *    lxw_row_t last_row, lxw_col_t last_col)
 *
 */
ZH_FUNC( WORKSHEET_SET_SELECTION )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t first_row = zh_parni( 2 ) ;
   lxw_col_t first_col = zh_parni( 3 ) ;
   lxw_row_t last_row = zh_parni( 4 ) ;
   lxw_col_t last_col = zh_parni( 5 ) ;

   worksheet_set_selection( self, first_row, first_col, last_row, last_col); 
}




/*
 * Set panes and mark them as frozen. With extra options.
 *
 * void
 * worksheet_freeze_panes_opt(lxw_worksheet *self,
 *    lxw_row_t first_row, lxw_col_t first_col,
 *    lxw_row_t top_row, lxw_col_t left_col,
 *    uint8_t type)
 *
 */
ZH_FUNC( WORKSHEET_FREEZE_PANES_OPT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t first_row = zh_parni( 2 ) ;
   lxw_col_t first_col = zh_parni( 3 ) ;
   lxw_row_t top_row = zh_parni( 4 ) ;
   lxw_col_t left_col = zh_parni( 5 ) ;
   uint8_t type = zh_parni( 6 ) ;

   worksheet_freeze_panes_opt( self, first_row, first_col, top_row, left_col, type); 
}




/*
 * Set panes and mark them as frozen.
 *
 * void
 * worksheet_freeze_panes(lxw_worksheet *self,
 *    lxw_row_t first_row, lxw_col_t first_col)
 *
 */
ZH_FUNC( WORKSHEET_FREEZE_PANES )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t first_row = zh_parni( 2 ) ;
   lxw_col_t first_col = zh_parni( 3 ) ;

   worksheet_freeze_panes( self, first_row, first_col); 
}




/*
 * Set panes and mark them as split.With extra options.
 *
 * void
 * worksheet_split_panes_opt(lxw_worksheet *self,
 *    double y_split, double x_split,
 *    lxw_row_t top_row, lxw_col_t left_col)
 *
 */
ZH_FUNC( WORKSHEET_SPLIT_PANES_OPT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   double y_split = zh_parnd( 2 ) ;
   double x_split = zh_parnd( 3 ) ;
   lxw_row_t top_row = zh_parni( 4 ) ;
   lxw_col_t left_col = zh_parni( 5 ) ;

   worksheet_split_panes_opt( self, y_split, x_split, top_row, left_col); 
}




/*
 * Set panes and mark them as split.
 *
 * void
 * worksheet_split_panes(lxw_worksheet *self, double y_split, double x_split)
 *
 */
ZH_FUNC( WORKSHEET_SPLIT_PANES )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   double y_split = zh_parnd( 2 ) ;
   double x_split = zh_parnd( 3 ) ;

   worksheet_split_panes( self, y_split, x_split); 
}




/*
 * Set the page orientation as portrait.
 *
 * void
 * worksheet_set_portrait(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_SET_PORTRAIT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_set_portrait( self ); 
}




/*
 * Set the page orientation as landscape.
 *
 * void
 * worksheet_set_landscape(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_SET_LANDSCAPE )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_set_landscape( self ); 
}




/*
 * Set the page view mode for Mac Excel.
 *
 * void
 * worksheet_set_page_view(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_SET_PAGE_VIEW )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_set_page_view( self ); 
}




/*
 * Set the paper type. Example. 1 = US Letter, 9 = A4
 *
 * void
 * worksheet_set_paper(lxw_worksheet *self, uint8_t paper_size)
 *
 */
ZH_FUNC( WORKSHEET_SET_PAPER )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   uint8_t paper_size = zh_parni( 2 ) ;

   worksheet_set_paper( self, paper_size); 
}




/*
 * Set the order in which pages are printed.
 *
 * void
 * worksheet_print_across(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_PRINT_ACROSS )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_print_across( self ); 
}




/*
 * Set all the page margins in inches.
 *
 * void
 * worksheet_set_margins(lxw_worksheet *self, double left, double right,
 *    double top, double bottom)
 *
 */
ZH_FUNC( WORKSHEET_SET_MARGINS )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   double left = zh_parnd( 2 ) ;
   double right = zh_parnd( 3 ) ;
   double top = zh_parnd( 4 ) ;
   double bottom = zh_parnd( 5 ) ;

   worksheet_set_margins( self, left, right, top, bottom ); 
}




/*
 * Set the page header caption and options.
 *
 * lxw_error
 * worksheet_set_header_opt(lxw_worksheet *self, const char *string,
 *    lxw_header_footer_options *options)
 *
 */
ZH_FUNC( WORKSHEET_SET_HEADER_OPT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   const char *string = zh_parcx( 2 ) ;
   lxw_header_footer_options *options = zh_parptr(3 ) ;

   zh_retni( worksheet_set_header_opt( self, string, options) ); 
}




/*
 * Set the page footer caption and options.
 *
 * lxw_error
 * worksheet_set_footer_opt(lxw_worksheet *self, const char *string,
 *    lxw_header_footer_options *options)
 *
 */
ZH_FUNC( WORKSHEET_SET_FOOTER_OPT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   const char *string = zh_parcx( 2 ) ;
   lxw_header_footer_options *options = zh_parptr(3 ) ;

   zh_retni( worksheet_set_footer_opt( self, string, options) ); 
}




/*
 * Set the page header caption.
 *
 * lxw_error
 * worksheet_set_header(lxw_worksheet *self, const char *string)
 *
 */
ZH_FUNC( WORKSHEET_SET_HEADER )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   const char *string = zh_parcx( 2 ) ;

   zh_retni( worksheet_set_header( self, string) ); 
}




/*
 * Set the page footer caption.
 *
 * lxw_error
 * worksheet_set_footer(lxw_worksheet *self, const char *string)
 *
 */
ZH_FUNC( WORKSHEET_SET_FOOTER )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   const char *string = zh_parcx( 2 ) ;

   zh_retni( worksheet_set_footer( self, string) ); 
}




/*
 * Set the option to show/hide gridlines on the screen and the printed page.
 *
 * void
 * worksheet_gridlines(lxw_worksheet *self, uint8_t option)
 *
 */
ZH_FUNC( WORKSHEET_GRIDLINES )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   uint8_t option = zh_parni( 2 ) ;

   worksheet_gridlines( self, option ); 
}




/*
 * Center the page horizontally.
 *
 * void
 * worksheet_center_horizontally(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_CENTER_HORIZONTALLY )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_center_horizontally( self ); 
}




/*
 * Center the page horizontally.
 *
 * void
 * worksheet_center_vertically(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_CENTER_VERTICALLY )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_center_vertically( self ); 
}




/*
 * Set the option to print the row and column headers on the printed page.
 *
 * void
 * worksheet_print_row_col_headers(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_PRINT_ROW_COL_HEADERS )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_print_row_col_headers( self ); 
}




/*
 * Set the rows to repeat at the top of each printed page.
 *
 * lxw_error
 * worksheet_repeat_rows(lxw_worksheet *self, lxw_row_t first_row,
 *    lxw_row_t last_row)
 *
 */
ZH_FUNC( WORKSHEET_REPEAT_ROWS )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t first_row = zh_parni( 2 ) ;
   lxw_row_t last_row = zh_parni( 3 ) ;

   zh_retni( worksheet_repeat_rows( self, first_row, last_row) ); 
}




/*
 * Set the columns to repeat at the left hand side of each printed page.
 *
 * lxw_error
 * worksheet_repeat_columns(lxw_worksheet *self, lxw_col_t first_col,
 *    lxw_col_t last_col)
 *
 */
ZH_FUNC( WORKSHEET_REPEAT_COLUMNS )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_col_t first_col = zh_parni( 2 ) ;
   lxw_col_t last_col = zh_parni( 3 ) ;

   zh_retni( worksheet_repeat_columns( self, first_col, last_col) ); 
}




/*
 * Set the print area in the current worksheet.
 *
 * lxw_error
 * worksheet_print_area(lxw_worksheet *self, lxw_row_t first_row,
 *    lxw_col_t first_col, lxw_row_t last_row,
 *       lxw_col_t last_col)
 *
 */
ZH_FUNC( WORKSHEET_PRINT_AREA )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t first_row = zh_parni( 2 ) ;
   lxw_col_t first_col = zh_parni( 3 ) ;
   lxw_row_t last_row = zh_parni( 4 ) ;
   lxw_col_t last_col = zh_parni( 5 ) ;

   zh_retni( worksheet_print_area( self, first_row, first_col, last_row, last_col) ); 
}




/*
 * maximum area printed.
 *
 * void
 * worksheet_fit_to_pages(lxw_worksheet *self, uint16_t width, uint16_t height)
 *
 */
ZH_FUNC( WORKSHEET_FIT_TO_PAGES )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   uint16_t width = zh_parnl( 2 ) ;
   uint16_t height = zh_parnl( 3 ) ;

   worksheet_fit_to_pages( self, width, height ); 
}




/*
 * Set the start page number.
 *
 * void
 * worksheet_set_start_page(lxw_worksheet *self, uint16_t start_page)
 *
 */
ZH_FUNC( WORKSHEET_SET_START_PAGE )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   uint16_t start_page = zh_parnl( 2 ) ;

   worksheet_set_start_page( self, start_page ); 
}




/*
 * Set the scale factor for the printed page.
 *
 * void
 * worksheet_set_print_scale(lxw_worksheet *self, uint16_t scale)
 *
 */
ZH_FUNC( WORKSHEET_SET_PRINT_SCALE )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   uint16_t scale = zh_parnl( 2 ) ;

   worksheet_set_print_scale( self, scale ); 
}




/*
 * Store the horizontal page breaks on a worksheet.
 *
 * lxw_error
 * worksheet_set_h_pagebreaks(lxw_worksheet *self, lxw_row_t hbreaks[])
 *
 */
/*
ZH_FUNC( WORKSHEET_SET_H_PAGEBREAKS )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t hbreaks[] = zh_parni( 2 ) ;

   zh_retni( worksheet_set_h_pagebreaks( self, hbreaks[] ); 
}
*/




/*
 * Store the vertical page breaks on a worksheet.
 *
 * lxw_error
 * worksheet_set_v_pagebreaks(lxw_worksheet *self, lxw_col_t vbreaks[])
 *
 */
/*
ZH_FUNC( WORKSHEET_SET_V_PAGEBREAKS )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_col_t vbreaks[] = zh_parni( 2 ) ;

   zh_retni( worksheet_set_v_pagebreaks( self, vbreaks[] ) ); 
}
*/



/*
 * Set the worksheet zoom factor.
 *
 * void
 * worksheet_set_zoom(lxw_worksheet *self, uint16_t scale)
 *
 */
ZH_FUNC( WORKSHEET_SET_ZOOM )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   uint16_t scale = zh_parnl( 2 ) ;

   worksheet_set_zoom( self, scale ); 
}




/*
 * Hide cell zero values.
 *
 * void
 * worksheet_hide_zero(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_HIDE_ZERO )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_hide_zero( self ); 
}




/*
 * Display the worksheet right to left for some eastern versions of Excel.
 *
 * void
 * worksheet_right_to_left(lxw_worksheet *self)
 *
 */
ZH_FUNC( WORKSHEET_RIGHT_TO_LEFT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;

   worksheet_right_to_left( self ); 
}




/*
 * Set the color of the worksheet tab.
 *
 * void
 * worksheet_set_tab_color(lxw_worksheet *self, lxw_color_t color)
 *
 */
ZH_FUNC( WORKSHEET_SET_TAB_COLOR )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_color_t color = zh_parnl(2 ) ;

   worksheet_set_tab_color( self, color ); 
}




/*
 * Set the worksheet protection flags to prevent modification of worksheet
 * objects.
 *
 * void
 * worksheet_protect(lxw_worksheet *self, const char *password,
 *    lxw_protection *options)
 *
 */
ZH_FUNC( WORKSHEET_PROTECT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   const char *password = zh_parcx( 2 ) ;
   lxw_protection *options = zh_parptr(3 ) ;

   worksheet_protect( self, password, options ); 
}




/*
 * Set the worksheet properties for outlines and grouping.
 *
 * void
 * worksheet_outline_settings(lxw_worksheet *self,
 *    uint8_t visible,
 *    uint8_t symbols_below,
 *    uint8_t symbols_right, uint8_t auto_style)
 */
ZH_FUNC( WORKSHEET_OUTLINE_SETTINGS )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   uint8_t visible = zh_parni( 2 ) ;
   uint8_t symbols_below = zh_parni( 3 ) ;
   uint8_t symbols_right = zh_parni( 4 ) ;
   uint8_t auto_style = zh_parni( 5 ) ;

   worksheet_outline_settings( self, visible, symbols_below, symbols_right, auto_style ); 
}




/*
 * Set the default row properties
 *
 * void
 * worksheet_set_default_row(lxw_worksheet *self, double height,
 *     uint8_t hide_unused_rows)
 */
ZH_FUNC( WORKSHEET_SET_DEFAULT_ROW )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   double height = zh_parnd( 2 ) ;
   uint8_t hide_unused_rows = zh_parni( 3 ) ;

   worksheet_set_default_row( self, height, hide_unused_rows ); 
}




/*
 * Insert an image into the worksheet.
 *
 * lxw_error
 * worksheet_insert_image_opt(lxw_worksheet *self,
 *      lxw_row_t row_num, lxw_col_t col_num,
 *      const char *filename,
 *      lxw_image_options *user_options)
 */
ZH_FUNC( WORKSHEET_INSERT_IMAGE_OPT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   lxw_col_t col_num = zh_parni( 3 ) ;
   const char *filename = zh_parcx( 4 ) ;
   lxw_image_options *user_options = zh_parptr(5 ) ;

   zh_retni( worksheet_insert_image_opt( self, row_num, col_num, filename, user_options ) ); 
}




/*
 * Insert an image into the worksheet.
 *
 * lxw_error
 * worksheet_insert_image(lxw_worksheet *self,
 *    lxw_row_t row_num, lxw_col_t col_num,
 *    const char *filename)
 */
ZH_FUNC( WORKSHEET_INSERT_IMAGE )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   lxw_col_t col_num = zh_parni( 3 ) ;
   const char *filename = zh_parcx( 4 ) ;

   zh_retni( worksheet_insert_image( self, row_num, col_num, filename ) ); 
}



/*
lxw_error
worksheet_insert_image_buffer(lxw_worksheet *self,
         lxw_row_t row_num,
         lxw_col_t col_num,
         const unsigned char *image_buffer,
         size_t image_size)
*/
/*
ZH_FUNC( WORKSHEET_INSERT_IMAGE_BUFFER )
{ 
   lxw_worksheet *self = zh_parptr( 1 );
   lxw_row_t row_num   = zh_parni( 2 );
   lxw_col_t col_num   = zh_parni( 3 );
   const unsigned char *image_buffer = zh_parcx( 4 );
   size_t image_size   = zh_parni( 4 );

   zh_retni( worksheet_insert_image_buffer( self, row_num, col_num, image_buffer, image_size ) );
}
*/



/*
 * Insert an chart into the worksheet.
 *
 * lxw_error
 * worksheet_insert_chart_opt(lxw_worksheet *self,
 *       lxw_row_t row_num, lxw_col_t col_num,
 *       lxw_chart *chart, lxw_image_options *user_options)
 *
 */
ZH_FUNC( WORKSHEET_INSERT_CHART_OPT )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   lxw_col_t col_num = zh_parni( 3 ) ;
   lxw_chart *chart = zh_parptr(4 ) ;
   lxw_image_options *user_options = zh_parptr(5 ) ;

   zh_retni( worksheet_insert_chart_opt( self, row_num, col_num, chart, user_options ) ); 
}




/*
 * Insert an image into the worksheet.
 *
 * lxw_error
 * worksheet_insert_chart(lxw_worksheet *self,
 *      lxw_row_t row_num, lxw_col_t col_num, lxw_chart *chart)
 *
 */
ZH_FUNC( WORKSHEET_INSERT_CHART )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t row_num = zh_parni( 2 ) ;
   lxw_col_t col_num = zh_parni( 3 ) ;
   lxw_chart *chart = zh_parptr(4 ) ;

   zh_retni( worksheet_insert_chart( self, row_num, col_num, chart) ); 
}




/*
 * Add a data validation to a worksheet, for a range. Ironically this requires
 * a lot of validation of the user input.
 *
 * lxw_error
 * worksheet_data_validation_range(lxw_worksheet *self, lxw_row_t first_row,
 *    lxw_col_t first_col,
 *    lxw_row_t last_row,
 *    lxw_col_t last_col,
 *    lxw_data_validation *validation)
 */
ZH_FUNC( WORKSHEET_DATA_VALIDATION_RANGE )
{ 
   lxw_worksheet *self = zh_parptr( 1 ) ;
   lxw_row_t first_row = zh_parni( 2 ) ;
   lxw_col_t first_col = zh_parni( 3 ) ;
   lxw_row_t last_row = zh_parni( 4 ) ;
   lxw_col_t last_col = zh_parni( 5 ) ;
   lxw_data_validation *validation = zh_parptr(6 ) ;

   zh_retni( worksheet_data_validation_range( self, first_row, first_col, last_row, last_col, validation ) ); 
}


/*
 * Add a data validation to a worksheet, for a cell.
 *
 * lxw_error
 * worksheet_data_validation_cell(lxw_worksheet *self, lxw_row_t row,
 *                                lxw_col_t col, lxw_data_validation *validation)
 *
 */
/*
ZH_FUNC( WORKSHEET_DATA_VALIDATION_CELL )
{
   lxw_worksheet *self = zh_parptr( 1 );
   lxw_row_t row = zh_parni( 2 );
   lxw_col_t col = zh_parni( 3 );
   lxw_data_validation *validation = zh_parptr( 4 );

   zh_retni( worksheet_data_validation_range(self, row, col,
                                           row, col, validation));
}
*/
ZH_FUNC( WORKSHEET_DATA_VALIDATION_CELL )
{
   lxw_worksheet *self = zh_parptr( 1 );
   lxw_row_t row = zh_parni( 2 );
   lxw_col_t col = zh_parni( 3 );
   PZH_ITEM pHash = zh_param( 4, ZH_IT_HASH );

   lxw_data_validation *validation = (lxw_data_validation *) zh_xalloc( sizeof(lxw_data_validation) ); 
   if( validation == NULL )
   {
      zh_errRT_BASE( EG_MEM, 0, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
   }
   memset( validation, 0, sizeof( lxw_data_validation) );

   ZH_SIZE nLen = zh_hashLen( pHash ), nPos = 0;

   if( pHash )
   {
      while( ++nPos <= nLen )
      {
         PZH_ITEM pKey = zh_hashGetKeyAt( pHash, nPos );
         PZH_ITEM pValue = zh_hashGetValueAt( pHash, nPos );
         if( pKey && pValue )
         {
            char *key = (char *)zh_itemGetC( pKey );
            if( ZH_IS_NUMERIC( pValue ) )
            {
               if( ZH_IS_NUMINT( pValue ) ){
                  ZH_MAXINT value = zh_itemGetNInt( pValue );
                  if( zh_stricmp( key, "validate" ) == 0 ){
                     validation->validate = value;
                  }
                  else if( zh_stricmp( key, "criteria" ) == 0 ){
                     validation->criteria = value;
                  }
                  else if( zh_stricmp( key, "ignore_blank" ) == 0 ){
                     validation->ignore_blank = value;
                  }
                  else if( zh_stricmp( key, "show_input" ) == 0 ){
                     validation->show_input = value;
                  }
                  else if( zh_stricmp( key, "show_error" ) == 0 ){
                     validation->show_error = value;
                  }
                  else if( zh_stricmp( key, "error_type" ) == 0 ){
                     validation->error_type = value;
                  }
                  else if( zh_stricmp( key, "dropdown" ) == 0 ){
                     validation->dropdown = value;
                  }
                  //else if( zh_stricmp( key, "is_between" ) == 0 ){
                  //   validation->is_between = value;
                  //}
                  else if( zh_stricmp( key, "value_number" ) == 0 ){
                     validation->value_number = value;
                  }
                  else if( zh_stricmp( key, "minimum_number" ) == 0 ){
                     validation->minimum_number = value;
                  }
                  else if( zh_stricmp( key, "maximum_number" ) == 0 ){
                     validation->maximum_number = value;
                  }
               }
               else if( ZH_IS_NUMERIC( pValue ) || ZH_IS_DOUBLE( pValue ) )
               {
                  double value = zh_itemGetND( pValue );
                  if( zh_stricmp( key, "value_number" ) == 0 ){
                     validation->value_number = value;
                  }
                  else if( zh_stricmp( key, "minimum_number" ) == 0 ){
                     validation->minimum_number = value;
                  }
                  else if( zh_stricmp( key, "maximum_number" ) == 0 ){
                     validation->maximum_number = value;
                  }
              }
          }
          else if( ZH_IS_STRING( pValue ) )
          {
             char *value = (char *) zh_itemGetC( pValue );
             if( zh_stricmp( key, "minimum_formula" ) == 0 ){
                validation->minimum_formula = value;
             }
             else if( zh_stricmp( key, "value_formula" ) == 0 ){
                validation->value_formula = value;
             }
             else if( zh_stricmp( key, "minimum_formula" ) == 0 ){
                validation->minimum_formula = value;
             }
             else if( zh_stricmp( key, "maximum_formula" ) == 0 ){
                validation->maximum_formula = value;
             }
             else if( zh_stricmp( key, "input_title" ) == 0 ){
                validation->input_title = value;
             }
             else if( zh_stricmp( key, "input_message" ) == 0 ){
                validation->input_message = value;
             }
             else if( zh_stricmp( key, "error_title" ) == 0 ){
                validation->error_title = value;
             }
             else if( zh_stricmp( key, "error_message" ) == 0 ){
                validation->error_message = value;
             }
          }
          else if( ZH_IS_DATETIME( pValue ) )
          {
             lxw_datetime datetime;
             long lDate, lTime;  

             if( zh_itemGetTDT( pValue, &lDate, &lTime ) )
             {
                int iYear, iMonth, iDay ;
                int iHour, iMin, iSec, iMSec ;

                zh_timeDecode( lTime, &iHour, &iMin, &iSec, &iMSec );
                zh_dateDecode( lDate, &iYear, &iMonth, &iDay );

                datetime.year = iYear;
                datetime.month = iMonth;
                datetime.day = iDay;
                datetime.hour = iHour;
                datetime.min = iMin;
                datetime.sec = iSec;

                if( zh_stricmp( key, "value_datetime" ) == 0 ){
                   validation->value_datetime = datetime;
                }
                else if( zh_stricmp( key, "minimum_datetime" ) == 0 ){
                   validation->minimum_datetime = datetime;
                }
                else if( zh_stricmp( key, "maximum_datetime" ) == 0 ){
                   validation->maximum_datetime = datetime;
                }
             }
          }
          else if( ZH_IS_ARRAY( pValue ) )
          {
             ZH_SIZE nLen = zh_itemSize( pValue );
             if( nLen )
             {
                validation->value_list = (char **) zh_xalloc( sizeof( char* ) * (nLen+1) );
                if( validation->value_list == NULL )
                {
                   zh_errRT_BASE( EG_MEM, 0, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
                }
                else
                {
                     memset( validation->value_list, 0, sizeof( char * ) * ( nLen+1 ) );
                     ZH_SIZE nIndex = 0;
                     while( nIndex<nLen ){
                        validation->value_list[ nIndex ] = zh_arrayGetC( pValue, nIndex+1 );
                        nIndex++ ;
                     }
                  }
               }
            }
         }
      }
      if( validation ){
         zh_retni( worksheet_data_validation_range(self, row, col,
                                                   row, col, validation));
         ZH_SIZE nIndex = 0;
         if( validation->value_list ){
            while( validation->value_list[nIndex] )
            {
               zh_xfree( validation->value_list[nIndex] );
               nIndex++;
            }
         }
         zh_xfree( validation );
      }
   }

}


//eof
