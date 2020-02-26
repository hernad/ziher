/*****************************************************************************
 * core - A library for creating Excel XLSX core files.
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

#include "xlsxwriter/xmlwriter.h"
#include "xlsxwriter/core.h"
#include "xlsxwriter/utility.h"

#include "zh_api.h"



/*****************************************************************************
 *
 * Private functions.
 *
 ****************************************************************************/



/*
 * Create a new core object.
 */
/*
lxw_core *
lxw_core_new(void)
*/
ZH_FUNC( LXW_CORE_NEW )
{ 
   zh_retptr( lxw_core_new() ); 
}





/*
 * Free a core object.
 */
/*
void
lxw_core_free(lxw_core *core)
*/
ZH_FUNC( LXW_CORE_FREE )
{ 
   lxw_core *core = zh_parptr( 1 ) ;

   lxw_core_free(core) ; 
}





/*
 * Convert a time_t struct to a ISO 8601 style "2010-01-01T00:00:00Z" date.
 */
/*
static void
_datetime_to_iso8601_date(time_t *timer, char *str, size_t size)
*/
/*
(RIGC 2019-05-28)  no se como manipular el tipo time_t
ZH_FUNC( _DATETIME_TO_ISO8601_DATE )
{ 
   time_t *timer = zh_parXX( 1 ) ;
   char *str = zh_parcx( 2 ) ;
   size_t size = zh_parXX( 3 ) ;

   zh_retXX( _datetime_to_iso8601_date(timer, str, size) ); 
}
*/




/*****************************************************************************
 *
 * XML file assembly functions.
 *
 ****************************************************************************/


/*
 * Assemble and write the XML file.
 */
/*
void
lxw_core_assemble_xml_file(lxw_core *self)
*/
ZH_FUNC( LXW_CORE_ASSEMBLE_XML_FILE )
{ 
   lxw_core *self = zh_parptr( 1 ) ;

   lxw_core_assemble_xml_file(self) ; 
}


//eof
