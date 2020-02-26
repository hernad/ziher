/*****************************************************************************
 * styles - A library for creating Excel XLSX styles files.
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
#include "xlsxwriter/styles.h"
#include "xlsxwriter/utility.h"

#include "zh_api.h"



/*****************************************************************************
 *
 * Private functions.
 *
 ****************************************************************************/

/*
 * Create a new styles object.
 */
/*
lxw_styles *
lxw_styles_new(void)
*/
ZH_FUNC( LXW_STYLES_NEW )
{ 
   zh_retptr( lxw_styles_new() ); 
}





/*
 * Free a styles object.
 */
/*
void
lxw_styles_free(lxw_styles *styles)
*/
ZH_FUNC( LXW_STYLES_FREE )
{ 
   lxw_styles *styles = zh_parptr( 1 ) ;

   lxw_styles_free(styles) ; 
}





/*
 * Write the <t> element for rich strings.
 */
/*
void
lxw_styles_write_string_fragment(lxw_styles *self, char *string)
*/
/*
ZH_FUNC( LXW_STYLES_WRITE_STRING_FRAGMENT )
{ 
   lxw_styles *self = zh_parptr( 1 ) ;
   char *string = zh_parcx( 2 ) ; // No pasa.. (RIGC - 2019/05/28)

   lxw_styles_write_string_fragment(self, string) ; 
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
lxw_styles_assemble_xml_file(lxw_styles *self)
*/
ZH_FUNC( LXW_STYLES_ASSEMBLE_XML_FILE )
{ 
   lxw_styles *self = zh_parptr( 1 ) ;

   lxw_styles_assemble_xml_file(self) ; 
}


//eof
