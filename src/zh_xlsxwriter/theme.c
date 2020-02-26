/*****************************************************************************
 * theme - A library for creating Excel XLSX theme files.
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

#include <string.h>

#include "xlsxwriter/xmlwriter.h"
#include "xlsxwriter/theme.h"
#include "xlsxwriter/utility.h"

#include "zh_api.h"


/*****************************************************************************
 *
 * Private functions.
 *
 ****************************************************************************/

/*
 * Create a new theme object.
 */
/*
lxw_theme *
lxw_theme_new(void)
*/
ZH_FUNC( LXW_THEME_NEW )
{ 
   zh_retptr( lxw_theme_new() ); 
}





/*
 * Free a theme object.
 */
/*
void
lxw_theme_free(lxw_theme *theme)
*/
ZH_FUNC( LXW_THEME_FREE )
{ 
   lxw_theme *theme = zh_parptr( 1 ) ;

   lxw_theme_free(theme) ; 
}





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
lxw_theme_assemble_xml_file(lxw_theme *self)
*/
ZH_FUNC( LXW_THEME_ASSEMBLE_XML_FILE )
{ 
   lxw_theme *self = zh_parptr( 1 ) ;

   lxw_theme_assemble_xml_file(self) ; 
}


//eof
