/*****************************************************************************
 * custom - A library for creating Excel custom property files.
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
#include "xlsxwriter/custom.h"
#include "xlsxwriter/utility.h"

#include "zh_api.h"


/*****************************************************************************
 *
 * Private functions.
 *
 ****************************************************************************/

/*
 * Create a new custom object.
 */
/*
lxw_custom *
lxw_custom_new(void)
*/
ZH_FUNC( LXW_CUSTOM_NEW )
{ 
   zh_retptr( lxw_custom_new() ); 
}





/*
 * Free a custom object.
 */
/*
void
lxw_custom_free(lxw_custom *custom)
*/
ZH_FUNC( LXW_CUSTOM_FREE )
{ 
   lxw_custom *custom = zh_parptr( 1 ) ;

   lxw_custom_free(custom) ; 
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
lxw_custom_assemble_xml_file(lxw_custom *self)
*/
ZH_FUNC( LXW_CUSTOM_ASSEMBLE_XML_FILE )
{ 
   lxw_custom *self = zh_parptr( 1 ) ;

   lxw_custom_assemble_xml_file(self) ; 
}


//eof
