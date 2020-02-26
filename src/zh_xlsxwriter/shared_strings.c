/*****************************************************************************
 * shared_strings - A library for creating Excel XLSX sst files.
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
#include "xlsxwriter/shared_strings.h"
#include "xlsxwriter/utility.h"
#include <ctype.h>

#include "zh_api.h"




/*****************************************************************************
 *
 * Private functions.
 *
 ****************************************************************************/

/*
 * Create a new SST SharedString object.
 */
/*
lxw_sst *
lxw_sst_new(void)
*/
ZH_FUNC( LXW_SST_NEW )
{ 
   zh_retptr( lxw_sst_new() ); 
}





/*
 * Free a SST SharedString table object.
 */
/*
void
lxw_sst_free(lxw_sst *sst)
*/
ZH_FUNC( LXW_SST_FREE )
{ 
   lxw_sst *sst = zh_parptr( 1 ) ;

   lxw_sst_free(sst) ; 
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
lxw_sst_assemble_xml_file(lxw_sst *self)
*/
ZH_FUNC( LXW_SST_ASSEMBLE_XML_FILE )
{ 
   lxw_sst *self = zh_parptr( 1 ) ;

   lxw_sst_assemble_xml_file(self) ; 
}



//eof
