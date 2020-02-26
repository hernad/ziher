/*****************************************************************************
 * relationships - A library for creating Excel XLSX relationships files.
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
#include "xlsxwriter/relationships.h"
#include "xlsxwriter/utility.h"

#include "zh_api.h"



/*****************************************************************************
 *
 * Private functions.
 *
 ****************************************************************************/

/*
 * Create a new relationships object.
 */
/*
lxw_relationships *
lxw_relationships_new(void)
*/
ZH_FUNC( LXW_RELATIONSHIPS_NEW )
{ 
   zh_retptr( lxw_relationships_new() ); 
}





/*
 * Free a relationships object.
 */
/*
void
lxw_free_relationships(lxw_relationships *rels)
*/
ZH_FUNC( LXW_FREE_RELATIONSHIPS )
{ 
   lxw_relationships *rels = zh_parptr( 1 ) ;

   lxw_free_relationships(rels) ; 
}





/*****************************************************************************
 *
 * XML functions.
 *
 ****************************************************************************/






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
lxw_relationships_assemble_xml_file(lxw_relationships *self)
*/
ZH_FUNC( LXW_RELATIONSHIPS_ASSEMBLE_XML_FILE )
{ 
   lxw_relationships *self = zh_parptr( 1 ) ;

   lxw_relationships_assemble_xml_file(self) ; 
}





/*****************************************************************************
 *
 * Public functions.
 *
 ****************************************************************************/






/*
 * Add a document relationship to XLSX .rels xml files.
 */
/*
void
lxw_add_document_relationship(lxw_relationships *self, const char *type,
           const char *target)
*/
ZH_FUNC( LXW_ADD_DOCUMENT_RELATIONSHIP )
{ 
   lxw_relationships *self = zh_parptr( 1 ) ;
   const char *type = zh_parcx( 2 ) ;
   const char *target = zh_parcx( 3 ) ;

   lxw_add_document_relationship(self, type, target) ; 
}





/*
 * Add a package relationship to XLSX .rels xml files.
 */
/*
void
lxw_add_package_relationship(lxw_relationships *self, const char *type,
             const char *target)
*/
ZH_FUNC( LXW_ADD_PACKAGE_RELATIONSHIP )
{ 
   lxw_relationships *self = zh_parptr( 1 ) ;
   const char *type = zh_parcx( 2 ) ;
   const char *target = zh_parcx( 3 ) ;

   lxw_add_package_relationship(self, type, target) ; 
}





/*
 * Add a MS schema package relationship to XLSX .rels xml files.
 */
/*
void
lxw_add_ms_package_relationship(lxw_relationships *self, const char *type,
                  const char *target)
*/
ZH_FUNC( LXW_ADD_MS_PACKAGE_RELATIONSHIP )
{ 
   lxw_relationships *self = zh_parptr( 1 ) ;
   const char *type = zh_parcx( 2 ) ;
   const char *target = zh_parcx( 3 ) ;

   lxw_add_ms_package_relationship(self, type, target) ; 
}


//eof
