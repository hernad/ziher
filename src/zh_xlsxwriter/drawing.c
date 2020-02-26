/*****************************************************************************
 * drawing - A library for creating Excel XLSX drawing files.
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
#include "xlsxwriter/common.h"
#include "xlsxwriter/drawing.h"
#include "xlsxwriter/utility.h"

#include "zh_api.h"


/*****************************************************************************
 *
 * Private functions.
 *
 ****************************************************************************/


/*
 * Create a new drawing collection.
 */
/*
lxw_drawing *
lxw_drawing_new(void)
*/
ZH_FUNC( LXW_DRAWING_NEW )
{ 
   zh_retptr( lxw_drawing_new() ); 
}





/*
 * Free a drawing object.
 */
/*
void
lxw_free_drawing_object(lxw_drawing_object *drawing_object)
*/
ZH_FUNC( LXW_FREE_DRAWING_OBJECT )
{ 
   lxw_drawing_object *drawing_object = zh_parptr( 1 ) ;

   lxw_free_drawing_object(drawing_object) ; 
}





/*
 * Free a drawing collection.
 */
/*
void
lxw_drawing_free(lxw_drawing *drawing)
*/
ZH_FUNC( LXW_DRAWING_FREE )
{ 
   lxw_drawing *drawing = zh_parptr( 1 ) ;

   lxw_drawing_free(drawing) ; 
}





/*
 * Add a drawing object to the drawing collection.
 */
/*
void
lxw_add_drawing_object(lxw_drawing *drawing,
              lxw_drawing_object *drawing_object)
*/
ZH_FUNC( LXW_ADD_DRAWING_OBJECT )
{ 
   lxw_drawing *drawing = zh_parptr( 1 ) ;
   lxw_drawing_object *drawing_object = zh_parptr( 2 ) ;

   lxw_add_drawing_object(drawing, drawing_object) ; 
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
lxw_drawing_assemble_xml_file(lxw_drawing *self)
*/
ZH_FUNC( LXW_DRAWING_ASSEMBLE_XML_FILE )
{ 
   lxw_drawing *self = zh_parptr( 1 ) ;

   lxw_drawing_assemble_xml_file(self) ; 
}


//eof
