/*
 * The cache for identifiers
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 * (or visit their website at https://www.gnu.org/licenses/).
 *
 */

#include <string.h>
#include "zh_comp.h"

#define ZH_IDENT_TABLE_SIZE  509UL

/* create a new identifier or return the existing one
 */
const char * zh_compIdentifierNew( ZH_COMP_DECL, const char * szName, int iType )
{
   const char * szIdent;

   szIdent = ( const char * ) zh_hashTableFind( ZH_COMP_PARAM->pIdentifiers,
                                                ( const void * ) szName );
   if( ! szIdent )
   {
      /*
       * In the future we may add direct support for static identifiers
       * so it will not be necessary to allocate separate buffer for them
       */
      if( iType == ZH_IDENT_COPY || iType == ZH_IDENT_STATIC )
         szIdent = zh_strdup( szName );
      else
         szIdent = szName;

      zh_hashTableAdd( ZH_COMP_PARAM->pIdentifiers,
                       ( const void * ) szIdent, ( const void * ) szIdent );
   }
   else if( iType == ZH_IDENT_FREE )
      zh_xfree( ZH_UNCONST( szName ) );

   return szIdent;
}

/* returns a hash key */
static ZH_HASH_FUNC( zh_comp_IdentKey )    /* ZH_SIZE func (void *Value, void *Cargo) */
{
   ZH_SIZE nSum = 0;
   const char * szName = ( const char * ) Value;

   while( *szName )
      nSum += *szName++;

   ZH_SYMBOL_UNUSED( HashPtr );
   ZH_SYMBOL_UNUSED( Cargo );

   return nSum % ZH_IDENT_TABLE_SIZE;
}

/* deletes an identifier */
static ZH_HASH_FUNC( zh_comp_IdentDel )
{
   zh_xfree( ZH_UNCONST( Value ) );
   ZH_SYMBOL_UNUSED( HashPtr );
   ZH_SYMBOL_UNUSED( Cargo );
   return 1;
}

/* compares two identifiers */
static ZH_HASH_FUNC( zh_comp_IdentComp )
{
   ZH_SYMBOL_UNUSED( HashPtr );
   return strcmp( ( const char * ) Value, ( const char * ) Cargo );
}

/* initialize the hash table for identifiers */
void zh_compIdentifierOpen( ZH_COMP_DECL )
{
   ZH_COMP_PARAM->pIdentifiers = zh_hashTableCreate( ZH_IDENT_TABLE_SIZE,
                     zh_comp_IdentKey, zh_comp_IdentDel, zh_comp_IdentComp );
}

/* release identifiers table */
void zh_compIdentifierClose( ZH_COMP_DECL )
{
   if( ZH_COMP_PARAM->pIdentifiers )
   {
      zh_hashTableKill( ZH_COMP_PARAM->pIdentifiers );
      ZH_COMP_PARAM->pIdentifiers = NULL;
   }
}
