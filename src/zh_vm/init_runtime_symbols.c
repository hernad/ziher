/*
 * Forces initialization of runtime support symbols
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Ziher Project gives permission for
 * additional uses of the text contained in its release of Ziher.
 *
 * The exception is that, if you link the Ziher libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Ziher library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Ziher
 * Project under the name Ziher.  If you copy code from other
 * Ziher Project or Free Software Foundation releases into a copy of
 * Ziher, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Ziher, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "zh_api.h"
#include "zh_vm.h"

ZH_FUNC_EXTERN( AADD );
ZH_FUNC_EXTERN( ABS );
ZH_FUNC_EXTERN( ASC );
ZH_FUNC_EXTERN( AT );
ZH_FUNC_EXTERN( BOF );
ZH_FUNC_EXTERN( BREAK );
ZH_FUNC_EXTERN( CDOW );
ZH_FUNC_EXTERN( CHR );
ZH_FUNC_EXTERN( CMONTH );
ZH_FUNC_EXTERN( COL );
ZH_FUNC_EXTERN( CTOD );
ZH_FUNC_EXTERN( DATE );
ZH_FUNC_EXTERN( DAY );
ZH_FUNC_EXTERN( DELETED );
ZH_FUNC_EXTERN( DEVPOS );
ZH_FUNC_EXTERN( DEVOUT );
ZH_FUNC_EXTERN( DOW );
ZH_FUNC_EXTERN( DTOC );
ZH_FUNC_EXTERN( DTOS );
ZH_FUNC_EXTERN( EMPTY );
ZH_FUNC_EXTERN( EOF );
ZH_FUNC_EXTERN( EXP );
ZH_FUNC_EXTERN( FCOUNT );
ZH_FUNC_EXTERN( FIELDNAME );
ZH_FUNC_EXTERN( FLOCK );
ZH_FUNC_EXTERN( FOUND );
ZH_FUNC_EXTERN( INKEY );
ZH_FUNC_EXTERN( INT );
ZH_FUNC_EXTERN( LASTREC );
ZH_FUNC_EXTERN( LEFT );
ZH_FUNC_EXTERN( LEN );
ZH_FUNC_EXTERN( LOCK );
ZH_FUNC_EXTERN( LOG );
ZH_FUNC_EXTERN( LOWER );
ZH_FUNC_EXTERN( LTRIM );
ZH_FUNC_EXTERN( MAX );
ZH_FUNC_EXTERN( MIN );
ZH_FUNC_EXTERN( MONTH );
ZH_FUNC_EXTERN( PCOL );
ZH_FUNC_EXTERN( PCOUNT );
ZH_FUNC_EXTERN( PROW );
ZH_FUNC_EXTERN( RECCOUNT );
ZH_FUNC_EXTERN( RECNO );
ZH_FUNC_EXTERN( REPLICATE );
ZH_FUNC_EXTERN( RLOCK );
ZH_FUNC_EXTERN( ROUND );
ZH_FUNC_EXTERN( ROW );
ZH_FUNC_EXTERN( RTRIM );
ZH_FUNC_EXTERN( SECONDS );
ZH_FUNC_EXTERN( SELECT );
ZH_FUNC_EXTERN( SETPOS );
ZH_FUNC_EXTERN( SETPOSBS );
ZH_FUNC_EXTERN( SPACE );
ZH_FUNC_EXTERN( SQRT );
ZH_FUNC_EXTERN( STR );
ZH_FUNC_EXTERN( SUBSTR );
ZH_FUNC_EXTERN( TIME );
ZH_FUNC_EXTERN( TRANSFORM );
ZH_FUNC_EXTERN( TRIM );
ZH_FUNC_EXTERN( TYPE );
ZH_FUNC_EXTERN( UPPER );
ZH_FUNC_EXTERN( VAL );
//ZH_FUNC_EXTERN( WORD );
ZH_FUNC_EXTERN( YEAR );
ZH_FUNC_EXTERN( ERRORBLOCK );
ZH_FUNC_EXTERN( __ZHVMINIT );
ZH_FUNC_EXTERN( BREAK );
ZH_FUNC_EXTERN( ZH_GTCOUNT );
//ZH_FUNC_EXTERN(FUNC_HELLO_ZIHER_2);
ZH_FUNC_EXTERN(READMODAL);
//ZH_FUNC_EXTERN(__GET);
ZH_FUNC_EXTERN(ATAIL);
ZH_FUNC_EXTERN(QOUT);
ZH_FUNC_EXTERN(SCROLL);

//ZH_FUNC_EXTERN(ZHOBJECT_VMINIT);
//ZH_FUNC_EXTERN(ZHCLASS_VMINIT);
//ZH_FUNC_EXTERN(ZHCLASS_VMINIT2);

ZH_FUNC_EXTERN(GET_VMINIT);
ZH_FUNC_EXTERN(__GET_VMINIT);
//ZH_FUNC_EXTERN(ERROR_VMINIT);

ZH_EXTERN_C_ ZH_EXTERN_ ZIHERF ZH_EXPORT zh_vmDoBlock ( void );

ZH_FUNC_EXTERN(ERRORNEW);
ZH_FUNC_EXTERN(ALERT);

ZH_FUNC_EXTERN(ZH_GTALERT);
ZH_FUNC_EXTERN(ZH_DEFAULTVALUE);

ZH_FUNC_EXTERN( SDDPG );

//ZH_FUNC_INITSTATICS( DBGBRWSR_ZH );


PZH_SYMBOL pZhSymEval = NULL;
PZH_SYMBOL pZhSymBreak = NULL;
PZH_SYMBOL pZhSymErrorNew = NULL;

static ZH_SYMBOL symbols[] = {
   { "AADD",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( AADD )      }, NULL },
   { "ALERT",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ALERT )      }, NULL },
   { "ZH_GTALERT", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ZH_GTALERT )  }, NULL },
   { "ABS",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ABS )       }, NULL },
   { "ASC",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ASC )       }, NULL },
   { "AT",        { ZH_FS_PUBLIC }, { ZH_FUNCNAME( AT )        }, NULL },
   { "ATAIL", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ATAIL ) }, NULL },
   { "BOF",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( BOF )       }, NULL },
   { "BREAK",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( BREAK )     }, NULL },

   { "EVAL",  { ZH_FS_PUBLIC }, { zh_vmDoBlock }, NULL }, /* symbol to evaluate codeblocks */
   { "BREAK", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( BREAK ) }, NULL }, /* symbol to generate break */

   { "ERRORNEW", { ZH_FS_PUBLIC | ZH_FS_LOCAL }, { ZH_FUNCNAME( ERRORNEW ) }, NULL },

   { "CDOW",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( CDOW )      }, NULL },
   { "CHR",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( CHR )       }, NULL },
   { "CMONTH",    { ZH_FS_PUBLIC }, { ZH_FUNCNAME( CMONTH )    }, NULL },
   { "COL",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( COL )       }, NULL },
   { "CTOD",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( CTOD )      }, NULL },
   { "DATE",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DATE )      }, NULL },
   { "DAY",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DAY )       }, NULL },
   { "DELETED",   { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DELETED )   }, NULL },
   { "DEVPOS",    { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DEVPOS )    }, NULL },
   { "DEVOUT",    { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DEVOUT ) }, NULL },
   { "DOW",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DOW )       }, NULL },
   { "DTOC",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DTOC )      }, NULL },
   { "DTOS",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( DTOS )      }, NULL },
   { "DISPLAY",   { ZH_FS_PUBLIC | ZH_FS_MESSAGE }, { NULL }, NULL },
   { "EMPTY",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( EMPTY )     }, NULL },
   { "EOF",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( EOF )       }, NULL },
   { "EXP",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( EXP )       }, NULL },
   { "FCOUNT",    { ZH_FS_PUBLIC }, { ZH_FUNCNAME( FCOUNT )    }, NULL },
   { "FIELDNAME", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( FIELDNAME ) }, NULL },
   { "FLOCK",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( FLOCK )     }, NULL },
   { "FOUND",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( FOUND )     }, NULL },
   { "GETLIST",   { ZH_FS_PUBLIC | ZH_FS_MEMVAR }, { NULL }, NULL },
   //{ "__GET",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( __GET ) }, NULL },
   { "INKEY",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( INKEY )     }, NULL },
   { "INT",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( INT )       }, NULL },
   { "LASTREC",   { ZH_FS_PUBLIC }, { ZH_FUNCNAME( LASTREC )   }, NULL },
   { "LEFT",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( LEFT )      }, NULL },
   { "LEN",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( LEN )       }, NULL },
   { "LOCK",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( LOCK )      }, NULL },
   { "LOG",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( LOG )       }, NULL },
   { "LOWER",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( LOWER )     }, NULL },
   { "LTRIM",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( LTRIM )     }, NULL },
   { "MAX",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( MAX )       }, NULL },
   { "MIN",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( MIN )       }, NULL },
   { "MONTH",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( MONTH )     }, NULL },
   { "PCOL",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( PCOL )      }, NULL },
   { "PCOUNT",    { ZH_FS_PUBLIC }, { ZH_FUNCNAME( PCOUNT )    }, NULL },
   { "PROW",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( PROW )      }, NULL },
   { "READMODAL", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( READMODAL ) }, NULL },
   { "RECCOUNT",  { ZH_FS_PUBLIC }, { ZH_FUNCNAME( RECCOUNT )  }, NULL },
   { "RECNO",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( RECNO )     }, NULL },
   { "REPLICATE", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( REPLICATE ) }, NULL },
   { "RLOCK",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( RLOCK )     }, NULL },
   { "ROUND",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ROUND )     }, NULL },
   { "ROW",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ROW )       }, NULL },
   { "RTRIM",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( RTRIM )     }, NULL },
   { "SCROLL",    { ZH_FS_PUBLIC }, { ZH_FUNCNAME( SCROLL )    }, NULL },
   { "SECONDS",   { ZH_FS_PUBLIC }, { ZH_FUNCNAME( SECONDS )   }, NULL },
   { "SELECT",    { ZH_FS_PUBLIC }, { ZH_FUNCNAME( SELECT )    }, NULL },
   { "SETPOS",    { ZH_FS_PUBLIC }, { ZH_FUNCNAME( SETPOS )    }, NULL },
   { "SETPOSBS",  { ZH_FS_PUBLIC }, { ZH_FUNCNAME( SETPOSBS )  }, NULL },
   { "SPACE",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( SPACE )     }, NULL },
   { "SQRT",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( SQRT )      }, NULL },
   { "ZH_DEFAULTVALUE",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ZH_DEFAULTVALUE )      }, NULL },
   { "STR",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( STR )       }, NULL },
   { "SUBSTR",    { ZH_FS_PUBLIC }, { ZH_FUNCNAME( SUBSTR )    }, NULL },
   { "TIME",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( TIME )      }, NULL },
   { "TRANSFORM", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( TRANSFORM ) }, NULL },
   { "TRIM",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( TRIM )      }, NULL },
   { "TYPE",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( TYPE )      }, NULL },
   { "UPPER",     { ZH_FS_PUBLIC }, { ZH_FUNCNAME( UPPER )     }, NULL },
   { "VAL",       { ZH_FS_PUBLIC }, { ZH_FUNCNAME( VAL )       }, NULL },
   { "QOUT",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( QOUT )      }, NULL },
   { "YEAR",      { ZH_FS_PUBLIC }, { ZH_FUNCNAME( YEAR )      }, NULL },
   { "ERRORBLOCK",   { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ERRORBLOCK ) }, NULL },
   { "__ZHVMINIT",   { ZH_FS_PUBLIC }, { ZH_FUNCNAME( __ZHVMINIT ) }, NULL },
   { "ZH_GTCOUNT",   { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ZH_GTCOUNT ) }, NULL },
   //{ "ZHOBJECT_VMINIT",   { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ZHOBJECT_VMINIT ) }, NULL },
   //{ "ZHCLASS_VMINIT",   { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ZHCLASS_VMINIT ) }, NULL },
   //{ "ZHCLASS_VMINIT2",   { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ZHCLASS_VMINIT2 ) }, NULL },
   { "GET_VMINIT",   { ZH_FS_PUBLIC }, { ZH_FUNCNAME( GET_VMINIT ) }, NULL },
   { "__GET_VMINIT",   { ZH_FS_PUBLIC }, { ZH_FUNCNAME( __GET_VMINIT ) }, NULL },

   //{ "ERROR_VMINIT", { ZH_FS_PUBLIC }, { ZH_FUNCNAME( ERROR_VMINIT ) }, NULL },

    { "SDDPG", { ZH_FS_PUBLIC | ZH_FS_LOCAL }, { ZH_FUNCNAME( SDDPG ) }, NULL },

   // { "(_INS2DBGBRWSR)", { ZH_FS_STATIC_INIT | ZH_FS_LOCAL }, { zh_INITSTATICS_DBGBRWSR_ZH }, NULL }

   

};

/* NOTE: The system symbol table with runtime functions HAVE TO be called
         last */

void zh_vmSymbolInit_RT( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_vmSymbolInit_RT()" ) );

   // puts("hernad zhbuiltin");
   //printf("dynsumcount before:%d\n", zh_dynsymCount());
   zh_vmProcessSymbols( symbols, ZH_SIZEOFARRAY( symbols ), "ZHBUILTIN", 0, 0 );
   //printf("dynsumcount after:%d\n", zh_dynsymCount());

   pZhSymEval = zh_dynsymFindSymbol("EVAL"); 
   pZhSymBreak =  zh_dynsymFindSymbol("BREAK");
   pZhSymErrorNew =  zh_dynsymFindSymbol("ERRORNEW");


}
