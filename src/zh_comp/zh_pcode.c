/*
 * Compiler PCode generation functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

#include "zh_comp.h"
#include "zh_assert.h"

#define ZH_PSIZE_FUNC( func )  ZH_PCODE_FUNC( func, PZH_VOID )

/*
 * functions for variable size PCODE tracing
 */
static ZH_PSIZE_FUNC( zh_p_pushstrshort )
{
   ZH_SYMBOL_UNUSED( cargo );
   return 2 + pFunc->pCode[ nPCodePos + 1 ];
}

static ZH_PSIZE_FUNC( zh_p_pushstr )
{
   ZH_SYMBOL_UNUSED( cargo );
   return 3 + ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
}

static ZH_PSIZE_FUNC( zh_p_pushstrlarge )
{
   ZH_SYMBOL_UNUSED( cargo );
   return 4 + ZH_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] );
}

static ZH_PSIZE_FUNC( zh_p_push_str_hidden )
{
   ZH_SYMBOL_UNUSED( cargo );
   return 4 + ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 2 ] );
}

static ZH_PSIZE_FUNC( zh_p_push_block )
{
   ZH_SYMBOL_UNUSED( cargo );
   return ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
}

static ZH_PSIZE_FUNC( zh_p_push_blockshort )
{
   ZH_SYMBOL_UNUSED( cargo );
   return pFunc->pCode[ nPCodePos + 1 ];
}

static ZH_PSIZE_FUNC( zh_p_push_blocklarge )
{
   ZH_SYMBOL_UNUSED( cargo );
   return ZH_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] );
}

static ZH_PSIZE_FUNC( zh_p_localname )
{
   ZH_SIZE nStart = nPCodePos;

   ZH_SYMBOL_UNUSED( cargo );
   nPCodePos += 3;
   while( pFunc->pCode[ nPCodePos++ ] )
      ;

   return nPCodePos - nStart;
}

static ZH_PSIZE_FUNC( zh_p_modulename )
{
   ZH_SIZE nStart = nPCodePos;

   ZH_SYMBOL_UNUSED( cargo );
   nPCodePos++;
   while( pFunc->pCode[ nPCodePos++ ])
      ;

   return nPCodePos - nStart;
}

static ZH_PSIZE_FUNC( zh_p_staticname )
{
   ZH_SIZE nStart = nPCodePos;

   ZH_SYMBOL_UNUSED( cargo );
   nPCodePos += 4;
   while( pFunc->pCode[ nPCodePos++ ] )
      ;

   return nPCodePos - nStart;
}

static ZH_PSIZE_FUNC( zh_p_threadstatics )
{
   ZH_SYMBOL_UNUSED( cargo );
   return 3 + ( ( ZH_SIZE ) ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) << 1 );
}

const ZH_BYTE zh_comp_pcode_len[] = {
   1,        /* ZH_P_AND                   */
   1,        /* ZH_P_ARRAYPUSH             */
   1,        /* ZH_P_ARRAYPOP              */
   3,        /* ZH_P_ARRAYDIM              */
   3,        /* ZH_P_ARRAYGEN              */
   1,        /* ZH_P_EQUAL                 */
   1,        /* ZH_P_ENDBLOCK              */
   1,        /* ZH_P_ENDPROC               */
   1,        /* ZH_P_EXACTLYEQUAL          */
   1,        /* ZH_P_FALSE                 */
   1,        /* ZH_P_FORTEST               */
   3,        /* ZH_P_FUNCTION              */
   2,        /* ZH_P_FUNCTIONSHORT         */
   3,        /* ZH_P_FRAME                 */
   1,        /* ZH_P_FUNCPTR               */
   1,        /* ZH_P_GREATER               */
   1,        /* ZH_P_GREATEREQUAL          */
   1,        /* ZH_P_DEC                   */
   1,        /* ZH_P_DIVIDE                */
   3,        /* ZH_P_DO                    */
   2,        /* ZH_P_DOSHORT               */
   1,        /* ZH_P_DUPLICATE             */
   9,        /* ZH_P_PUSHTIMESTAMP         */
   1,        /* ZH_P_INC                   */
   1,        /* ZH_P_INSTRING              */
   2,        /* ZH_P_JUMPNEAR              */
   3,        /* ZH_P_JUMP                  */
   4,        /* ZH_P_JUMPFAR               */
   2,        /* ZH_P_JUMPFALSENEAR         */
   3,        /* ZH_P_JUMPFALSE             */
   4,        /* ZH_P_JUMPFALSEFAR          */
   2,        /* ZH_P_JUMPTRUENEAR          */
   3,        /* ZH_P_JUMPTRUE              */
   4,        /* ZH_P_JUMPTRUEFAR           */
   1,        /* ZH_P_LESSEQUAL             */
   1,        /* ZH_P_LESS                  */
   3,        /* ZH_P_LINE                  */
   0,        /* ZH_P_LOCALNAME             */
   2,        /* ZH_P_MACROPOP              */
   2,        /* ZH_P_MACROPOPALIASED       */
   2,        /* ZH_P_MACRO_PUSH             */
   3,        /* ZH_P_MACRO_ARRAY_GEN         */
   2,        /* ZH_P_MACRO_PUSHLIST         */
   1,        /* ZH_P_MACRO_PUSHINDEX        */
   2,        /* ZH_P_MACRO_PUSHPARE         */
   2,        /* ZH_P_MACRO_PUSHALIASED      */
   1,        /* ZH_P_MACROSYMBOL           */
   1,        /* ZH_P_MACROTEXT             */
   3,        /* ZH_P_MESSAGE               */
   1,        /* ZH_P_MINUS                 */
   1,        /* ZH_P_MODULUS               */
   0,        /* ZH_P_MODULENAME            */
             /* start: pcodes generated by macro compiler */
   3,        /* ZH_P_MMESSAGE              */
   3,        /* ZH_P_MPOPALIASEDFIELD      */
   3,        /* ZH_P_MPOPALIASEDVAR        */
   3,        /* ZH_P_MPOPFIELD             */
   3,        /* ZH_P_MPOPMEMVAR            */
   3,        /* ZH_P_MPUSH_ALIASED_FIELD     */
   3,        /* ZH_P_MPUSHALIASEDVAR       */
   0,        /* ZH_P_MPUSH_BLOCK            */
   3,        /* ZH_P_MPUSHFIELD            */
   3,        /* ZH_P_MPUSHMEMVAR           */
   3,        /* ZH_P_MPUSHMEMVARREF        */
   3,        /* ZH_P_MPUSHSYM              */
   3,        /* ZH_P_MPUSHVARIABLE         */
             /* end: */
   1,        /* ZH_P_MULT                  */
   1,        /* ZH_P_NEGATE                */
   1,        /* ZH_P_NOOP                  */
   1,        /* ZH_P_NOT                   */
   1,        /* ZH_P_NOTEQUAL              */
   1,        /* ZH_P_OR                    */
   4,        /* ZH_P_PARAMETER             */
   1,        /* ZH_P_PLUS                  */
   1,        /* ZH_P_POP                   */
   1,        /* ZH_P_POPALIAS              */
   3,        /* ZH_P_POPALIASEDFIELD       */
   2,        /* ZH_P_POPALIASEDFIELDNEAR   */
   3,        /* ZH_P_POPALIASEDVAR         */
   3,        /* ZH_P_POPFIELD              */
   3,        /* ZH_P_POPLOCAL              */
   2,        /* ZH_P_POPLOCALNEAR          */
   3,        /* ZH_P_POPMEMVAR             */
   3,        /* ZH_P_POPSTATIC             */
   3,        /* ZH_P_POPVAR           */
   1,        /* ZH_P_POWER                 */
   1,        /* ZH_P_PUSHALIAS             */
   3,        /* ZH_P_PUSH_ALIASED_FIELD      */
   2,        /* ZH_P_PUSH_ALIASED_FIELDNEAR  */
   3,        /* ZH_P_PUSHALIASEDVAR        */
   0,        /* ZH_P_PUSH_BLOCK             */
   0,        /* ZH_P_PUSH_BLOCKSHORT        */
   3,        /* ZH_P_PUSHFIELD             */
   2,        /* ZH_P_PUSHBYTE              */
   3,        /* ZH_P_PUSHINT               */
   3,        /* ZH_P_PUSHLOCAL             */
   2,        /* ZH_P_PUSHLOCALNEAR         */
   3,        /* ZH_P_PUSHLOCALREF          */
   5,        /* ZH_P_PUSHLONG              */
   3,        /* ZH_P_PUSHMEMVAR            */
   3,        /* ZH_P_PUSHMEMVARREF         */
   1,        /* ZH_P_PUSHNIL               */
   1 + sizeof( double ) + sizeof( ZH_BYTE ) + sizeof( ZH_BYTE ),        /* ZH_P_PUSHDOUBLE            */
   1,        /* ZH_P_PUSHSELF              */
   3,        /* ZH_P_PUSHSTATIC            */
   3,        /* ZH_P_PUSHSTATICREF         */
   0,        /* ZH_P_PUSHSTR               */
   0,        /* ZH_P_PUSHSTRSHORT          */
   3,        /* ZH_P_PUSHSYM               */
   2,        /* ZH_P_PUSHSYMNEAR           */
   3,        /* ZH_P_PUSHVAR          */
   1,        /* ZH_P_RETVALUE              */
   3,        /* ZH_P_SEND                  */
   2,        /* ZH_P_SENDSHORT             */
   4,        /* ZH_P_SEQBEGIN              */
   4,        /* ZH_P_SEQEND                */
   1,        /* ZH_P_SEQRECOVER            */
   3,        /* ZH_P_SFRAME                */
   5,        /* ZH_P_STATICS               */
   0,        /* ZH_P_STATICNAME            */
   1,        /* ZH_P_SWAPALIAS             */
   1,        /* ZH_P_TRUE                  */
   1,        /* ZH_P_ZERO                  */
   1,        /* ZH_P_ONE                   */
   3,        /* ZH_P_MACRO_FUNC             */
   3,        /* ZH_P_MACRODO               */
   0,        /* ZH_P_MPUSHSTR              */
   4,        /* ZH_P_LOCALNEARADDINT       */
   1,        /* ZH_P_MACRO_PUSHREF          */
   9,        /* ZH_P_PUSHLONGLONG          */
   3,        /* ZH_P_ENUMSTART             */
   1,        /* ZH_P_ENUMNEXT              */
   1,        /* ZH_P_ENUMPREV              */
   1,        /* ZH_P_ENUMEND               */
   3,        /* ZH_P_SWITCH                */
   5,        /* ZH_P_PUSH_DATE              */
             /* optimization of inlined math operations */
   1,        /* ZH_P_PLUSEQPOP             */
   1,        /* ZH_P_MINUSEQPOP            */
   1,        /* ZH_P_MULTEQPOP             */
   1,        /* ZH_P_DIVEQPOP              */
   1,        /* ZH_P_PLUSEQ                */
   1,        /* ZH_P_MINUSEQ               */
   1,        /* ZH_P_MULTEQ                */
   1,        /* ZH_P_DIVEQ                 */
   1,        /* ZH_P_WITHOBJECTSTART       */
   3,        /* ZH_P_WITHOBJECTMESSAGE     */
   1,        /* ZH_P_WITHOBJECTEND         */
   3,        /* ZH_P_MACRO_SEND             */
   1,        /* ZH_P_PUSHOVARREF           */
   1,        /* ZH_P_ARRAYPUSHREF          */
   3,        /* ZH_P_VFRAME                */
   4,        /* ZH_P_LARGEFRAME            */
   4,        /* ZH_P_LARGEVFRAME           */
   0,        /* ZH_P_PUSH_STR_HIDDEN         */
   5,        /* ZH_P_LOCALADDINT           */
   1,        /* ZH_P_MODEQPOP              */
   1,        /* ZH_P_EXPEQPOP              */
   1,        /* ZH_P_MODEQ                 */
   1,        /* ZH_P_EXPEQ                 */
   1,        /* ZH_P_DUPLUNREF             */
   0,        /* ZH_P_MPUSH_BLOCKLARGE       */
   0,        /* ZH_P_MPUSHSTRLARGE         */
   0,        /* ZH_P_PUSH_BLOCKLARGE        */
   0,        /* ZH_P_PUSHSTRLARGE          */
   2,        /* ZH_P_SWAP                  */
   1,        /* ZH_P_PUSHVPARAMS           */
   1,        /* ZH_P_PUSHUNREF             */
   4,        /* ZH_P_SEQALWAYS             */
   4,        /* ZH_P_ALWAYSBEGIN           */
   1,        /* ZH_P_ALWAYSEND             */
   1,        /* ZH_P_DECEQPOP              */
   1,        /* ZH_P_INCEQPOP              */
   1,        /* ZH_P_DECEQ                 */
   1,        /* ZH_P_INCEQ                 */
   3,        /* ZH_P_LOCALDEC              */
   3,        /* ZH_P_LOCALINC              */
   3,        /* ZH_P_LOCALINCPUSH          */
   3,        /* ZH_P_PUSHFUNCSYM           */
   3,        /* ZH_P_HASHGEN               */
   1,        /* ZH_P_SEQBLOCK              */
   0,        /* ZH_P_THREADSTATICS         */
   1         /* ZH_P_PUSHAPARAMS           */
};

/*
 * this table has pointers to functions which count
 * real size of variable size PCODEs
 */
static const PZH_PCODE_FUNC s_psize_table[] =
{
   NULL,                       /* ZH_P_AND                   */
   NULL,                       /* ZH_P_ARRAYPUSH             */
   NULL,                       /* ZH_P_ARRAYPOP              */
   NULL,                       /* ZH_P_ARRAYDIM              */
   NULL,                       /* ZH_P_ARRAYGEN              */
   NULL,                       /* ZH_P_EQUAL                 */
   NULL,                       /* ZH_P_ENDBLOCK              */
   NULL,                       /* ZH_P_ENDPROC               */
   NULL,                       /* ZH_P_EXACTLYEQUAL          */
   NULL,                       /* ZH_P_FALSE                 */
   NULL,                       /* ZH_P_FORTEST               */
   NULL,                       /* ZH_P_FUNCTION              */
   NULL,                       /* ZH_P_FUNCTIONSHORT         */
   NULL,                       /* ZH_P_FRAME                 */
   NULL,                       /* ZH_P_FUNCPTR               */
   NULL,                       /* ZH_P_GREATER               */
   NULL,                       /* ZH_P_GREATEREQUAL          */
   NULL,                       /* ZH_P_DEC                   */
   NULL,                       /* ZH_P_DIVIDE                */
   NULL,                       /* ZH_P_DO                    */
   NULL,                       /* ZH_P_DOSHORT               */
   NULL,                       /* ZH_P_DUPLICATE             */
   NULL,                       /* ZH_P_PUSHTIMESTAMP         */
   NULL,                       /* ZH_P_INC                   */
   NULL,                       /* ZH_P_INSTRING              */
   NULL,                       /* ZH_P_JUMPNEAR              */
   NULL,                       /* ZH_P_JUMP                  */
   NULL,                       /* ZH_P_JUMPFAR               */
   NULL,                       /* ZH_P_JUMPFALSENEAR         */
   NULL,                       /* ZH_P_JUMPFALSE             */
   NULL,                       /* ZH_P_JUMPFALSEFAR          */
   NULL,                       /* ZH_P_JUMPTRUENEAR          */
   NULL,                       /* ZH_P_JUMPTRUE              */
   NULL,                       /* ZH_P_JUMPTRUEFAR           */
   NULL,                       /* ZH_P_LESSEQUAL             */
   NULL,                       /* ZH_P_LESS                  */
   NULL,                       /* ZH_P_LINE                  */
   zh_p_localname,             /* ZH_P_LOCALNAME             */
   NULL,                       /* ZH_P_MACROPOP              */
   NULL,                       /* ZH_P_MACROPOPALIASED       */
   NULL,                       /* ZH_P_MACRO_PUSH             */
   NULL,                       /* ZH_P_MACRO_ARRAY_GEN         */
   NULL,                       /* ZH_P_MACRO_PUSHLIST         */
   NULL,                       /* ZH_P_MACRO_PUSHINDEX        */
   NULL,                       /* ZH_P_MACRO_PUSHPARE         */
   NULL,                       /* ZH_P_MACRO_PUSHALIASED      */
   NULL,                       /* ZH_P_MACROSYMBOL           */
   NULL,                       /* ZH_P_MACROTEXT             */
   NULL,                       /* ZH_P_MESSAGE               */
   NULL,                       /* ZH_P_MINUS                 */
   NULL,                       /* ZH_P_MODULUS               */
   zh_p_modulename,            /* ZH_P_MODULENAME            */
                               /* start: pcodes generated by macro compiler */
   NULL,                       /* ZH_P_MMESSAGE              */
   NULL,                       /* ZH_P_MPOPALIASEDFIELD      */
   NULL,                       /* ZH_P_MPOPALIASEDVAR        */
   NULL,                       /* ZH_P_MPOPFIELD             */
   NULL,                       /* ZH_P_MPOPMEMVAR            */
   NULL,                       /* ZH_P_MPUSH_ALIASED_FIELD     */
   NULL,                       /* ZH_P_MPUSHALIASEDVAR       */
   NULL,                       /* ZH_P_MPUSH_BLOCK            */
   NULL,                       /* ZH_P_MPUSHFIELD            */
   NULL,                       /* ZH_P_MPUSHMEMVAR           */
   NULL,                       /* ZH_P_MPUSHMEMVARREF        */
   NULL,                       /* ZH_P_MPUSHSYM              */
   NULL,                       /* ZH_P_MPUSHVARIABLE         */
                               /* end: */
   NULL,                       /* ZH_P_MULT                  */
   NULL,                       /* ZH_P_NEGATE                */
   NULL,                       /* ZH_P_NOOP                  */
   NULL,                       /* ZH_P_NOT                   */
   NULL,                       /* ZH_P_NOTEQUAL              */
   NULL,                       /* ZH_P_OR                    */
   NULL,                       /* ZH_P_PARAMETER             */
   NULL,                       /* ZH_P_PLUS                  */
   NULL,                       /* ZH_P_POP                   */
   NULL,                       /* ZH_P_POPALIAS              */
   NULL,                       /* ZH_P_POPALIASEDFIELD       */
   NULL,                       /* ZH_P_POPALIASEDFIELDNEAR   */
   NULL,                       /* ZH_P_POPALIASEDVAR         */
   NULL,                       /* ZH_P_POPFIELD              */
   NULL,                       /* ZH_P_POPLOCAL              */
   NULL,                       /* ZH_P_POPLOCALNEAR          */
   NULL,                       /* ZH_P_POPMEMVAR             */
   NULL,                       /* ZH_P_POPSTATIC             */
   NULL,                       /* ZH_P_POPVAR           */
   NULL,                       /* ZH_P_POWER                 */
   NULL,                       /* ZH_P_PUSHALIAS             */
   NULL,                       /* ZH_P_PUSH_ALIASED_FIELD      */
   NULL,                       /* ZH_P_PUSH_ALIASED_FIELDNEAR  */
   NULL,                       /* ZH_P_PUSHALIASEDVAR        */
   zh_p_push_block,             /* ZH_P_PUSH_BLOCK             */
   zh_p_push_blockshort,        /* ZH_P_PUSH_BLOCKSHORT        */
   NULL,                       /* ZH_P_PUSHFIELD             */
   NULL,                       /* ZH_P_PUSHBYTE              */
   NULL,                       /* ZH_P_PUSHINT               */
   NULL,                       /* ZH_P_PUSHLOCAL             */
   NULL,                       /* ZH_P_PUSHLOCALNEAR         */
   NULL,                       /* ZH_P_PUSHLOCALREF          */
   NULL,                       /* ZH_P_PUSHLONG              */
   NULL,                       /* ZH_P_PUSHMEMVAR            */
   NULL,                       /* ZH_P_PUSHMEMVARREF         */
   NULL,                       /* ZH_P_PUSHNIL               */
   NULL,                       /* ZH_P_PUSHDOUBLE            */
   NULL,                       /* ZH_P_PUSHSELF              */
   NULL,                       /* ZH_P_PUSHSTATIC            */
   NULL,                       /* ZH_P_PUSHSTATICREF         */
   zh_p_pushstr,               /* ZH_P_PUSHSTR               */
   zh_p_pushstrshort,          /* ZH_P_PUSHSTRSHORT          */
   NULL,                       /* ZH_P_PUSHSYM               */
   NULL,                       /* ZH_P_PUSHSYMNEAR           */
   NULL,                       /* ZH_P_PUSHVAR          */
   NULL,                       /* ZH_P_RETVALUE              */
   NULL,                       /* ZH_P_SEND                  */
   NULL,                       /* ZH_P_SENDSHORT             */
   NULL,                       /* ZH_P_SEQBEGIN              */
   NULL,                       /* ZH_P_SEQEND                */
   NULL,                       /* ZH_P_SEQRECOVER            */
   NULL,                       /* ZH_P_SFRAME                */
   NULL,                       /* ZH_P_STATICS               */
   zh_p_staticname,            /* ZH_P_STATICNAME            */
   NULL,                       /* ZH_P_SWAPALIAS             */
   NULL,                       /* ZH_P_TRUE                  */
   NULL,                       /* ZH_P_ZERO                  */
   NULL,                       /* ZH_P_ONE                   */
   NULL,                       /* ZH_P_MACRO_FUNC             */
   NULL,                       /* ZH_P_MACRODO               */
   NULL,                       /* ZH_P_MPUSHSTR              */
   NULL,                       /* ZH_P_LOCALNEARADDINT       */
   NULL,                       /* ZH_P_MACRO_PUSHREF          */
   NULL,                       /* ZH_P_PUSHLONGLONG          */
   NULL,                       /* ZH_P_ENUMSTART             */
   NULL,                       /* ZH_P_ENUMNEXT              */
   NULL,                       /* ZH_P_ENUMPREV              */
   NULL,                       /* ZH_P_ENUMEND               */
   NULL,                       /* ZH_P_SWITCH                */
   NULL,                       /* ZH_P_PUSH_DATE              */
                               /* optimization of inlined math operations */
   NULL,                       /* ZH_P_PLUSEQPOP             */
   NULL,                       /* ZH_P_MINUSEQPOP            */
   NULL,                       /* ZH_P_MULTEQPOP             */
   NULL,                       /* ZH_P_DIVEQPOP              */
   NULL,                       /* ZH_P_PLUSEQ                */
   NULL,                       /* ZH_P_MINUSEQ               */
   NULL,                       /* ZH_P_MULTEQ                */
   NULL,                       /* ZH_P_DIVEQ                 */
   NULL,                       /* ZH_P_WITHOBJECTSTART       */
   NULL,                       /* ZH_P_WITHOBJECTMESSAGE     */
   NULL,                       /* ZH_P_WITHOBJECTEND         */
   NULL,                       /* ZH_P_MACRO_SEND             */
   NULL,                       /* ZH_P_PUSHOVARREF           */
   NULL,                       /* ZH_P_ARRAYPUSHREF          */
   NULL,                       /* ZH_P_VFRAME                */
   NULL,                       /* ZH_P_LARGEFRAME            */
   NULL,                       /* ZH_P_LARGEVFRAME           */
   zh_p_push_str_hidden,         /* ZH_P_PUSH_STR_HIDDEN         */
   NULL,                       /* ZH_P_LOCALADDINT           */
   NULL,                       /* ZH_P_MODEQPOP              */
   NULL,                       /* ZH_P_EXPEQPOP              */
   NULL,                       /* ZH_P_MODEQ                 */
   NULL,                       /* ZH_P_EXPEQ                 */
   NULL,                       /* ZH_P_DUPLUNREF             */
   NULL,                       /* ZH_P_MPUSH_BLOCKLARGE       */
   NULL,                       /* ZH_P_MPUSHSTRLARGE         */
   zh_p_push_blocklarge,        /* ZH_P_PUSH_BLOCKLARGE        */
   zh_p_pushstrlarge,          /* ZH_P_PUSHSTRLARGE          */
   NULL,                       /* ZH_P_SWAP                  */
   NULL,                       /* ZH_P_PUSHVPARAMS           */
   NULL,                       /* ZH_P_PUSHUNREF             */
   NULL,                       /* ZH_P_SEQALWAYS             */
   NULL,                       /* ZH_P_ALWAYSBEGIN           */
   NULL,                       /* ZH_P_ALWAYSEND             */
   NULL,                       /* ZH_P_DECEQPOP              */
   NULL,                       /* ZH_P_INCEQPOP              */
   NULL,                       /* ZH_P_DECEQ                 */
   NULL,                       /* ZH_P_INCEQ                 */
   NULL,                       /* ZH_P_LOCALDEC              */
   NULL,                       /* ZH_P_LOCALINC              */
   NULL,                       /* ZH_P_LOCALINCPUSH          */
   NULL,                       /* ZH_P_PUSHFUNCSYM           */
   NULL,                       /* ZH_P_HASHGEN               */
   NULL,                       /* ZH_P_SEQBLOCK              */
   zh_p_threadstatics,         /* ZH_P_THREADSTATICS         */
   NULL                        /* ZH_P_PUSHAPARAMS           */
};

ZH_I_SIZE zh_compPCodeSize( PZH_ZFUNC pFunc, ZH_SIZE nOffset )
{
   ZH_I_SIZE nSize = 0;
   ZH_BYTE opcode = pFunc->pCode[ nOffset ];

   if( opcode < ZH_P_LAST_PCODE )
   {
      nSize = zh_comp_pcode_len[ opcode ];

      if( nSize == 0 )
      {
         PZH_PCODE_FUNC pCall = s_psize_table[ opcode ];

         if( pCall != NULL )
            nSize = pCall( pFunc, nOffset, NULL );
      }
   }
   return nSize;
}

void zh_compPCodeEval( PZH_ZFUNC pFunc, const PZH_PCODE_FUNC * pFunctions, void * cargo )
{
   ZH_SIZE nPos = 0;
   ZH_SIZE nSkip;

   /* Make sure that table is correct */
   assert( sizeof( zh_comp_pcode_len ) == ZH_P_LAST_PCODE );
   assert( sizeof( s_psize_table ) / sizeof( PZH_PCODE_FUNC ) == ZH_P_LAST_PCODE );

   while( nPos < pFunc->nPCodePos )
   {
      ZH_BYTE opcode = pFunc->pCode[ nPos ];
      if( opcode < ZH_P_LAST_PCODE )
      {
         PZH_PCODE_FUNC pCall = pFunctions[ opcode ];
         nSkip = pCall ? pCall( pFunc, nPos, cargo ) : 0;
         if( nSkip == 0 )
         {
            nSkip = zh_comp_pcode_len[ opcode ];
            if( nSkip == 0 )
            {
               pCall = s_psize_table[ opcode ];
               if( pCall != NULL )
                  nSkip = pCall( pFunc, nPos, NULL );
            }
         }

         if( nSkip == 0 )
         {
            char szOpcode[ 16 ];
            ++nPos;
            zh_snprintf( szOpcode, sizeof( szOpcode ), "%i", opcode );
            zh_errInternal( ZH_EI_COMPBADOPSIZE, "Invalid (zero) opcode %s size in zh_compPCodeEval()", szOpcode, NULL );
         }
         nPos += nSkip;
      }
      else
      {
         char szOpcode[ 16 ];
         ++nPos;
         zh_snprintf( szOpcode, sizeof( szOpcode ), "%i", opcode );
         zh_errInternal( ZH_EI_COMPBADOPCODE, "Invalid opcode: %s in zh_compPCodeEval()", szOpcode, NULL );
      }
   }
}

void zh_compPCodeTrace( PZH_ZFUNC pFunc, const PZH_PCODE_FUNC * pFunctions, void * cargo )
{
   ZH_SIZE nPos = 0;

   /* Make sure that table is correct */
   assert( sizeof( zh_comp_pcode_len ) == ZH_P_LAST_PCODE );

   while( nPos < pFunc->nPCodePos )
   {
      ZH_BYTE opcode = pFunc->pCode[ nPos ];
      if( opcode < ZH_P_LAST_PCODE )
      {
         PZH_PCODE_FUNC pCall = pFunctions[ opcode ];
         if( pCall )
            nPos = pCall( pFunc, nPos, cargo );
         else
            nPos += zh_comp_pcode_len[ opcode ];
      }
      else
      {
         char szOpcode[ 16 ];
         ++nPos;
         zh_snprintf( szOpcode, sizeof( szOpcode ), "%i", opcode );
         zh_errInternal( ZH_EI_COMPBADOPCODE, "Invalid opcode: %s in zh_compPCodeTrace()", szOpcode, NULL );
      }
   }
}

void zh_compGenPCode1( ZH_BYTE byte, ZH_COMP_DECL )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;   /* get the currently defined function */

   if( ! pFunc->pCode )                              /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( ZH_BYTE * ) zh_xgrab( ZH_PCODE_CHUNK );
      pFunc->nPCodeSize = ZH_PCODE_CHUNK;
      pFunc->nPCodePos  = 0;
   }
   else if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 1 )
      pFunc->pCode = ( ZH_BYTE * ) zh_xrealloc( pFunc->pCode, pFunc->nPCodeSize += ZH_PCODE_CHUNK );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte;
}

void zh_compGenPCode2( ZH_BYTE byte1, ZH_BYTE byte2, ZH_COMP_DECL )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;   /* get the currently defined function */

   if( ! pFunc->pCode )                              /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( ZH_BYTE * ) zh_xgrab( ZH_PCODE_CHUNK );
      pFunc->nPCodeSize = ZH_PCODE_CHUNK;
      pFunc->nPCodePos  = 0;
   }
   else if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 2 )
      pFunc->pCode = ( ZH_BYTE * ) zh_xrealloc( pFunc->pCode, pFunc->nPCodeSize += ZH_PCODE_CHUNK );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte2;
}

void zh_compGenPCode3( ZH_BYTE byte1, ZH_BYTE byte2, ZH_BYTE byte3, ZH_COMP_DECL )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;   /* get the currently defined function */

   if( ! pFunc->pCode )                              /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( ZH_BYTE * ) zh_xgrab( ZH_PCODE_CHUNK );
      pFunc->nPCodeSize = ZH_PCODE_CHUNK;
      pFunc->nPCodePos  = 0;
   }
   else if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 3 )
      pFunc->pCode = ( ZH_BYTE * ) zh_xrealloc( pFunc->pCode, pFunc->nPCodeSize += ZH_PCODE_CHUNK );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte3;
}

void zh_compGenPCode4( ZH_BYTE byte1, ZH_BYTE byte2, ZH_BYTE byte3, ZH_BYTE byte4, ZH_COMP_DECL )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;   /* get the currently defined function */

   if( ! pFunc->pCode )                              /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( ZH_BYTE * ) zh_xgrab( ZH_PCODE_CHUNK );
      pFunc->nPCodeSize = ZH_PCODE_CHUNK;
      pFunc->nPCodePos  = 0;
   }
   else if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 4 )
      pFunc->pCode = ( ZH_BYTE * ) zh_xrealloc( pFunc->pCode, pFunc->nPCodeSize += ZH_PCODE_CHUNK );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte3;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte4;
}

void zh_compGenPCodeN( const ZH_BYTE * pBuffer, ZH_SIZE nSize, ZH_COMP_DECL )
{
   PZH_ZFUNC pFunc = ZH_COMP_PARAM->functions.pLast;   /* get the currently defined function */

   if( ! pFunc->pCode )                              /* has been created the memory block to hold the pcode ? */
   {
      pFunc->nPCodeSize = ( ( nSize / ZH_PCODE_CHUNK ) + 1 ) * ZH_PCODE_CHUNK;
      pFunc->pCode      = ( ZH_BYTE * ) zh_xgrab( pFunc->nPCodeSize );
      pFunc->nPCodePos  = 0;
   }
   else if( pFunc->nPCodePos + nSize > pFunc->nPCodeSize )
   {
      /* not enough free space in pcode buffer - increase it */
      pFunc->nPCodeSize += ( ( ( nSize / ZH_PCODE_CHUNK ) + 1 ) * ZH_PCODE_CHUNK );
      pFunc->pCode = ( ZH_BYTE * ) zh_xrealloc( pFunc->pCode, pFunc->nPCodeSize );
   }

   memcpy( pFunc->pCode + pFunc->nPCodePos, pBuffer, nSize );
   pFunc->nPCodePos += nSize;
}
