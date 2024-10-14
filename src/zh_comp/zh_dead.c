/*
 * Dead (unaccessible) PCODE eliminator
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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

#include "zh_comp.h"
#include "zh_assert.h"

/* helper structure to pass information */
typedef struct _ZH_CODETRACE_INFO
{
   ZH_BYTE * pCodeMark;
   ZH_SIZE * pnJumps;
   ZH_SIZE   nJumpPos;
   ZH_SIZE   nJumpSize;
   ZH_SIZE   nJumpCount;
   ZH_SIZE   nPCodeSize;
   ZH_BOOL   fFinished;
} ZH_CODETRACE_INFO, * PZH_CODETRACE_INFO;

#define ZH_CODETRACE_FUNC( func )  ZH_PCODE_FUNC( func, PZH_CODETRACE_INFO )
typedef ZH_CODETRACE_FUNC( ZH_CODETRACE_FUNC_ );
typedef ZH_CODETRACE_FUNC_ * PZH_CODETRACE_FUNC;

#define ZH_JUMPADDR_ALLOC  64


static void zh_compCodeTraceAddJump( PZH_CODETRACE_INFO pInfo, ZH_SIZE nPCodePos )
{
   /* Checking for nPCodePos < pInfo->nPCodeSize disabled intentionally
    * for easier detecting bugs in generated PCODE
    */
   /*
      if( nPCodePos < pInfo->nPCodeSize && pInfo->pCodeMark[ nPCodePos ] == 0 )
    */
   if( pInfo->pCodeMark[ nPCodePos ] == 0 )
   {
      if( pInfo->nJumpSize == 0 )
      {
         pInfo->nJumpSize = ZH_JUMPADDR_ALLOC;
         pInfo->pnJumps = ( ZH_SIZE * ) zh_xgrab( pInfo->nJumpSize *
                                                  sizeof( ZH_SIZE ) );
      }
      else if( pInfo->nJumpSize == pInfo->nJumpCount )
      {
         pInfo->nJumpSize += ZH_JUMPADDR_ALLOC;
         pInfo->pnJumps = ( ZH_SIZE * ) zh_xrealloc( pInfo->pnJumps,
                                                     pInfo->nJumpSize * sizeof( ZH_SIZE ) );
      }
      pInfo->pnJumps[ pInfo->nJumpCount++ ] = nPCodePos;
      pInfo->pCodeMark[ nPCodePos ] = 1;
   }
}

static ZH_SIZE zh_compCodeTraceNextPos( PZH_CODETRACE_INFO pInfo, ZH_SIZE nPCodePos )
{
   if( nPCodePos < pInfo->nPCodeSize && pInfo->pCodeMark[ nPCodePos ] == 0 )
      return nPCodePos;

   while( pInfo->nJumpPos < pInfo->nJumpCount )
   {
      nPCodePos = pInfo->pnJumps[ pInfo->nJumpPos++ ];
      if( pInfo->pCodeMark[ nPCodePos ] == 1 )
         return nPCodePos;
   }

   pInfo->fFinished = ZH_TRUE;
   return pInfo->nPCodeSize;
}

static void zh_compCodeTraceMark( PZH_CODETRACE_INFO pInfo, ZH_SIZE nPCodePos, ZH_SIZE nSize )
{
   if( nSize > 0 )
      memset( &pInfo->pCodeMark[ nPCodePos ], 2, nSize );
}

/*
 * PCODE trace functions
 */

static ZH_CODETRACE_FUNC( zh_p_default )
{
   ZH_SIZE nSize = zh_compPCodeSize( pFunc, nPCodePos );

   zh_compCodeTraceMark( cargo, nPCodePos, nSize );
   return zh_compCodeTraceNextPos( cargo, nPCodePos + nSize );
}

static ZH_CODETRACE_FUNC( zh_p_jumpnear )
{
   ZH_SIZE nNewPos = nPCodePos + ( signed char ) pFunc->pCode[ nPCodePos + 1 ];

   zh_compCodeTraceMark( cargo, nPCodePos, 2 );
   return zh_compCodeTraceNextPos( cargo, nNewPos );
}

static ZH_CODETRACE_FUNC( zh_p_jump )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKSHORT( pAddr );

   zh_compCodeTraceMark( cargo, nPCodePos, 3 );
   return zh_compCodeTraceNextPos( cargo, nNewPos );
}

static ZH_CODETRACE_FUNC( zh_p_jumpfar )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   zh_compCodeTraceMark( cargo, nPCodePos, 4 );
   return zh_compCodeTraceNextPos( cargo, nNewPos );
}

static ZH_CODETRACE_FUNC( zh_p_jumpfalsenear )
{
   ZH_SIZE nNewPos = nPCodePos + ( signed char ) pFunc->pCode[ nPCodePos + 1 ];

   zh_compCodeTraceMark( cargo, nPCodePos, 2 );
   zh_compCodeTraceAddJump( cargo, nNewPos );

   return zh_compCodeTraceNextPos( cargo, nPCodePos + 2 );
}

static ZH_CODETRACE_FUNC( zh_p_jumpfalse )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKSHORT( pAddr );

   zh_compCodeTraceMark( cargo, nPCodePos, 3 );
   zh_compCodeTraceAddJump( cargo, nNewPos );

   return zh_compCodeTraceNextPos( cargo, nPCodePos + 3 );
}

static ZH_CODETRACE_FUNC( zh_p_jumpfalsefar )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   zh_compCodeTraceMark( cargo, nPCodePos, 4 );
   zh_compCodeTraceAddJump( cargo, nNewPos );

   return zh_compCodeTraceNextPos( cargo, nPCodePos + 4 );
}

static ZH_CODETRACE_FUNC( zh_p_jumptruenear )
{
   ZH_SIZE nNewPos = nPCodePos + ( signed char ) pFunc->pCode[ nPCodePos + 1 ];

   zh_compCodeTraceMark( cargo, nPCodePos, 2 );
   zh_compCodeTraceAddJump( cargo, nNewPos );

   return zh_compCodeTraceNextPos( cargo, nPCodePos + 2 );
}

static ZH_CODETRACE_FUNC( zh_p_jumptrue )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKSHORT( pAddr );

   zh_compCodeTraceMark( cargo, nPCodePos, 3 );
   zh_compCodeTraceAddJump( cargo, nNewPos );

   return zh_compCodeTraceNextPos( cargo, nPCodePos + 3 );
}

static ZH_CODETRACE_FUNC( zh_p_jumptruefar )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   zh_compCodeTraceMark( cargo, nPCodePos, 4 );
   zh_compCodeTraceAddJump( cargo, nNewPos );

   return zh_compCodeTraceNextPos( cargo, nPCodePos + 4 );
}

static ZH_CODETRACE_FUNC( zh_p_seqalways )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nAlwaysPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   zh_compCodeTraceMark( cargo, nPCodePos, 4 );
   zh_compCodeTraceAddJump( cargo, nAlwaysPos );

   return zh_compCodeTraceNextPos( cargo, nPCodePos + 4 );
}

static ZH_CODETRACE_FUNC( zh_p_alwaysbegin )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nAlwaysEndPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   zh_compCodeTraceMark( cargo, nPCodePos, 4 );
   zh_compCodeTraceAddJump( cargo, nAlwaysEndPos );

   return zh_compCodeTraceNextPos( cargo, nPCodePos + 4 );
}

static ZH_CODETRACE_FUNC( zh_p_seqbegin )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nRecoverPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   /* this is a hack for -gc3 output - it's not really necessary
    * for pure PCODE evaluation
    */
   if( pFunc->pCode[ nRecoverPos ] != ZH_P_SEQEND &&
       pFunc->pCode[ nRecoverPos - 4 ] == ZH_P_SEQEND )
   {
      zh_compCodeTraceAddJump( cargo, nRecoverPos - 4 );
   }

   zh_compCodeTraceMark( cargo, nPCodePos, 4 );
   zh_compCodeTraceAddJump( cargo, nRecoverPos );

   return zh_compCodeTraceNextPos( cargo, nPCodePos + 4 );
}

static ZH_CODETRACE_FUNC( zh_p_seqend )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   zh_compCodeTraceMark( cargo, nPCodePos, 4 );

   return zh_compCodeTraceNextPos( cargo, nNewPos );
}

static ZH_CODETRACE_FUNC( zh_p_switch )
{
   ZH_USHORT usCases = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), us;
   ZH_SIZE nStart = nPCodePos, nNewPos;

   nPCodePos += 3;
   for( us = 0; us < usCases; ++us )
   {
      switch( pFunc->pCode[ nPCodePos ] )
      {
         case ZH_P_PUSHBYTE:
            nPCodePos += 2;
            break;
         case ZH_P_PUSHINT:
            nPCodePos += 3;
            break;
         case ZH_P_PUSHLONG:
         case ZH_P_PUSH_DATE:
            nPCodePos += 5;
            break;
         case ZH_P_PUSHLONGLONG:
            nPCodePos += 9;
            break;
         case ZH_P_PUSHSTRSHORT:
            nPCodePos += 2 + pFunc->pCode[ nPCodePos + 1 ];
            break;
         case ZH_P_PUSHSTR:
            nPCodePos += 3 + ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
            break;
         case ZH_P_PUSHSTRLARGE:
            nPCodePos += 4 + ZH_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] );
            break;
         case ZH_P_PUSHNIL:
            /* default clause */
            us = usCases;
            nPCodePos++;
            break;
      }
      switch( pFunc->pCode[ nPCodePos ] )
      {
         case ZH_P_JUMPNEAR:
            nNewPos = nPCodePos + ( signed char ) pFunc->pCode[ nPCodePos + 1 ];
            nPCodePos += 2;
            break;
         case ZH_P_JUMP:
            nNewPos = nPCodePos + ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
            nPCodePos += 3;
            break;
         /*case ZH_P_JUMPFAR:*/
         default:
            nNewPos = nPCodePos + ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
            nPCodePos += 4;
            break;
      }
      zh_compCodeTraceAddJump( cargo, nNewPos );
   }
   zh_compCodeTraceMark( cargo, nStart, nPCodePos - nStart );

   return zh_compCodeTraceNextPos( cargo, us > usCases ?
                                   cargo->nPCodeSize : nPCodePos );
}

static ZH_CODETRACE_FUNC( zh_p_endblock )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   zh_compCodeTraceMark( cargo, nPCodePos, 1 );
   return zh_compCodeTraceNextPos( cargo, cargo->nPCodeSize );
}

static ZH_CODETRACE_FUNC( zh_p_endproc )
{
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );

   zh_compCodeTraceMark( cargo, nPCodePos, 1 );
   return zh_compCodeTraceNextPos( cargo, cargo->nPCodeSize );
}

/* NOTE: The  order of functions have to match the order of opcodes
 *       mnemonics
 */
static const PZH_CODETRACE_FUNC s_codeTraceFuncTable[] =
{
   zh_p_default,               /* ZH_P_AND                   */
   zh_p_default,               /* ZH_P_ARRAYPUSH             */
   zh_p_default,               /* ZH_P_ARRAYPOP              */
   zh_p_default,               /* ZH_P_ARRAYDIM              */
   zh_p_default,               /* ZH_P_ARRAYGEN              */
   zh_p_default,               /* ZH_P_EQUAL                 */
   zh_p_endblock,              /* ZH_P_ENDBLOCK              */
   zh_p_endproc,               /* ZH_P_ENDPROC               */
   zh_p_default,               /* ZH_P_EXACTLYEQUAL          */
   zh_p_default,               /* ZH_P_FALSE                 */
   zh_p_default,               /* ZH_P_FORTEST               */
   zh_p_default,               /* ZH_P_FUNCTION              */
   zh_p_default,               /* ZH_P_FUNCTIONSHORT         */
   zh_p_default,               /* ZH_P_FRAME                 */
   zh_p_default,               /* ZH_P_FUNCPTR               */
   zh_p_default,               /* ZH_P_GREATER               */
   zh_p_default,               /* ZH_P_GREATEREQUAL          */
   zh_p_default,               /* ZH_P_DEC                   */
   zh_p_default,               /* ZH_P_DIVIDE                */
   zh_p_default,               /* ZH_P_DO                    */
   zh_p_default,               /* ZH_P_DOSHORT               */
   zh_p_default,               /* ZH_P_DUPLICATE             */
   zh_p_default,               /* ZH_P_PUSHTIMESTAMP         */
   zh_p_default,               /* ZH_P_INC                   */
   zh_p_default,               /* ZH_P_INSTRING              */
   zh_p_jumpnear,              /* ZH_P_JUMPNEAR              */
   zh_p_jump,                  /* ZH_P_JUMP                  */
   zh_p_jumpfar,               /* ZH_P_JUMPFAR               */
   zh_p_jumpfalsenear,         /* ZH_P_JUMPFALSENEAR         */
   zh_p_jumpfalse,             /* ZH_P_JUMPFALSE             */
   zh_p_jumpfalsefar,          /* ZH_P_JUMPFALSEFAR          */
   zh_p_jumptruenear,          /* ZH_P_JUMPTRUENEAR          */
   zh_p_jumptrue,              /* ZH_P_JUMPTRUE              */
   zh_p_jumptruefar,           /* ZH_P_JUMPTRUEFAR           */
   zh_p_default,               /* ZH_P_LESSEQUAL             */
   zh_p_default,               /* ZH_P_LESS                  */
   zh_p_default,               /* ZH_P_LINE                  */
   zh_p_default,               /* ZH_P_LOCALNAME             */
   zh_p_default,               /* ZH_P_MACROPOP              */
   zh_p_default,               /* ZH_P_MACROPOPALIASED       */
   zh_p_default,               /* ZH_P_MACRO_PUSH             */
   zh_p_default,               /* ZH_P_MACRO_ARRAY_GEN         */
   zh_p_default,               /* ZH_P_MACRO_PUSHLIST         */
   zh_p_default,               /* ZH_P_MACRO_PUSHINDEX        */
   zh_p_default,               /* ZH_P_MACRO_PUSHPARE         */
   zh_p_default,               /* ZH_P_MACRO_PUSHALIASED      */
   zh_p_default,               /* ZH_P_MACROSYMBOL           */
   zh_p_default,               /* ZH_P_MACROTEXT             */
   zh_p_default,               /* ZH_P_MESSAGE               */
   zh_p_default,               /* ZH_P_MINUS                 */
   zh_p_default,               /* ZH_P_MODULUS               */
   zh_p_default,               /* ZH_P_MODULENAME            */
                               /* start: pcodes generated by macro compiler */
   zh_p_default,               /* ZH_P_MMESSAGE              */
   zh_p_default,               /* ZH_P_MPOPALIASEDFIELD      */
   zh_p_default,               /* ZH_P_MPOPALIASEDVAR        */
   zh_p_default,               /* ZH_P_MPOPFIELD             */
   zh_p_default,               /* ZH_P_MPOPMEMVAR            */
   zh_p_default,               /* ZH_P_MPUSH_ALIASED_FIELD     */
   zh_p_default,               /* ZH_P_MPUSHALIASEDVAR       */
   zh_p_default,               /* ZH_P_MPUSH_BLOCK            */
   zh_p_default,               /* ZH_P_MPUSHFIELD            */
   zh_p_default,               /* ZH_P_MPUSHMEMVAR           */
   zh_p_default,               /* ZH_P_MPUSHMEMVARREF        */
   zh_p_default,               /* ZH_P_MPUSHSYM              */
   zh_p_default,               /* ZH_P_MPUSHVARIABLE         */
                               /* end: */
   zh_p_default,               /* ZH_P_MULT                  */
   zh_p_default,               /* ZH_P_NEGATE                */
   zh_p_default,               /* ZH_P_NOOP                  */
   zh_p_default,               /* ZH_P_NOT                   */
   zh_p_default,               /* ZH_P_NOTEQUAL              */
   zh_p_default,               /* ZH_P_OR                    */
   zh_p_default,               /* ZH_P_PARAMETER             */
   zh_p_default,               /* ZH_P_PLUS                  */
   zh_p_default,               /* ZH_P_POP                   */
   zh_p_default,               /* ZH_P_POPALIAS              */
   zh_p_default,               /* ZH_P_POPALIASEDFIELD       */
   zh_p_default,               /* ZH_P_POPALIASEDFIELDNEAR   */
   zh_p_default,               /* ZH_P_POPALIASEDVAR         */
   zh_p_default,               /* ZH_P_POPFIELD              */
   zh_p_default,               /* ZH_P_POPLOCAL              */
   zh_p_default,               /* ZH_P_POPLOCALNEAR          */
   zh_p_default,               /* ZH_P_POPMEMVAR             */
   zh_p_default,               /* ZH_P_POPSTATIC             */
   zh_p_default,               /* ZH_P_POPVAR           */
   zh_p_default,               /* ZH_P_POWER                 */
   zh_p_default,               /* ZH_P_PUSHALIAS             */
   zh_p_default,               /* ZH_P_PUSH_ALIASED_FIELD      */
   zh_p_default,               /* ZH_P_PUSH_ALIASED_FIELDNEAR  */
   zh_p_default,               /* ZH_P_PUSHALIASEDVAR        */
   zh_p_default,               /* ZH_P_PUSH_BLOCK             */
   zh_p_default,               /* ZH_P_PUSH_BLOCKSHORT        */
   zh_p_default,               /* ZH_P_PUSHFIELD             */
   zh_p_default,               /* ZH_P_PUSHBYTE              */
   zh_p_default,               /* ZH_P_PUSHINT               */
   zh_p_default,               /* ZH_P_PUSHLOCAL             */
   zh_p_default,               /* ZH_P_PUSHLOCALNEAR         */
   zh_p_default,               /* ZH_P_PUSHLOCALREF          */
   zh_p_default,               /* ZH_P_PUSHLONG              */
   zh_p_default,               /* ZH_P_PUSHMEMVAR            */
   zh_p_default,               /* ZH_P_PUSHMEMVARREF         */
   zh_p_default,               /* ZH_P_PUSHNIL               */
   zh_p_default,               /* ZH_P_PUSHDOUBLE            */
   zh_p_default,               /* ZH_P_PUSHSELF              */
   zh_p_default,               /* ZH_P_PUSHSTATIC            */
   zh_p_default,               /* ZH_P_PUSHSTATICREF         */
   zh_p_default,               /* ZH_P_PUSHSTR               */
   zh_p_default,               /* ZH_P_PUSHSTRSHORT          */
   zh_p_default,               /* ZH_P_PUSHSYM               */
   zh_p_default,               /* ZH_P_PUSHSYMNEAR           */
   zh_p_default,               /* ZH_P_PUSHVAR          */
   zh_p_default,               /* ZH_P_RETVALUE              */
   zh_p_default,               /* ZH_P_SEND                  */
   zh_p_default,               /* ZH_P_SENDSHORT             */
   zh_p_seqbegin,              /* ZH_P_SEQBEGIN              */
   zh_p_seqend,                /* ZH_P_SEQEND                */
   zh_p_default,               /* ZH_P_SEQRECOVER            */
   zh_p_default,               /* ZH_P_SFRAME                */
   zh_p_default,               /* ZH_P_STATICS               */
   zh_p_default,               /* ZH_P_STATICNAME            */
   zh_p_default,               /* ZH_P_SWAPALIAS             */
   zh_p_default,               /* ZH_P_TRUE                  */
   zh_p_default,               /* ZH_P_ZERO                  */
   zh_p_default,               /* ZH_P_ONE                   */
   zh_p_default,               /* ZH_P_MACRO_FUNC             */
   zh_p_default,               /* ZH_P_MACRODO               */
   zh_p_default,               /* ZH_P_MPUSHSTR              */
   zh_p_default,               /* ZH_P_LOCALNEARADDINT       */
   zh_p_default,               /* ZH_P_MACRO_PUSHREF          */
   zh_p_default,               /* ZH_P_PUSHLONGLONG          */
   zh_p_default,               /* ZH_P_ENUMSTART             */
   zh_p_default,               /* ZH_P_ENUMNEXT              */
   zh_p_default,               /* ZH_P_ENUMPREV              */
   zh_p_default,               /* ZH_P_ENUMEND               */
   zh_p_switch,                /* ZH_P_SWITCH                */
   zh_p_default,               /* ZH_P_PUSH_DATE              */
                               /* optimization of inlined math operations */
   zh_p_default,               /* ZH_P_PLUSEQPOP             */
   zh_p_default,               /* ZH_P_MINUSEQPOP            */
   zh_p_default,               /* ZH_P_MULTEQPOP             */
   zh_p_default,               /* ZH_P_DIVEQPOP              */
   zh_p_default,               /* ZH_P_PLUSEQ                */
   zh_p_default,               /* ZH_P_MINUSEQ               */
   zh_p_default,               /* ZH_P_MULTEQ                */
   zh_p_default,               /* ZH_P_DIVEQ                 */
   zh_p_default,               /* ZH_P_WITHOBJECTSTART       */
   zh_p_default,               /* ZH_P_WITHOBJECTMESSAGE     */
   zh_p_default,               /* ZH_P_WITHOBJECTEND         */
   zh_p_default,               /* ZH_P_MACRO_SEND             */
   zh_p_default,               /* ZH_P_PUSHOVARREF           */
   zh_p_default,               /* ZH_P_ARRAYPUSHREF          */
   zh_p_default,               /* ZH_P_VFRAME                */
   zh_p_default,               /* ZH_P_LARGEFRAME            */
   zh_p_default,               /* ZH_P_LARGEVFRAME           */
   zh_p_default,               /* ZH_P_PUSH_STR_HIDDEN         */
   zh_p_default,               /* ZH_P_LOCALADDINT           */
   zh_p_default,               /* ZH_P_MODEQPOP              */
   zh_p_default,               /* ZH_P_EXPEQPOP              */
   zh_p_default,               /* ZH_P_MODEQ                 */
   zh_p_default,               /* ZH_P_EXPEQ                 */
   zh_p_default,               /* ZH_P_DUPLUNREF             */
   zh_p_default,               /* ZH_P_MPUSH_BLOCKLARGE       */
   zh_p_default,               /* ZH_P_MPUSHSTRLARGE         */
   zh_p_default,               /* ZH_P_PUSH_BLOCKLARGE        */
   zh_p_default,               /* ZH_P_PUSHSTRLARGE          */
   zh_p_default,               /* ZH_P_SWAP                  */
   zh_p_default,               /* ZH_P_PUSHVPARAMS           */
   zh_p_default,               /* ZH_P_PUSHUNREF             */
   zh_p_seqalways,             /* ZH_P_SEQALWAYS             */
   zh_p_alwaysbegin,           /* ZH_P_ALWAYSBEGIN           */
   zh_p_default,               /* ZH_P_ALWAYSEND             */
   zh_p_default,               /* ZH_P_DECEQPOP              */
   zh_p_default,               /* ZH_P_INCEQPOP              */
   zh_p_default,               /* ZH_P_DECEQ                 */
   zh_p_default,               /* ZH_P_INCEQ                 */
   zh_p_default,               /* ZH_P_LOCALDEC              */
   zh_p_default,               /* ZH_P_LOCALINC              */
   zh_p_default,               /* ZH_P_LOCALINCPUSH          */
   zh_p_default,               /* ZH_P_PUSHFUNCSYM           */
   zh_p_default,               /* ZH_P_HASHGEN               */
   zh_p_default,               /* ZH_P_SEQBLOCK              */
   zh_p_default,               /* ZH_P_THREADSTATICS         */
   zh_p_default                /* ZH_P_PUSHAPARAMS           */
};

void zh_compCodeTraceMarkDead( ZH_COMP_DECL, PZH_ZFUNC pFunc )
{
   const PZH_CODETRACE_FUNC * pFuncTable = s_codeTraceFuncTable;
   ZH_CODETRACE_INFO code_info;

   if( ! ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_OPTJUMP ) || pFunc->nPCodePos < 2 )
      return;

   assert( ZH_P_LAST_PCODE == sizeof( s_codeTraceFuncTable ) / sizeof( PZH_CODETRACE_FUNC ) );

   code_info.pnJumps = NULL;
   code_info.nJumpPos = 0;
   code_info.nJumpSize = 0;
   code_info.nJumpCount = 0;
   code_info.nPCodeSize = pFunc->nPCodePos;
   code_info.fFinished = ZH_FALSE;

   code_info.pCodeMark = ( ZH_BYTE * ) zh_xgrabz( code_info.nPCodeSize );

   zh_compPCodeTrace( pFunc, ( const PZH_PCODE_FUNC * ) pFuncTable, ( void * ) &code_info );

   if( code_info.fFinished )
   {
      ZH_SIZE nPos = 0, nCount = 0;
      ZH_BYTE bLastCode = ZH_P_LAST_PCODE;

      do
      {
         if( code_info.pCodeMark[ nPos ] == 0 )
            ++nCount;
         else
         {
            bLastCode = pFunc->pCode[ nPos ];
            if( nCount )
            {
               zh_compNOOPfill( pFunc, nPos - nCount, nCount, ZH_FALSE, ZH_TRUE );
               nCount = 0;
            }
         }
      }
      while( ++nPos < code_info.nPCodeSize );

      /* do not strip the last ZH_P_ENDBLOCK / ZH_P_ENDPROC marker */
      if( nCount > 0 && bLastCode != ( pFunc->szName ? ZH_P_ENDPROC : ZH_P_ENDBLOCK ) )
      {
         --nPos;
         --nCount;
      }

      if( nCount > 0 )
      {
         /*
          * We cannot simply decrease size of the generated PCODE here
          * because jumps or noops tables may point to the this area
          * and we will have to update also the jump table, [druzus]
          */
         /*
            pFunc->pCode[ nPos - nCount ] = pFunc->pCode[ nPos - 1 ];
            pFunc->nPCodePos = pFunc->nPCodeSize = nPos - nCount + 1;
          */
         zh_compNOOPfill( pFunc, nPos - nCount, nCount, ZH_FALSE, ZH_TRUE );
      }
   }

   zh_xfree( code_info.pCodeMark );
   if( code_info.pnJumps )
      zh_xfree( code_info.pnJumps );
}
