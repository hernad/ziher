/*
 * Generate table with jump labels
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

#define ZH_LABEL_FUNC( func )  ZH_PCODE_FUNC( func, PZH_LABEL_INFO )
typedef ZH_LABEL_FUNC( ZH_LABEL_FUNC_ );
typedef ZH_LABEL_FUNC_ * PZH_LABEL_FUNC;

/*
 * jump functions
 */
static ZH_LABEL_FUNC( zh_p_jumpnear )
{
   ZH_SIZE nNewPos = nPCodePos + ( signed char ) pFunc->pCode[ nPCodePos + 1 ];

   cargo->pnLabels[ nNewPos ]++;
   return 2;
}

static ZH_LABEL_FUNC( zh_p_jump )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKSHORT( pAddr );

   cargo->pnLabels[ nNewPos ]++;
   return 3;
}

static ZH_LABEL_FUNC( zh_p_jumpfar )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   cargo->pnLabels[ nNewPos ]++;
   return 4;
}

static ZH_LABEL_FUNC( zh_p_jumpfalsenear )
{
   ZH_SIZE nNewPos = nPCodePos + ( signed char ) pFunc->pCode[ nPCodePos + 1 ];

   cargo->fCondJump = ZH_TRUE;
   cargo->pnLabels[ nNewPos ]++;
   return 2;
}

static ZH_LABEL_FUNC( zh_p_jumpfalse )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKSHORT( pAddr );

   cargo->fCondJump = ZH_TRUE;
   cargo->pnLabels[ nNewPos ]++;
   return 3;
}

static ZH_LABEL_FUNC( zh_p_jumpfalsefar )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   cargo->fCondJump = ZH_TRUE;
   cargo->pnLabels[ nNewPos ]++;
   return 4;
}

static ZH_LABEL_FUNC( zh_p_jumptruenear )
{
   ZH_SIZE nNewPos = nPCodePos + ( signed char ) pFunc->pCode[ nPCodePos + 1 ];

   cargo->fCondJump = ZH_TRUE;
   cargo->pnLabels[ nNewPos ]++;
   return 2;
}

static ZH_LABEL_FUNC( zh_p_jumptrue )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKSHORT( pAddr );

   cargo->fCondJump = ZH_TRUE;
   cargo->pnLabels[ nNewPos ]++;
   return 3;
}

static ZH_LABEL_FUNC( zh_p_jumptruefar )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nNewPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   cargo->fCondJump = ZH_TRUE;
   cargo->pnLabels[ nNewPos ]++;
   return 4;
}

static ZH_LABEL_FUNC( zh_p_seqalways )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nAlwaysPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   if( cargo->fSetSeqBegin )
      cargo->pnLabels[ nAlwaysPos ]++;
   return 4;
}

static ZH_LABEL_FUNC( zh_p_alwaysbegin )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nAlwaysEndPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   if( cargo->fSetSeqBegin )
      cargo->pnLabels[ nAlwaysEndPos ]++;
   return 4;
}

static ZH_LABEL_FUNC( zh_p_seqbegin )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SIZE nRecoverPos = nPCodePos + ZH_PCODE_MKINT24( pAddr );

   if( cargo->fSetSeqBegin )
      cargo->pnLabels[ nRecoverPos ]++;
   return 4;
}

static ZH_LABEL_FUNC( zh_p_seqend )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_ISIZ nOffset = ZH_PCODE_MKINT24( pAddr );
   ZH_SIZE nNewPos = nPCodePos + nOffset;

   if( cargo->fSetSeqBegin || nOffset != 4 )
      cargo->pnLabels[ nNewPos ]++;
   return 4;
}

/* NOTE: The  order of functions have to match the order of opcodes
 *       mnemonics
 */
static const PZH_LABEL_FUNC s_GenLabelFuncTable[] =
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
   zh_p_jumpnear,              /* ZH_P_JUMPNEAR              */
   zh_p_jump,                  /* ZH_P_JUMP                  */
   zh_p_jumpfar,               /* ZH_P_JUMPFAR               */
   zh_p_jumpfalsenear,         /* ZH_P_JUMPFALSENEAR         */
   zh_p_jumpfalse,             /* ZH_P_JUMPFALSE             */
   zh_p_jumpfalsefar,          /* ZH_P_JUMPFALSEFAR          */
   zh_p_jumptruenear,          /* ZH_P_JUMPTRUENEAR          */
   zh_p_jumptrue,              /* ZH_P_JUMPTRUE              */
   zh_p_jumptruefar,           /* ZH_P_JUMPTRUEFAR           */
   NULL,                       /* ZH_P_LESSEQUAL             */
   NULL,                       /* ZH_P_LESS                  */
   NULL,                       /* ZH_P_LINE                  */
   NULL,                       /* ZH_P_LOCALNAME             */
   NULL,                       /* ZH_P_MACROPOP              */
   NULL,                       /* ZH_P_MACROPOPALIASED       */
   NULL,                       /* ZH_P_MACROPUSH             */
   NULL,                       /* ZH_P_MACROARRAYGEN         */
   NULL,                       /* ZH_P_MACROPUSHLIST         */
   NULL,                       /* ZH_P_MACROPUSHINDEX        */
   NULL,                       /* ZH_P_MACROPUSHPARE         */
   NULL,                       /* ZH_P_MACROPUSHALIASED      */
   NULL,                       /* ZH_P_MACROSYMBOL           */
   NULL,                       /* ZH_P_MACROTEXT             */
   NULL,                       /* ZH_P_MESSAGE               */
   NULL,                       /* ZH_P_MINUS                 */
   NULL,                       /* ZH_P_MODULUS               */
   NULL,                       /* ZH_P_MODULENAME            */
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
   NULL,                       /* ZH_P_POPVARIABLE           */
   NULL,                       /* ZH_P_POWER                 */
   NULL,                       /* ZH_P_PUSHALIAS             */
   NULL,                       /* ZH_P_PUSH_ALIASED_FIELD      */
   NULL,                       /* ZH_P_PUSH_ALIASED_FIELDNEAR  */
   NULL,                       /* ZH_P_PUSHALIASEDVAR        */
   NULL,                       /* ZH_P_PUSH_BLOCK             */
   NULL,                       /* ZH_P_PUSH_BLOCKSHORT        */
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
   NULL,                       /* ZH_P_PUSHSTR               */
   NULL,                       /* ZH_P_PUSHSTRSHORT          */
   NULL,                       /* ZH_P_PUSHSYM               */
   NULL,                       /* ZH_P_PUSHSYMNEAR           */
   NULL,                       /* ZH_P_PUSHVARIABLE          */
   NULL,                       /* ZH_P_RETVALUE              */
   NULL,                       /* ZH_P_SEND                  */
   NULL,                       /* ZH_P_SENDSHORT             */
   zh_p_seqbegin,              /* ZH_P_SEQBEGIN              */
   zh_p_seqend,                /* ZH_P_SEQEND                */
   NULL,                       /* ZH_P_SEQRECOVER            */
   NULL,                       /* ZH_P_SFRAME                */
   NULL,                       /* ZH_P_STATICS               */
   NULL,                       /* ZH_P_STATICNAME            */
   NULL,                       /* ZH_P_SWAPALIAS             */
   NULL,                       /* ZH_P_TRUE                  */
   NULL,                       /* ZH_P_ZERO                  */
   NULL,                       /* ZH_P_ONE                   */
   NULL,                       /* ZH_P_MACROFUNC             */
   NULL,                       /* ZH_P_MACRODO               */
   NULL,                       /* ZH_P_MPUSHSTR              */
   NULL,                       /* ZH_P_LOCALNEARADDINT       */
   NULL,                       /* ZH_P_MACROPUSHREF          */
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
   NULL,                       /* ZH_P_PUSH_STR_HIDDEN         */
   NULL,                       /* ZH_P_LOCALADDINT           */
   NULL,                       /* ZH_P_MODEQPOP              */
   NULL,                       /* ZH_P_EXPEQPOP              */
   NULL,                       /* ZH_P_MODEQ                 */
   NULL,                       /* ZH_P_EXPEQ                 */
   NULL,                       /* ZH_P_DUPLUNREF             */
   NULL,                       /* ZH_P_MPUSH_BLOCKLARGE       */
   NULL,                       /* ZH_P_MPUSHSTRLARGE         */
   NULL,                       /* ZH_P_PUSH_BLOCKLARGE        */
   NULL,                       /* ZH_P_PUSHSTRLARGE          */
   NULL,                       /* ZH_P_SWAP                  */
   NULL,                       /* ZH_P_PUSHVPARAMS           */
   NULL,                       /* ZH_P_PUSHUNREF             */
   zh_p_seqalways,             /* ZH_P_SEQALWAYS             */
   zh_p_alwaysbegin,           /* ZH_P_ALWAYSBEGIN           */
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
   NULL,                       /* ZH_P_THREADSTATICS         */
   NULL                        /* ZH_P_PUSHAPARAMS           */
};

void zh_compGenLabelTable( PZH_ZFUNC pFunc, PZH_LABEL_INFO label_info )
{
   const PZH_LABEL_FUNC * pFuncTable = s_GenLabelFuncTable;
   ZH_SIZE nLabel = 0, n;

   assert( ZH_P_LAST_PCODE == sizeof( s_GenLabelFuncTable ) / sizeof( PZH_LABEL_FUNC ) );

   zh_compPCodeEval( pFunc, ( const PZH_PCODE_FUNC * ) pFuncTable, ( void * ) label_info );

   for( n = 0; n < pFunc->nPCodePos; ++n )
   {
      if( label_info->pnLabels[ n ] )
      {
         label_info->pnLabels[ n ] = ++nLabel;
      }
   }
}
