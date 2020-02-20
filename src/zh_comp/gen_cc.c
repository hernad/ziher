/*
 * Compiler C source with real code generation
 *
 * Copyright 2006-2009 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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
#include "zh_date.h"
#include "zh_assert.h"

#define ZH_GENC_FUNC( func )   ZH_PCODE_FUNC( func, PZH_LABEL_INFO )
typedef ZH_GENC_FUNC( ZH_GENC_FUNC_ );
typedef ZH_GENC_FUNC_ * PZH_GENC_FUNC;

#define ZH_GENC_GETLABEL( l )  ( ( l ) < pFunc->nPCodePos ? cargo->pnLabels[ ( l ) ] : 0 )

#define ZH_GENC_LABEL()  \
   do \
   { \
      ZH_SIZE nLab = ZH_GENC_GETLABEL( nPCodePos ); \
      if( nLab != 0 ) \
         fprintf( cargo->yyc, "lab%05" ZH_PFS "u: ;\n", nLab ); \
   } while( 0 )

#define ZH_GENC_ERROR( s )  \
   do \
   { \
      fprintf( cargo->yyc, "\t#error: \"" s "\"\n" ); \
   } while( 0 )

void zh_compGenCString( FILE * yyc, const ZH_BYTE * pText, ZH_SIZE nLen )
{
   ZH_SIZE nPos;

   fputc( '"', yyc );
   for( nPos = 0; nPos < nLen; nPos++ )
   {
      ZH_BYTE uchr = ( ZH_BYTE ) pText[ nPos ];
      /*
       * NOTE: After optimization some Chr( n ) can be converted
       *       into a string containing non-printable characters.
       *
       * ? is escaped to avoid conflicts with trigraph sequences which
       * are part of ANSI C standard
       */
      if( uchr == '"' || uchr == '\\' || uchr == '?' )
         fprintf( yyc, "\\%c", uchr );
      else if( uchr < ( ZH_BYTE ) ' ' || uchr >= 127 )
      {
         ZH_BYTE uchrnext = nPos < nLen - 1 ? pText[ nPos + 1 ] : 0;

         fprintf( yyc, "\\x%02X%s", uchr,
                  ( uchrnext >= ( ZH_BYTE ) '0' && uchrnext <= ( ZH_BYTE ) '9' ) ||
                  ( uchrnext >= ( ZH_BYTE ) 'a' && uchrnext <= ( ZH_BYTE ) 'z' ) ||
                  ( uchrnext >= ( ZH_BYTE ) 'A' && uchrnext <= ( ZH_BYTE ) 'Z' ) ? "\" \"" : "" );
      }
      else
         fprintf( yyc, "%c", uchr );
   }
   fputc( '"', yyc );
}

static void zh_compGenCStrData( FILE * yyc, const ZH_BYTE * pText, ZH_SIZE nLen,
                                int iMethod )
{
#ifdef __ZH_CSTRING_SIZE_MAX
   #if __ZH_CSTRING_SIZE_MAX - 0 < 1
      #undef __ZH_CSTRING_SIZE_MAX
      #define __ZH_CSTRING_SIZE_MAX  4096
   #endif
   if( nLen > __ZH_CSTRING_SIZE_MAX )
   {
      ZH_SIZE nPos;

      fprintf( yyc, "\t{\tconst unsigned char str[ %" ZH_PFS "u ] = {", nLen + 1 );
      for( nPos = 0; nPos < nLen; nPos++ )
      {
         if( ( nPos & 0x0F ) == 0 )
            fprintf( yyc, "\n\t\t" );
         fprintf( yyc, "%d,", ( int ) pText[ nPos ] );
      }
      fprintf( yyc, "0 };\n\t\tzh_xvmPushString" );
      if( iMethod < 0 )
         fprintf( yyc, "Const( " );
      else
         fprintf( yyc, "Hidden( %d, ", iMethod );
      fprintf( yyc, "( const char * ) str,  %" ZH_PFS "u );\n\t}\n", nLen );
   }
   else
#endif
   {
      fprintf( yyc, "\tzh_xvmPushString" );
      if( iMethod < 0 )
         fprintf( yyc, "Const( " );
      else
         fprintf( yyc, "Hidden( %d, ", iMethod );
      zh_compGenCString( yyc, pText, nLen );
      fprintf( yyc, ", %" ZH_PFS "u );\n", nLen );
   }
}

static void zh_gencc_copyLocals( FILE * yyc, int iLocal1, int iLocal2 )
{
   if( iLocal1 != iLocal2 )
      fprintf( yyc, "\tzh_xvmCopyLocals( %d, %d );\n", iLocal1, iLocal2 );
}

static int zh_gencc_checkJumpCondAhead( ZH_LONG lValue, PZH_ZFUNC pFunc, ZH_SIZE nPCodePos, PZH_LABEL_INFO cargo,
                                        const char * szFunc )
{
   if( ZH_GENC_GETLABEL( nPCodePos + 1 ) == 0 )
   {
      ZH_ISIZ nOffset = 0;
      ZH_BOOL fNot = ZH_FALSE;
      int iSize = 0;

      switch( pFunc->pCode[ nPCodePos + 1 ] )
      {
         case ZH_P_JUMPFALSENEAR:
            nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 2 ] );
            fNot = ZH_TRUE;
            iSize = 3;
            break;

         case ZH_P_JUMPFALSE:
            nOffset = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 2 ] );
            fNot = ZH_TRUE;
            iSize = 4;
            break;

         case ZH_P_JUMPFALSEFAR:
            nOffset = ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 2 ] );
            fNot = ZH_TRUE;
            iSize = 5;
            break;

         case ZH_P_JUMPTRUENEAR:
            nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 2 ] );
            iSize = 3;
            break;

         case ZH_P_JUMPTRUE:
            nOffset = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 2 ] );
            iSize = 4;
            break;

         case ZH_P_JUMPTRUEFAR:
            nOffset = ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 2 ] );
            iSize = 5;
            break;
      }

      if( iSize )
      {
         fprintf( cargo->yyc, "\tif( zh_xvm%sIntIs( %ldL, &fValue ) ) break;\n",
                  szFunc, lValue );
         fprintf( cargo->yyc, "\tif( %sfValue )\n\t\tgoto lab%05" ZH_PFS "u;\n",
                  fNot ? "!" : "", ZH_GENC_GETLABEL( nPCodePos + 1 + nOffset ) );
         return iSize;
      }
   }
   fprintf( cargo->yyc, "\tif( zh_xvm%sInt( %ldL ) ) break;\n",
            szFunc, lValue );
   return 1;
}

static int zh_gencc_checkNumAhead( ZH_LONG lValue, PZH_ZFUNC pFunc, ZH_SIZE nPCodePos, PZH_LABEL_INFO cargo )
{
   if( ZH_GENC_GETLABEL( nPCodePos ) == 0 )
   {
      switch( pFunc->pCode[ nPCodePos ] )
      {
         case ZH_P_POPLOCAL:
            fprintf( cargo->yyc, "\tzh_xvmLocalSetInt( %d, %ldL );\n",
                     ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                     lValue );
            return 3;

         case ZH_P_POPLOCALNEAR:
            fprintf( cargo->yyc, "\tzh_xvmLocalSetInt( %d, %ldL );\n",
                     ( signed char ) pFunc->pCode[ nPCodePos + 1 ], lValue );
            return 2;

         case ZH_P_EQUAL:
         case ZH_P_EXACTLYEQUAL:
            return zh_gencc_checkJumpCondAhead( lValue, pFunc, nPCodePos, cargo, "Equal" );

         case ZH_P_NOTEQUAL:
            return zh_gencc_checkJumpCondAhead( lValue, pFunc, nPCodePos, cargo, "NotEqual" );

         case ZH_P_GREATER:
            return zh_gencc_checkJumpCondAhead( lValue, pFunc, nPCodePos, cargo, "GreaterThen" );

         case ZH_P_GREATEREQUAL:
            return zh_gencc_checkJumpCondAhead( lValue, pFunc, nPCodePos, cargo, "GreaterEqualThen" );

         case ZH_P_LESS:
            return zh_gencc_checkJumpCondAhead( lValue, pFunc, nPCodePos, cargo, "LessThen" );

         case ZH_P_LESSEQUAL:
            return zh_gencc_checkJumpCondAhead( lValue, pFunc, nPCodePos, cargo, "LessEqualThen" );

         case ZH_P_ARRAYPUSH:
            if( lValue > 0 )
            {
               fprintf( cargo->yyc, "\tif( zh_xvmArrayItemPush( %ldL ) ) break;\n", lValue );
               return 1;
            }
            break;

         case ZH_P_ARRAYPOP:
            if( lValue > 0 )
            {
               fprintf( cargo->yyc, "\tif( zh_xvmArrayItemPop( %ldL ) ) break;\n", lValue );
               return 1;
            }
            break;

         case ZH_P_MULT:
            fprintf( cargo->yyc, "\tif( zh_xvmMultByInt( %ldL ) ) break;\n", lValue );
            return 1;

         case ZH_P_DIVIDE:
            fprintf( cargo->yyc, "\tif( zh_xvmDivideByInt( %ldL ) ) break;\n", lValue );
            return 1;

         case ZH_P_MODULUS:
            fprintf( cargo->yyc, "\tif( zh_xvmModulusByInt( %ldL ) ) break;\n", lValue );
            return 1;

         case ZH_P_MINUS:
            if( lValue > 0 )
            {
               fprintf( cargo->yyc, "\tif( zh_xvmAddInt( -%ldL ) ) break;\n", lValue );
               return 1;
            }
#if -LONG_MAX > LONG_MIN
            else if( lValue < -LONG_MAX )
               break;
#endif
            lValue = -lValue;
            /* fallthrough */

         case ZH_P_PLUS:
            fprintf( cargo->yyc, "\tif( zh_xvmAddInt( %ldL ) ) break;\n", lValue );
            return 1;

         case ZH_P_RETVALUE:
            fprintf( cargo->yyc, "\tzh_xvmRetInt( %ldL );\n", lValue );
            return 1;
      }
   }
   return 0;
}

static int zh_gencc_checkPlusAhead( PZH_ZFUNC pFunc, ZH_SIZE nPCodePos, PZH_LABEL_INFO cargo )
{
   if( ZH_GENC_GETLABEL( nPCodePos ) == 0 )
   {
      switch( pFunc->pCode[ nPCodePos ] )
      {
         case ZH_P_POPLOCALNEAR:
            fprintf( cargo->yyc, "\tzh_xvmLocalAdd( %d );\n",
                     ( signed char ) pFunc->pCode[ nPCodePos + 1 ] );
            return 2;

         case ZH_P_POPLOCAL:
            fprintf( cargo->yyc, "\tzh_xvmLocalAdd( %d );\n",
                     ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
            return 3;

         case ZH_P_POPSTATIC:
            fprintf( cargo->yyc, "\tzh_xvmStaticAdd( %hu );\n",
                     ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
            return 3;

         case ZH_P_POPMEMVAR:
            fprintf( cargo->yyc, "\tzh_xvmMemvarAdd( symbols + %hu );\n",
                     ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
            return 3;
      }
   }
   return 0;
}

static ZH_GENC_FUNC( zh_p_and )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmAnd() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_arraypush )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmArrayPush() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_arraypushref )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmArrayPushRef() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_arraypop )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmArrayPop() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_dec )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmDec() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_arraydim )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmArrayDim( %hu );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_divide )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmDivide() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_do )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmDo( %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_doshort )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmDo( %u ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_duplicate )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmDuplicate();\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_duplunref )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmDuplUnRef();\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pushunref )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushUnRef();\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_swap )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmSwap( %u );\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_equal )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmEqual() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_exactlyequal )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmExactlyEqual() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_endblock )
{
   ZH_GENC_LABEL();

   ZH_GENC_ERROR( "ZH_P_ENDBLOCK" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_endproc )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\t/* *** END PROC *** */\n" );
   if( nPCodePos < pFunc->nPCodePos - 1 )
   {
      if( cargo->iNestedBlock )
      {
         cargo->fEndRequest = ZH_TRUE;
         fprintf( cargo->yyc, "\tzh_xvmEndProc();\n" );
      }
      fprintf( cargo->yyc, "\tbreak;\n" );
   }
   return 1;
}

static ZH_GENC_FUNC( zh_p_false )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushLogical( ZH_FALSE );\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_fortest )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmForTest() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_frame )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmFrame( %u, %u );\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static ZH_GENC_FUNC( zh_p_funcptr )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmFuncPtr();\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_function )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmFunction( %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_functionshort )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmFunction( %u ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_arraygen )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmArrayGen( %hu );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_hashgen )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmHashGen( %hu );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_greater )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmGreater() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_greaterequal )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmGreaterEqual() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_inc )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmInc() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_instring )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmInstring() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_jumpnear )
{
   ZH_ISIZ nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 1 ] );

   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tgoto lab%05" ZH_PFS "u;\n",
            ZH_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 2;
}

static ZH_GENC_FUNC( zh_p_jump )
{
   ZH_ISIZ nOffset = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );

   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tgoto lab%05" ZH_PFS "u;\n",
            ZH_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_jumpfar )
{
   ZH_ISIZ nOffset = ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );

   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tgoto lab%05" ZH_PFS "u;\n",
            ZH_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 4;
}

static ZH_GENC_FUNC( zh_p_jumpfalsenear )
{
   ZH_ISIZ nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 1 ] );

   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopLogical( &fValue ) ) break;\n\tif( ! fValue )\n\t\tgoto lab%05" ZH_PFS "u;\n",
            ZH_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 2;
}

static ZH_GENC_FUNC( zh_p_jumpfalse )
{
   ZH_ISIZ nOffset = ZH_PCODE_MKSHORT( &( pFunc->pCode[ nPCodePos + 1 ] ) );

   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopLogical( &fValue ) ) break;\n\tif( ! fValue )\n\t\tgoto lab%05" ZH_PFS "u;\n",
            ZH_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_jumpfalsefar )
{
   ZH_ISIZ nOffset = ZH_PCODE_MKINT24( &( pFunc->pCode[ nPCodePos + 1 ] ) );

   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopLogical( &fValue ) ) break;\n\tif( ! fValue )\n\t\tgoto lab%05" ZH_PFS "u;\n",
            ZH_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 4;
}

static ZH_GENC_FUNC( zh_p_jumptruenear )
{
   ZH_ISIZ nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 1 ] );

   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopLogical( &fValue ) ) break;\n\tif( fValue )\n\t\tgoto lab%05" ZH_PFS "u;\n",
            ZH_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 2;
}

static ZH_GENC_FUNC( zh_p_jumptrue )
{
   ZH_ISIZ nOffset = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );

   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopLogical( &fValue ) ) break;\n\tif( fValue )\n\t\tgoto lab%05" ZH_PFS "u;\n",
            ZH_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_jumptruefar )
{
   ZH_ISIZ nOffset = ZH_PCODE_MKINT24( &( pFunc->pCode[ nPCodePos + 1 ] ) );

   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopLogical( &fValue ) ) break;\n\tif( fValue )\n\t\tgoto lab%05" ZH_PFS "u;\n",
            ZH_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 4;
}

static ZH_GENC_FUNC( zh_p_less )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmLess() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_lessequal )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmLessEqual() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_line )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmSetLine( %hu );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_localname )
{
   ZH_USHORT usLen;

   ZH_GENC_LABEL();

   usLen = ( ZH_USHORT ) strlen( ( char * ) &pFunc->pCode[ nPCodePos + 3 ] );
   fprintf( cargo->yyc, "\tzh_xvmLocalName( %hu, ",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   zh_compGenCString( cargo->yyc, &pFunc->pCode[ nPCodePos + 3 ], usLen );
   fprintf( cargo->yyc, " );\n" );
   return usLen + 4;
}

static ZH_GENC_FUNC( zh_p_macropop )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroPop( %u ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_macropopaliased )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroPopAliased( %u ) ) break;\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_macropush )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroPush( %u ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_macropushref )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroPushRef() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_macrodo )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroDo( %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_macro_func )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroFunc( %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_macrosend )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroSend( %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_macro_array_gen )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroArrayGen( %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_macropushlist )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroPushList( %u ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_macropushindex )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroPushIndex() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_macropushpare )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroPushPare( %u ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_macropushaliased )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroPushAliased( %u ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_macrosymbol )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroSymbol() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_macrotext )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMacroText() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_message )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushSymbol( symbols + %hu );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_minus )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMinus() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_modulename )
{
   ZH_USHORT usLen;

   ZH_GENC_LABEL();

   usLen = ( ZH_USHORT ) strlen( ( char * ) &pFunc->pCode[ nPCodePos + 1 ] );
   fprintf( cargo->yyc, "\tzh_xvmModuleName( " );
   zh_compGenCString( cargo->yyc, &pFunc->pCode[ nPCodePos + 1 ], usLen );
   fprintf( cargo->yyc, " );\n" );
   return usLen + 2;
}

static ZH_GENC_FUNC( zh_p_modulus )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmModulus() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_mult )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMult() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_negate )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmNegate() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_not )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmNot() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_notequal )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmNotEqual() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_or )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmOr() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_parameter )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmParameter( symbols + %hu, %u );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
            pFunc->pCode[ nPCodePos + 3 ] );
   return 4;
}

static ZH_GENC_FUNC( zh_p_plus )
{
   int iSkip;

   ZH_GENC_LABEL();

   iSkip = zh_gencc_checkPlusAhead( pFunc, nPCodePos + 1, cargo );

   if( iSkip != 0 )
      return 1 + iSkip;

   fprintf( cargo->yyc, "\tif( zh_xvmPlus() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pop )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_stackPop();\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_popalias )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopAlias() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_popaliasedfield )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopAliasedField( symbols + %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_popaliasedfieldnear )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopAliasedField( symbols + %u ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_popaliasedvar )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopAliasedVar( symbols + %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_popfield )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopField( symbols + %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_poplocal )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPopLocal( %hd );\n",
            ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_poplocalnear )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPopLocal( %d );\n",
            ( signed char ) pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_popmemvar )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopMemvar( symbols + %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_popstatic )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPopStatic( %hu );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_popvariable )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPopVariable( symbols + %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_power )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPower() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pushalias )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPushAlias() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pushaliasedfield )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPushAliasedField( symbols + %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushaliasedfieldnear )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPushAliasedField( symbols + %u ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_pushaliasedvar )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPushAliasedVar( symbols + %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_push_blockshort )
{
   ZH_USHORT usSize, us;

   ZH_GENC_LABEL();

   usSize = pFunc->pCode[ nPCodePos + 1 ] - 2;
   nPCodePos += 2;

   fprintf( cargo->yyc, "\t{\n\t\tstatic const ZH_BYTE codeblock[ %hu ] = {", usSize );

   for( us = 0; us < usSize; ++us )
   {
      if( ( us & 0x0f ) == 0 )
         fprintf( cargo->yyc, "\n\t\t\t" );
      if( us == usSize - 1 )
         fprintf( cargo->yyc, "%u", pFunc->pCode[ nPCodePos + us ] );
      else
         fprintf( cargo->yyc, "%u, ", pFunc->pCode[ nPCodePos + us ] );
   }
   fprintf( cargo->yyc, " };\n\t\tzh_xvmPushBlockShort( codeblock, symbols );\n\t}\n" );

   return 2 + usSize;
}

static ZH_GENC_FUNC( zh_p_push_block )
{
   ZH_USHORT usSize, us;

   ZH_GENC_LABEL();

   usSize = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) - 3;
   nPCodePos += 3;

   fprintf( cargo->yyc, "\t{\n\t\tstatic const ZH_BYTE codeblock[ %hu ] = {", usSize );

   for( us = 0; us < usSize; ++us )
   {
      if( ( us & 0x0f ) == 0 )
         fprintf( cargo->yyc, "\n\t\t\t" );
      if( us == usSize - 1 )
         fprintf( cargo->yyc, "%u", pFunc->pCode[ nPCodePos + us ] );
      else
         fprintf( cargo->yyc, "%u, ", pFunc->pCode[ nPCodePos + us ] );
   }
   fprintf( cargo->yyc, " };\n\t\tzh_xvmPushBlock( codeblock, symbols );\n\t}\n" );

   return 3 + usSize;
}

static ZH_GENC_FUNC( zh_p_push_blocklarge )
{
   ZH_SIZE nSize, ul;

   ZH_GENC_LABEL();

   nSize = ZH_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] ) - 4;
   nPCodePos += 4;

   fprintf( cargo->yyc, "\t{\n\t\tstatic const ZH_BYTE codeblock[ %" ZH_PFS "u ] = {", nSize );

   for( ul = 0; ul < nSize; ++ul )
   {
      if( ( ul & 0x0f ) == 0 )
         fprintf( cargo->yyc, "\n\t\t\t" );
      if( ul == nSize - 1 )
         fprintf( cargo->yyc, "%u", pFunc->pCode[ nPCodePos + ul ] );
      else
         fprintf( cargo->yyc, "%u, ", pFunc->pCode[ nPCodePos + ul ] );
   }
   fprintf( cargo->yyc, " };\n\t\tzh_xvmPushBlock( codeblock, symbols );\n\t}\n" );

   return 4 + nSize;
}

static ZH_GENC_FUNC( zh_p_pushdouble )
{
   ZH_GENC_LABEL();


   /*
    * This version keeps double calculation compatible with RT FL functions
    */
   fprintf( cargo->yyc, "\tzh_xvmPushDouble( * ( double * ) " );
   {
      double d = ZH_PCODE_MKDOUBLE( &pFunc->pCode[ nPCodePos + 1 ] );
      zh_compGenCString( cargo->yyc, ( const ZH_BYTE * ) &d, sizeof( double ) );
   }
   fprintf( cargo->yyc, ", %u, %u );\n",
            pFunc->pCode[ nPCodePos + 1 + sizeof( double ) ],
            pFunc->pCode[ nPCodePos + 1 + sizeof( double ) + sizeof( ZH_BYTE ) ] );

   return sizeof( double ) + sizeof( ZH_BYTE ) + sizeof( ZH_BYTE ) + 1;
}

static ZH_GENC_FUNC( zh_p_pushfield )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPushField( symbols + %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushbyte )
{
   int iVal = ( signed char ) pFunc->pCode[ nPCodePos + 1 ], iSkip;

   ZH_GENC_LABEL();

   iSkip = zh_gencc_checkNumAhead( iVal, pFunc, nPCodePos + 2, cargo );

   if( iSkip == 0 )
      fprintf( cargo->yyc, "\tzh_xvmPushInteger( %d );\n", iVal );
   return 2 + iSkip;
}

static ZH_GENC_FUNC( zh_p_pushint )
{
   int iVal = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), iSkip;

   ZH_GENC_LABEL();

   iSkip = zh_gencc_checkNumAhead( iVal, pFunc, nPCodePos + 3, cargo );

   if( iSkip == 0 )
      fprintf( cargo->yyc, "\tzh_xvmPushInteger( %d );\n", iVal );
   return 3 + iSkip;
}

static ZH_GENC_FUNC( zh_p_pushlocal )
{
   ZH_GENC_LABEL();

   if( ZH_GENC_GETLABEL( nPCodePos + 3 ) == 0 )
   {
      switch( pFunc->pCode[ nPCodePos + 3 ] )
      {
         case ZH_P_POPLOCALNEAR:
            zh_gencc_copyLocals( cargo->yyc,
                        ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                        ( signed char ) pFunc->pCode[ nPCodePos + 4 ] );
            return 5;

         case ZH_P_POPLOCAL:
            zh_gencc_copyLocals( cargo->yyc,
                        ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                        ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 4 ] ) );
            return 6;
      }
   }

   fprintf( cargo->yyc, "\tzh_xvmPushLocal( %d );\n",
            ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushlocalnear )
{
   ZH_GENC_LABEL();

   if( ZH_GENC_GETLABEL( nPCodePos + 2 ) == 0 )
   {
      switch( pFunc->pCode[ nPCodePos + 2 ] )
      {
         case ZH_P_POPLOCALNEAR:
            zh_gencc_copyLocals( cargo->yyc,
                        ( signed char ) pFunc->pCode[ nPCodePos + 1 ],
                        ( signed char ) pFunc->pCode[ nPCodePos + 3 ] );
            return 4;

         case ZH_P_POPLOCAL:
            zh_gencc_copyLocals( cargo->yyc,
                        ( signed char ) pFunc->pCode[ nPCodePos + 1 ],
                        ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
            return 5;
      }
   }

   fprintf( cargo->yyc, "\tzh_xvmPushLocal( %d );\n",
            ( signed char ) pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_pushlocalref )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushLocalByRef( %d );\n",
            ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushlong )
{
   ZH_LONG lVal = ZH_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ), iSkip;

   ZH_GENC_LABEL();

   iSkip = zh_gencc_checkNumAhead( lVal, pFunc, nPCodePos + 5, cargo );

   if( iSkip == 0 )
   {
      fprintf( cargo->yyc, "#if INT_MAX >= INT32_MAX\n" );
      fprintf( cargo->yyc, "\tzh_xvmPushInteger( %ld );\n", lVal );
      fprintf( cargo->yyc, "#else\n" );
      fprintf( cargo->yyc, "\tzh_xvmPushLong( %ldL );\n", lVal );
      fprintf( cargo->yyc, "#endif\n" );
   }
   return 5 + iSkip;
}

static ZH_GENC_FUNC( zh_p_pushlonglong )
{
#ifdef ZH_LONG_LONG_OFF
   ZH_GENC_LABEL();
   fprintf( cargo->yyc, "\tzh_xvmPushLongLong( %.1f );\n", ZH_PCODE_MKLONGLONG( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 9;
#elif LONG_MAX < LONGLONG_MAX
   ZH_LONGLONG llVal;
   char szBuf[ 24 ];

   ZH_GENC_LABEL();

   llVal = ZH_PCODE_MKLONGLONG( &pFunc->pCode[ nPCodePos + 1 ] );
   fprintf( cargo->yyc, "\tzh_xvmPushLongLong( ZH_LL( %s ) );\n",
            zh_numToStr( szBuf, sizeof( szBuf ), llVal ) );
   return 9;
#else
   ZH_LONGLONG llVal;
   char szBuf[ 24 ];
   int iSkip;

   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "#if LONG_MAX >= LONGLONG_MAX\n" );
   llVal = ZH_PCODE_MKLONGLONG( &pFunc->pCode[ nPCodePos + 1 ] );
   iSkip = zh_gencc_checkNumAhead( llVal, pFunc, nPCodePos + 9, cargo );

   if( iSkip == 0 )
      fprintf( cargo->yyc, "\tzh_xvmPushLong( %ldL );\n", ( long ) llVal );
   fprintf( cargo->yyc, "#else\n" );
   fprintf( cargo->yyc, "\tzh_xvmPushLongLong( ZH_LL( %s ) );\n",
            zh_numToStr( szBuf, sizeof( szBuf ), llVal ) );
   if( iSkip > 0 )
   {
      int iDone = 0;
      while( iDone < iSkip )
      {
         ZH_BYTE opcode = pFunc->pCode[ nPCodePos + 9 + iDone ];
         if( opcode >= ZH_P_LAST_PCODE )
            break;
         iDone += ( int ) cargo->pFuncTable[ opcode ]( pFunc, nPCodePos + 9 + iDone, cargo );
      }
      if( iDone != iSkip )
         ZH_GENC_ERROR( "PCODE mismatch" );
   }
   fprintf( cargo->yyc, "#endif\n" );
   return 9 + iSkip;
#endif
}

static ZH_GENC_FUNC( zh_p_pushmemvar )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPushMemvar( symbols + %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushmemvarref )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPushMemvarByRef( symbols + %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushnil )
{
   ZH_GENC_LABEL();

   if( pFunc->pCode[ nPCodePos + 1 ] == ZH_P_RETVALUE &&
       ZH_GENC_GETLABEL( nPCodePos + 1 ) == 0 )
   {
      fprintf( cargo->yyc, "\tzh_xvmRetNil();\n" );
      return 2;
   }
   else
   {
      fprintf( cargo->yyc, "\tzh_xvmPushNil();\n" );
      return 1;
   }
}

static ZH_GENC_FUNC( zh_p_pushself )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushSelf();\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pushstatic )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushStatic( %hu );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushstaticref )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushStaticByRef( %hu );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushstrshort )
{
   ZH_USHORT usLen = pFunc->pCode[ nPCodePos + 1 ] - 1;

   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushStringConst( " );
   zh_compGenCString( cargo->yyc, &pFunc->pCode[ nPCodePos + 2 ], usLen );
   fprintf( cargo->yyc, ", %hu );\n", usLen );

   return 3 + usLen;
}

static ZH_GENC_FUNC( zh_p_pushstr )
{
   ZH_SIZE nLen = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) - 1;

   ZH_GENC_LABEL();

   zh_compGenCStrData( cargo->yyc, &pFunc->pCode[ nPCodePos + 3 ], nLen, -1 );

   return 4 + nLen;
}

static ZH_GENC_FUNC( zh_p_pushstrlarge )
{
   ZH_SIZE nLen = ZH_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] ) - 1;

   ZH_GENC_LABEL();

   zh_compGenCStrData( cargo->yyc, &pFunc->pCode[ nPCodePos + 4 ], nLen, -1 );

   return 5 + nLen;
}

static ZH_GENC_FUNC( zh_p_push_str_hidden )
{
   ZH_SIZE nLen = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 2 ] );

   ZH_GENC_LABEL();

   zh_compGenCStrData( cargo->yyc, &pFunc->pCode[ nPCodePos + 4 ], nLen,
                       pFunc->pCode[ nPCodePos + 1 ] );

   return 4 + nLen;
}

static ZH_GENC_FUNC( zh_p_pushsym )
{
   ZH_GENC_LABEL();

   if( ZH_GENC_GETLABEL( nPCodePos + 3 ) == 0 )
   {
      switch( pFunc->pCode[ nPCodePos + 3 ] )
      {
         case ZH_P_PUSHNIL:
            fprintf( cargo->yyc, "\tzh_xvmPushFuncSymbol( symbols + %hu );\n",
                     ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
            return 4;
         case ZH_P_PUSH_ALIASED_FIELDNEAR:
            fprintf( cargo->yyc,
                     "\tif( zh_xvmPushAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                     pFunc->pCode[ nPCodePos + 4 ] );
            return 5;
         case ZH_P_PUSH_ALIASED_FIELD:
            fprintf( cargo->yyc,
                     "\tif( zh_xvmPushAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                     ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 4 ] ) );
            return 6;
         case ZH_P_POPALIASEDFIELDNEAR:
            fprintf( cargo->yyc,
                     "\tif( zh_xvmPopAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                     pFunc->pCode[ nPCodePos + 4 ] );
            return 5;
         case ZH_P_POPALIASEDFIELD:
            fprintf( cargo->yyc,
                     "\tif( zh_xvmPopAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                     ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 4 ] ) );
            return 6;
      }
   }

   fprintf( cargo->yyc, "\tzh_xvmPushSymbol( symbols + %hu );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushsymnear )
{
   ZH_GENC_LABEL();

   if( ZH_GENC_GETLABEL( nPCodePos + 2 ) == 0 )
   {
      switch( pFunc->pCode[ nPCodePos + 2 ] )
      {
         case ZH_P_PUSHNIL:
            fprintf( cargo->yyc, "\tzh_xvmPushFuncSymbol( symbols + %u );\n",
                     pFunc->pCode[ nPCodePos + 1 ] );
            return 3;
         case ZH_P_PUSH_ALIASED_FIELDNEAR:
            fprintf( cargo->yyc,
                     "\tif( zh_xvmPushAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     pFunc->pCode[ nPCodePos + 1 ],
                     pFunc->pCode[ nPCodePos + 3 ] );
            return 4;
         case ZH_P_PUSH_ALIASED_FIELD:
            fprintf( cargo->yyc,
                     "\tif( zh_xvmPushAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     pFunc->pCode[ nPCodePos + 1 ],
                     ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
            return 5;
         case ZH_P_POPALIASEDFIELDNEAR:
            fprintf( cargo->yyc,
                     "\tif( zh_xvmPopAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     pFunc->pCode[ nPCodePos + 1 ],
                     pFunc->pCode[ nPCodePos + 3 ] );
            return 4;
         case ZH_P_POPALIASEDFIELD:
            fprintf( cargo->yyc,
                     "\tif( zh_xvmPopAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     pFunc->pCode[ nPCodePos + 1 ],
                     ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
            return 5;
      }
   }

   fprintf( cargo->yyc, "\tzh_xvmPushSymbol( symbols + %u );\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_pushfuncsym )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushFuncSymbol( symbols + %hu );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_pushvariable )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPushVariable( symbols + %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_retvalue )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmRetValue();\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_send )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmSend( %hu ) ) break;\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_sendshort )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmSend( %u ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static ZH_GENC_FUNC( zh_p_pushovarref )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPushObjectVarRef() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_seqalways )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmSeqAlways();\n\tdo {\n" );
   cargo->iNestedBlock++;
   return 4;
}

static ZH_GENC_FUNC( zh_p_alwaysbegin )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\t} while( 0 );\n\tif( zh_xvmAlwaysBegin() ) break;\n\tdo {\n" );
   return 4;
}

static ZH_GENC_FUNC( zh_p_alwaysend )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\t} while( 0 );\n\tif( zh_xvmAlwaysEnd() ) break;\n" );
   cargo->iNestedBlock--;
   return 1;
}

static ZH_GENC_FUNC( zh_p_seqblock )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmSeqBlock() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_seqbegin )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmSeqBegin();\n\tfor( ;; ) {\n" );
   cargo->iNestedBlock++;
   return 4;
}

static ZH_GENC_FUNC( zh_p_seqend )
{
   ZH_ISIZ nOffset = ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );

   ZH_GENC_LABEL();

   if( nOffset == 4 ) /* no RECOVER clause */
      fprintf( cargo->yyc, "\tbreak;\n\t}\n\tif( zh_xvmSeqEnd() ) break;\n" );
   else               /* RECOVER exists */
      fprintf( cargo->yyc, "\tif( zh_xvmSeqEndTest() ) break;\n\tgoto lab%05" ZH_PFS "u;\n\t}\n",
               ZH_GENC_GETLABEL( nPCodePos + nOffset ) );
   cargo->iNestedBlock--;
   return 4;
}

static ZH_GENC_FUNC( zh_p_seqrecover )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmSeqRecover() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_sframe )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmSFrame( symbols + %hu );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_statics )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmStatics( symbols + %hu, %hu );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
   return 5;
}

static ZH_GENC_FUNC( zh_p_staticname )
{
   ZH_USHORT usLen;

   ZH_GENC_LABEL();

   usLen = ( ZH_USHORT ) strlen( ( char * ) &pFunc->pCode[ nPCodePos + 4 ] );
   fprintf( cargo->yyc, "\tzh_xvmStaticName( %hu, %hu, ",
            ( ZH_USHORT ) pFunc->pCode[ nPCodePos + 1 ],
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 2 ] ) );
   zh_compGenCString( cargo->yyc, &pFunc->pCode[ nPCodePos + 4 ], usLen );
   fprintf( cargo->yyc, " );\n" );
   return usLen + 5;
}

static ZH_GENC_FUNC( zh_p_threadstatics )
{
   ZH_USHORT w;
   ZH_SIZE nSize, ul;

   ZH_GENC_LABEL();

   w = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
   nSize = ( ZH_SIZE ) w << 1;

   fprintf( cargo->yyc, "\t{\n\t\tstatic const ZH_BYTE statics[ %" ZH_PFS "u ] = {", nSize );

   for( ul = 0; ul < nSize; ++ul )
   {
      if( ( ul & 0x0f ) == 0 )
         fprintf( cargo->yyc, "\n\t\t\t" );
      if( ul == nSize - 1 )
         fprintf( cargo->yyc, "%u", pFunc->pCode[ nPCodePos + ul + 3 ] );
      else
         fprintf( cargo->yyc, "%u, ", pFunc->pCode[ nPCodePos + ul + 3 ] );
   }
   fprintf( cargo->yyc, " };\n\t\tzh_xvmThreadStatics( %hu, statics );\n\t}\n", w );

   return 3 + nSize;
}

static ZH_GENC_FUNC( zh_p_swapalias )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmSwapAlias() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_true )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushLogical( ZH_TRUE );\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_one )
{
   int iSkip;

   ZH_GENC_LABEL();

   iSkip = zh_gencc_checkNumAhead( 1, pFunc, nPCodePos + 1, cargo );
   if( iSkip == 0 )
      fprintf( cargo->yyc, "\tzh_xvmPushInteger( 1 );\n" );
   return 1 + iSkip;
}

static ZH_GENC_FUNC( zh_p_zero )
{
   int iSkip;

   ZH_GENC_LABEL();

   iSkip = zh_gencc_checkNumAhead( 0, pFunc, nPCodePos + 1, cargo );
   if( iSkip == 0 )
      fprintf( cargo->yyc, "\tzh_xvmPushInteger( 0 );\n" );
   return 1 + iSkip;
}

static ZH_GENC_FUNC( zh_p_noop )
{
   ZH_GENC_LABEL();

   return 1;
}

static ZH_GENC_FUNC( zh_p_dummy )
{
   ZH_SYMBOL_UNUSED( cargo );
   ZH_SYMBOL_UNUSED( pFunc );
   ZH_SYMBOL_UNUSED( nPCodePos );
   return 1;
}

static ZH_GENC_FUNC( zh_p_enumstart )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmEnumStart( %u, %u ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static ZH_GENC_FUNC( zh_p_enumnext )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmEnumNext() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_enumprev )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmEnumPrev() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_enumend )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmEnumEnd();\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_switch )
{
   ZH_USHORT usCases = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), us;
   ZH_SIZE nStart = nPCodePos, nNewPos;
   ZH_BOOL fNum = ZH_FALSE, fStr = ZH_FALSE, fDefault = ZH_FALSE;

   ZH_GENC_LABEL();

   nPCodePos += 3;
   for( us = 0; us < usCases; ++us )
   {
      switch( pFunc->pCode[ nPCodePos ] )
      {
         case ZH_P_PUSHLONG:
            fNum = ZH_TRUE;
            nPCodePos += 5;
            break;
         case ZH_P_PUSHSTRSHORT:
            fStr = ZH_TRUE;
            nPCodePos += 2 + pFunc->pCode[ nPCodePos + 1 ];
            break;
         case ZH_P_PUSHNIL:
            /* default clause */
            fDefault = ZH_TRUE;
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
   }

   if( fStr || fNum )
   {
      fprintf( cargo->yyc, "\t{\n\t\tPZH_ITEM pSwitch;\n\t\tZH_TYPE type;\n" );
      if( fStr )
         fprintf( cargo->yyc, "\t\tconst char * pszText;\n\t\tZH_SIZE nLen;\n" );
      if( fNum )
         fprintf( cargo->yyc, "\t\tlong lVal;\n" );
      fprintf( cargo->yyc,
               "\t\tif( zh_xvmSwitchGet( &pSwitch ) ) break;\n"
               "\t\ttype = zh_itemType( pSwitch );\n" );
      if( fStr )
      {
         fprintf( cargo->yyc, "\t\tpszText = ( type & ZH_IT_STRING ) ? zh_itemGetCPtr( pSwitch ) : NULL;\n" );
         fprintf( cargo->yyc, "\t\tnLen = pszText ? zh_itemGetCLen( pSwitch ) : 0;\n" );
      }
      if( fNum )
         fprintf( cargo->yyc, "\t\tlVal = ( type & ZH_IT_NUMINT ) ? zh_itemGetNL( pSwitch ) : 0;\n\n" );
   }

   nPCodePos = nStart + 3;
   for( us = 0; us < usCases; ++us )
   {
      switch( pFunc->pCode[ nPCodePos ] )
      {
         case ZH_P_PUSHLONG:
            fprintf( cargo->yyc, "\t\tif( ( type & ZH_IT_NUMINT ) != 0 && lVal == %ldL )\n",
                     ZH_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ) );
            nPCodePos += 5;
            break;
         case ZH_P_PUSHSTRSHORT:
            fprintf( cargo->yyc, "\t\tif( pszText && nLen == %d && ! memcmp( pszText, ",
                     pFunc->pCode[ nPCodePos + 1 ] - 1 );
            zh_compGenCString( cargo->yyc, &pFunc->pCode[ nPCodePos + 2 ],
                               pFunc->pCode[ nPCodePos + 1 ] - 1 );
            fprintf( cargo->yyc, ", %d ) )\n", pFunc->pCode[ nPCodePos + 1 ] - 1 );
            nPCodePos += 2 + pFunc->pCode[ nPCodePos + 1 ];
            break;
         case ZH_P_PUSHNIL:
            /* default clause */
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
      fprintf( cargo->yyc, "\t\t{\n\t\t\tzh_stackPop();\n\t\t\tgoto lab%05" ZH_PFS "u;\n\t\t}\n",
               ZH_GENC_GETLABEL( nNewPos ) );
   }
   if( ! fDefault )
      fprintf( cargo->yyc, "\t\tzh_stackPop();\n" );
   if( fStr || fNum )
      fprintf( cargo->yyc, "\t}\n" );

   return nPCodePos - nStart;
}

static ZH_GENC_FUNC( zh_p_pushdate )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushDate( %ldL );\n",
            ( long ) ZH_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 5;
}

static ZH_GENC_FUNC( zh_p_pushtimestamp )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushTimeStamp( %ldL, %ldL );\n",
            ( long ) ZH_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ),
            ( long ) ZH_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 5 ] ) );
   return 9;
}

static ZH_GENC_FUNC( zh_p_localnearaddint )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmLocalAddInt( %u, %d ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ],
            ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 2 ] ) );
   return 4;
}

static ZH_GENC_FUNC( zh_p_localaddint )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmLocalAddInt( %d, %d ) ) break;\n",
            ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
            ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
   return 5;
}

static ZH_GENC_FUNC( zh_p_localinc )
{
   int iLocal = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );

   ZH_GENC_LABEL();

   if( ZH_GENC_GETLABEL( nPCodePos + 3 ) == 0 &&
       ( ( pFunc->pCode[ nPCodePos + 3 ] == ZH_P_PUSHLOCAL &&
           iLocal == ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 4 ] ) ) ||
         ( pFunc->pCode[ nPCodePos + 3 ] == ZH_P_PUSHLOCALNEAR &&
           iLocal == pFunc->pCode[ nPCodePos + 4 ] ) ) )
   {
      fprintf( cargo->yyc, "\tif( zh_xvmLocalIncPush( %d ) ) break;\n", iLocal );
      return ( pFunc->pCode[ nPCodePos + 3 ] == ZH_P_PUSHLOCAL ) ? 6 : 5;
   }

   fprintf( cargo->yyc, "\tif( zh_xvmLocalInc( %d ) ) break;\n", iLocal );
   return 3;
}

static ZH_GENC_FUNC( zh_p_localdec )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmLocalDec( %d ) ) break;\n",
            ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static ZH_GENC_FUNC( zh_p_localincpush )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmLocalIncPush( %d ) ) break;\n",
            ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );

   return 3;
}

static ZH_GENC_FUNC( zh_p_pluseqpop )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPlusEqPop() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_minuseqpop )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMinusEqPop() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_multeqpop )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMultEqPop() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_diveqpop )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmDivEqPop() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_modeqpop )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmModEqPop() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_expeqpop )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmExpEqPop() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_deceqpop )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmDecEqPop() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_inceqpop )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmIncEqPop() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pluseq )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmPlusEq() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_minuseq )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMinusEq() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_multeq )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmMultEq() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_diveq )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmDivEq() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_modeq )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmModEq() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_expeq )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmExpEq() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_deceq )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmDecEq() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_inceq )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( zh_xvmIncEq() ) break;\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_withobjectstart )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmWithObjectStart();\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_withobjectend )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmWithObjectEnd();\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_withobjectmessage )
{
   ZH_USHORT usSym = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] );

   ZH_GENC_LABEL();

   if( usSym == 0xFFFF )
      fprintf( cargo->yyc, "\tzh_xvmWithObjectMessage( NULL );\n" );
   else
      fprintf( cargo->yyc, "\tzh_xvmWithObjectMessage( symbols + %hu );\n",
               usSym );

   return 3;
}

static ZH_GENC_FUNC( zh_p_vframe )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmVFrame( %u, %u );\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static ZH_GENC_FUNC( zh_p_largeframe )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmFrame( %u, %u );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
            pFunc->pCode[ nPCodePos + 3 ] );
   return 4;
}

static ZH_GENC_FUNC( zh_p_largevframe )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmVFrame( %u, %u );\n",
            ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
            pFunc->pCode[ nPCodePos + 3 ] );
   return 4;
}

static ZH_GENC_FUNC( zh_p_pushvparams )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushVParams();\n" );
   return 1;
}

static ZH_GENC_FUNC( zh_p_pushaparams )
{
   ZH_GENC_LABEL();

   fprintf( cargo->yyc, "\tzh_xvmPushAParams();\n" );
   return 1;
}


/* NOTE: The  order of functions have to match the order of opcodes
 *       mnemonics
 */
static const PZH_GENC_FUNC s_verbose_table[] = {
   zh_p_and,
   zh_p_arraypush,
   zh_p_arraypop,
   zh_p_arraydim,
   zh_p_arraygen,
   zh_p_equal,
   zh_p_endblock,
   zh_p_endproc,
   zh_p_exactlyequal,
   zh_p_false,
   zh_p_fortest,
   zh_p_function,
   zh_p_functionshort,
   zh_p_frame,
   zh_p_funcptr,
   zh_p_greater,
   zh_p_greaterequal,
   zh_p_dec,
   zh_p_divide,
   zh_p_do,
   zh_p_doshort,
   zh_p_duplicate,
   zh_p_pushtimestamp,
   zh_p_inc,
   zh_p_instring,
   zh_p_jumpnear,
   zh_p_jump,
   zh_p_jumpfar,
   zh_p_jumpfalsenear,
   zh_p_jumpfalse,
   zh_p_jumpfalsefar,
   zh_p_jumptruenear,
   zh_p_jumptrue,
   zh_p_jumptruefar,
   zh_p_lessequal,
   zh_p_less,
   zh_p_line,
   zh_p_localname,
   zh_p_macropop,
   zh_p_macropopaliased,
   zh_p_macropush,
   zh_p_macro_array_gen,
   zh_p_macropushlist,
   zh_p_macropushindex,
   zh_p_macropushpare,
   zh_p_macropushaliased,
   zh_p_macrosymbol,
   zh_p_macrotext,
   zh_p_message,
   zh_p_minus,
   zh_p_modulus,
   zh_p_modulename,
   /* start: pcodes generated by macro compiler */
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_dummy,
   /* end: */
   zh_p_mult,
   zh_p_negate,
   zh_p_noop,
   zh_p_not,
   zh_p_notequal,
   zh_p_or,
   zh_p_parameter,
   zh_p_plus,
   zh_p_pop,
   zh_p_popalias,
   zh_p_popaliasedfield,
   zh_p_popaliasedfieldnear,
   zh_p_popaliasedvar,
   zh_p_popfield,
   zh_p_poplocal,
   zh_p_poplocalnear,
   zh_p_popmemvar,
   zh_p_popstatic,
   zh_p_popvariable,
   zh_p_power,
   zh_p_pushalias,
   zh_p_pushaliasedfield,
   zh_p_pushaliasedfieldnear,
   zh_p_pushaliasedvar,
   zh_p_push_block,
   zh_p_push_blockshort,
   zh_p_pushfield,
   zh_p_pushbyte,
   zh_p_pushint,
   zh_p_pushlocal,
   zh_p_pushlocalnear,
   zh_p_pushlocalref,
   zh_p_pushlong,
   zh_p_pushmemvar,
   zh_p_pushmemvarref,
   zh_p_pushnil,
   zh_p_pushdouble,
   zh_p_pushself,
   zh_p_pushstatic,
   zh_p_pushstaticref,
   zh_p_pushstr,
   zh_p_pushstrshort,
   zh_p_pushsym,
   zh_p_pushsymnear,
   zh_p_pushvariable,
   zh_p_retvalue,
   zh_p_send,
   zh_p_sendshort,
   zh_p_seqbegin,
   zh_p_seqend,
   zh_p_seqrecover,
   zh_p_sframe,
   zh_p_statics,
   zh_p_staticname,
   zh_p_swapalias,
   zh_p_true,
   zh_p_zero,
   zh_p_one,
   zh_p_macro_func,
   zh_p_macrodo,
   /* start: more pcodes generated by macro compiler */
   zh_p_dummy,
   /* end: */
   zh_p_localnearaddint,
   zh_p_macropushref,
   zh_p_pushlonglong,
   zh_p_enumstart,
   zh_p_enumnext,
   zh_p_enumprev,
   zh_p_enumend,
   zh_p_switch,
   zh_p_pushdate,
   /* optimization of inlined math operations (+=, -= */
   zh_p_pluseqpop,
   zh_p_minuseqpop,
   zh_p_multeqpop,
   zh_p_diveqpop,
   zh_p_pluseq,
   zh_p_minuseq,
   zh_p_multeq,
   zh_p_diveq,
   zh_p_withobjectstart,
   zh_p_withobjectmessage,
   zh_p_withobjectend,
   zh_p_macrosend,
   zh_p_pushovarref,
   zh_p_arraypushref,
   zh_p_vframe,
   zh_p_largeframe,
   zh_p_largevframe,
   zh_p_push_str_hidden,
   zh_p_localaddint,
   zh_p_modeqpop,
   zh_p_expeqpop,
   zh_p_modeq,
   zh_p_expeq,
   zh_p_duplunref,
   zh_p_dummy,
   zh_p_dummy,
   zh_p_push_blocklarge,
   zh_p_pushstrlarge,
   zh_p_swap,
   zh_p_pushvparams,
   zh_p_pushunref,
   zh_p_seqalways,
   zh_p_alwaysbegin,
   zh_p_alwaysend,
   zh_p_deceqpop,
   zh_p_inceqpop,
   zh_p_deceq,
   zh_p_inceq,
   zh_p_localdec,
   zh_p_localinc,
   zh_p_localincpush,
   zh_p_pushfuncsym,
   zh_p_hashgen,
   zh_p_seqblock,
   zh_p_threadstatics,
   zh_p_pushaparams
};

void zh_compGenCRealCode( ZH_COMP_DECL, PZH_ZFUNC pFunc, FILE * yyc )
{
   const PZH_GENC_FUNC * pFuncTable = s_verbose_table;
   ZH_LABEL_INFO label_info;

   /* Make sure that table is correct */
   assert( ZH_P_LAST_PCODE == sizeof( s_verbose_table ) / sizeof( PZH_GENC_FUNC ) );

   label_info.yyc = yyc;
   label_info.fVerbose = ( ZH_COMP_PARAM->iGenCOutput == ZH_COMPGENC_VERBOSE );
   label_info.fSetSeqBegin = ZH_FALSE;
   label_info.fCondJump = ZH_FALSE;
   label_info.fEndRequest = ZH_FALSE;
   label_info.iNestedBlock = 0;
   label_info.pFuncTable = ( const PZH_PCODE_FUNC * ) pFuncTable;
   if( pFunc->nPCodePos == 0 )
      label_info.pnLabels = NULL;
   else
   {
      label_info.pnLabels = ( ZH_SIZE * ) zh_xgrabz( pFunc->nPCodePos * sizeof( ZH_SIZE ) );
      zh_compGenLabelTable( pFunc, &label_info );
   }

   fprintf( yyc, "{\n" );
   if( label_info.fCondJump )
      fprintf( yyc, "   ZH_BOOL fValue;\n" );
   fprintf( yyc, "   do {\n" );

   zh_compPCodeEval( pFunc, ( const PZH_PCODE_FUNC * ) pFuncTable, ( void * ) &label_info );

   fprintf( yyc, "   } while( 0 );\n" );
   if( label_info.fEndRequest )
      fprintf( yyc, "   zh_xvmExitProc();\n" );
   fprintf( yyc, "}\n" );

   if( label_info.pnLabels )
      zh_xfree( label_info.pnLabels );
}
