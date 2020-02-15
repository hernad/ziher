/*
 * Compiler PCODE optimizer
 *
 * Copyright 2007 Przemyslaw Czerpak
 * Copyright 2008 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
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

#define ZH_OPT_FUNC( func )  ZH_PCODE_FUNC( func, void * )
typedef ZH_OPT_FUNC( ZH_OPT_FUNC_ );
typedef ZH_OPT_FUNC_ * PZH_OPT_FUNC;


static ZH_OPT_FUNC( zh_p_poplocal )
{
   ZH_BYTE * pVar = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SHORT iVar = ZH_PCODE_MKSHORT( pVar );

   ZH_SYMBOL_UNUSED( cargo );

   if( ZH_LIM_INT8( iVar ) )
   {
      pFunc->pCode[ nPCodePos ] = ZH_P_POPLOCALNEAR;
      zh_compNOOPfill( pFunc, nPCodePos + 2, 1, ZH_FALSE, ZH_FALSE );
   }

   return 3;
}

static ZH_OPT_FUNC( zh_p_pushlocal )
{
   ZH_BYTE * pVar = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SHORT iVar = ZH_PCODE_MKSHORT( pVar );

   ZH_SYMBOL_UNUSED( cargo );

   if( pFunc->pCode[ nPCodePos + 3 ] == ZH_P_POPLOCAL &&
       ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 4 ] ) == iVar &&
       ! zh_compHasJump( pFunc, nPCodePos + 3 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 6, ZH_FALSE, ZH_FALSE );
   }
   else if( pFunc->pCode[ nPCodePos + 3 ] == ZH_P_POPLOCALNEAR &&
            ( ZH_SCHAR ) pFunc->pCode[ nPCodePos + 4 ] == iVar &&
            ! zh_compHasJump( pFunc, nPCodePos + 3 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 5, ZH_FALSE, ZH_FALSE );
   }
   else if( pFunc->pCode[ nPCodePos + 3 ] == ZH_P_POP &&
            ! zh_compHasJump( pFunc, nPCodePos + 3 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 4, ZH_FALSE, ZH_FALSE );
   }
   else if( ZH_LIM_INT8( iVar ) )
   {
      pFunc->pCode[ nPCodePos ] = ZH_P_PUSHLOCALNEAR;
      zh_compNOOPfill( pFunc, nPCodePos + 2, 1, ZH_FALSE, ZH_FALSE );
   }

   return 3;
}

static ZH_OPT_FUNC( zh_p_pushlocalnear )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( pFunc->pCode[ nPCodePos + 2 ] == ZH_P_POPLOCAL &&
       ( ZH_SCHAR ) pFunc->pCode[ nPCodePos + 1 ] ==
       ZH_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) &&
       ! zh_compHasJump( pFunc, nPCodePos + 2 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 5, ZH_FALSE, ZH_FALSE );
   }
   else if( pFunc->pCode[ nPCodePos + 2 ] == ZH_P_POPLOCALNEAR &&
            pFunc->pCode[ nPCodePos + 1 ] == pFunc->pCode[ nPCodePos + 3 ] &&
            ! zh_compHasJump( pFunc, nPCodePos + 2 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 4, ZH_FALSE, ZH_FALSE );
   }
   else if( pFunc->pCode[ nPCodePos + 2 ] == ZH_P_POP &&
            ! zh_compHasJump( pFunc, nPCodePos + 2 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 3, ZH_FALSE, ZH_FALSE );
   }

   return 2;
}

static ZH_OPT_FUNC( zh_p_localaddint )
{
   ZH_BYTE * pVar = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_SHORT iVar = ZH_PCODE_MKSHORT( pVar );

   ZH_SYMBOL_UNUSED( cargo );

   if( ZH_LIM_INT8( iVar ) )
   {
      pVar[ 0 ] = ZH_P_LOCALNEARADDINT;
      pVar[ 1 ] = ZH_LOBYTE( iVar );
      zh_compNOOPfill( pFunc, nPCodePos, 1, ZH_FALSE, ZH_FALSE );
   }

   return 5;
}

static ZH_OPT_FUNC( zh_p_pushstatic )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( pFunc->pCode[ nPCodePos + 3 ] == ZH_P_POPSTATIC &&
       ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ==
       ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 4 ] ) &&
       ! zh_compHasJump( pFunc, nPCodePos + 3 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 6, ZH_FALSE, ZH_FALSE );
   }
   else if( pFunc->pCode[ nPCodePos + 3 ] == ZH_P_POP &&
            ! zh_compHasJump( pFunc, nPCodePos + 3 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 4, ZH_FALSE, ZH_FALSE );
   }

   return 3;
}

static ZH_OPT_FUNC( zh_p_pushmemvar )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( pFunc->pCode[ nPCodePos + 3 ] == ZH_P_POPMEMVAR &&
       ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ==
       ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 4 ] ) &&
       ! zh_compHasJump( pFunc, nPCodePos + 3 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 6, ZH_FALSE, ZH_FALSE );
   }
   else if( pFunc->pCode[ nPCodePos + 3 ] == ZH_P_POP &&
            ! zh_compHasJump( pFunc, nPCodePos + 3 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 4, ZH_FALSE, ZH_FALSE );
   }

   return 3;
}

static ZH_OPT_FUNC( zh_p_pushnil )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( pFunc->pCode[ nPCodePos + 1 ] == ZH_P_POP &&
       ! zh_compHasJump( pFunc, nPCodePos + 1 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 2, ZH_FALSE, ZH_FALSE );
   }

   return 1;
}

static ZH_OPT_FUNC( zh_p_false )
{
   ZH_SYMBOL_UNUSED( cargo );

   switch( pFunc->pCode[ nPCodePos + 1 ] )
   {
      case ZH_P_POP:
      case ZH_P_NOT:
      case ZH_P_JUMPFALSENEAR:
      case ZH_P_JUMPFALSE:
      case ZH_P_JUMPFALSEFAR:
      case ZH_P_JUMPTRUENEAR:
      case ZH_P_JUMPTRUE:
      case ZH_P_JUMPTRUEFAR:
         if( ! zh_compHasJump( pFunc, nPCodePos + 1 ) )
         {
            int iCount = 1;

            switch( pFunc->pCode[ nPCodePos + 1 ] )
            {
               case ZH_P_JUMPFALSENEAR:
                  pFunc->pCode[ nPCodePos + 1 ] = ZH_P_JUMPNEAR;
                  break;
               case ZH_P_JUMPFALSE:
                  pFunc->pCode[ nPCodePos + 1 ] = ZH_P_JUMP;
                  break;
               case ZH_P_JUMPFALSEFAR:
                  pFunc->pCode[ nPCodePos + 1 ] = ZH_P_JUMPFAR;
                  break;
               case ZH_P_NOT:
                  pFunc->pCode[ nPCodePos + 1 ] = ZH_P_TRUE;
                  break;
               case ZH_P_POP:
                  iCount = 2;
                  break;
               case ZH_P_JUMPTRUENEAR:
                  iCount = 3;
                  break;
               case ZH_P_JUMPTRUE:
                  iCount = 4;
                  break;
               case ZH_P_JUMPTRUEFAR:
                  iCount = 5;
                  break;
            }
            zh_compNOOPfill( pFunc, nPCodePos, iCount, ZH_FALSE, ZH_FALSE );
         }
         break;
   }
   return 1;
}

static ZH_OPT_FUNC( zh_p_true )
{
   ZH_SYMBOL_UNUSED( cargo );

   switch( pFunc->pCode[ nPCodePos + 1 ] )
   {
      case ZH_P_POP:
      case ZH_P_NOT:
      case ZH_P_JUMPTRUENEAR:
      case ZH_P_JUMPTRUE:
      case ZH_P_JUMPTRUEFAR:
      case ZH_P_JUMPFALSENEAR:
      case ZH_P_JUMPFALSE:
      case ZH_P_JUMPFALSEFAR:
         if( ! zh_compHasJump( pFunc, nPCodePos + 1 ) )
         {
            int iCount = 1;

            switch( pFunc->pCode[ nPCodePos + 1 ] )
            {
               case ZH_P_JUMPTRUENEAR:
                  pFunc->pCode[ nPCodePos + 1 ] = ZH_P_JUMPNEAR;
                  break;
               case ZH_P_JUMPTRUE:
                  pFunc->pCode[ nPCodePos + 1 ] = ZH_P_JUMP;
                  break;
               case ZH_P_JUMPTRUEFAR:
                  pFunc->pCode[ nPCodePos + 1 ] = ZH_P_JUMPFAR;
                  break;
               case ZH_P_NOT:
                  pFunc->pCode[ nPCodePos + 1 ] = ZH_P_FALSE;
                  break;
               case ZH_P_POP:
                  iCount = 2;
                  break;
               case ZH_P_JUMPFALSENEAR:
                  iCount = 3;
                  break;
               case ZH_P_JUMPFALSE:
                  iCount = 4;
                  break;
               case ZH_P_JUMPFALSEFAR:
                  iCount = 5;
                  break;
            }
            zh_compNOOPfill( pFunc, nPCodePos, iCount, ZH_FALSE, ZH_FALSE );
         }
         break;
   }
   return 1;
}

static ZH_OPT_FUNC( zh_p_duplicate )
{
   ZH_SYMBOL_UNUSED( cargo );

   switch( pFunc->pCode[ nPCodePos + 1 ] )
   {
      case ZH_P_JUMPTRUEFAR:
      case ZH_P_JUMPFALSEFAR:
         if( pFunc->pCode[ nPCodePos + 5 ] == ZH_P_POP )
         {
            ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 2 ];
            ZH_ISIZ nOffset = ZH_PCODE_MKINT24( pAddr ), nLastOffset = 0;
            ZH_SIZE nNewPos = nPCodePos + 1 + nOffset;
            ZH_BOOL fNot = ZH_FALSE, fOK = ZH_TRUE, fRepeat = ZH_TRUE;

            do
            {
               if( pFunc->pCode[ nNewPos ] == ZH_P_DUPLICATE )
               {
                  if( nOffset > 0 )
                     zh_p_duplicate( pFunc, nNewPos, NULL );
               }

               if( pFunc->pCode[ nNewPos ] == ZH_P_NOOP )
               {
                  nNewPos++;
                  nOffset++;
               }
               else if( pFunc->pCode[ nNewPos ] == ZH_P_NOT )
               {
                  nNewPos++;
                  nOffset++;
                  fNot = ! fNot;
                  fOK  = ! fOK;
               }
               else if( pFunc->pCode[ nNewPos ] == ZH_P_DUPLICATE &&
                        ( pFunc->pCode[ nNewPos + 1 ] == ZH_P_JUMPTRUEFAR ||
                          pFunc->pCode[ nNewPos + 1 ] == ZH_P_JUMPFALSEFAR ) )
               {
                  ZH_ISIZ nJump;
                  if( pFunc->pCode[ nNewPos + 1 ] != pFunc->pCode[ nPCodePos + 1 ] )
                     fNot = ! fNot;
                  nJump = fNot ? 4 : ZH_PCODE_MKINT24( &pFunc->pCode[ nNewPos + 2 ] );
                  nOffset += nJump + 1;
                  nNewPos = nPCodePos + 1 + nOffset;
                  fRepeat = nJump > 0;
               }
               else
               {
                  fRepeat = ZH_FALSE;
                  if( pFunc->pCode[ nNewPos ] == ZH_P_POP )
                     fOK = ZH_TRUE;
               }

               if( fOK )
                  nLastOffset = nOffset;
            }
            while( fRepeat );

            if( ( pFunc->pCode[ nNewPos ] == ZH_P_JUMPTRUEFAR ||
                  pFunc->pCode[ nNewPos ] == ZH_P_JUMPFALSEFAR ) &&
                ! zh_compHasJump( pFunc, nPCodePos + 1 ) &&
                ! zh_compHasJump( pFunc, nPCodePos + 5 ) )
            {
               if( pFunc->pCode[ nNewPos ] != pFunc->pCode[ nPCodePos + 1 ] )
                  fNot = ! fNot;
               if( fNot )
                  nOffset += 4;
               else
                  nOffset += ZH_PCODE_MKINT24( &pFunc->pCode[ nNewPos + 1 ] );

               ZH_PUT_LE_UINT24( pAddr, nOffset );
               zh_compNOOPfill( pFunc, nPCodePos, 1, ZH_FALSE, ZH_FALSE );
               zh_compNOOPfill( pFunc, nPCodePos + 5, 1, ZH_FALSE, ZH_FALSE );
            }
            else if( nLastOffset )
            {
               ZH_PUT_LE_UINT24( pAddr, nLastOffset );
            }
         }
         break;
   }
   return 1;
}

static ZH_OPT_FUNC( zh_p_not )
{
   ZH_BYTE opcode;

   ZH_SYMBOL_UNUSED( cargo );

   switch( pFunc->pCode[ nPCodePos + 1 ] )
   {
      case ZH_P_NOT:
         opcode = ZH_P_NOOP;
         break;
      case ZH_P_JUMPTRUENEAR:
         opcode = ZH_P_JUMPFALSENEAR;
         break;
      case ZH_P_JUMPTRUE:
         opcode = ZH_P_JUMPFALSE;
         break;
      case ZH_P_JUMPTRUEFAR:
         opcode = ZH_P_JUMPFALSEFAR;
         break;
      case ZH_P_JUMPFALSENEAR:
         opcode = ZH_P_JUMPTRUENEAR;
         break;
      case ZH_P_JUMPFALSE:
         opcode = ZH_P_JUMPTRUE;
         break;
      case ZH_P_JUMPFALSEFAR:
         opcode = ZH_P_JUMPTRUEFAR;
         break;
/* This optimization will be enabled in the future in a little bit differ form */
#if 0
      case ZH_P_DUPLICATE:
         if( ( pFunc->pCode[ nPCodePos + 2 ] == ZH_P_JUMPTRUEFAR ||
               pFunc->pCode[ nPCodePos + 2 ] == ZH_P_JUMPFALSEFAR ) &&
             pFunc->pCode[ nPCodePos + 6 ] == ZH_P_POP )
         {
            ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 3 ];
            ZH_ISIZ nOffset = ZH_PCODE_MKINT24( pAddr );

            if( nOffset > 0 )
            {
               zh_p_duplicate( pFunc, nPCodePos + 1, NULL );
               nOffset = ZH_PCODE_MKINT24( pAddr );
            }

            if( ( pFunc->pCode[ nPCodePos + 1 ] == ZH_P_NOT ||
                  ( pFunc->pCode[ nPCodePos + 1 ] == ZH_P_DUPLICATE &&
                    pFunc->pCode[ nPCodePos + nOffset + 2 ] == ZH_P_NOT ) ) &&
                ! zh_compHasJump( pFunc, nPCodePos + 1 ) )
            {
               zh_compNOOPfill( pFunc, nPCodePos, 1, ZH_FALSE, ZH_FALSE );
               if( pFunc->pCode[ nPCodePos + 2 ] == ZH_P_JUMPTRUEFAR )
                  pFunc->pCode[ nPCodePos + 2 ] = ZH_P_JUMPFALSEFAR;
               else
                  pFunc->pCode[ nPCodePos + 2 ] = ZH_P_JUMPTRUEFAR;
               if( pFunc->pCode[ nPCodePos + 1 ] == ZH_P_DUPLICATE )
               {
                  ++nOffset;
                  ZH_PUT_LE_UINT24( pAddr, nOffset );
               }
            }
         }
         /* fallthrough */
#endif
      default:
         opcode = ZH_P_LAST_PCODE;
         break;
   }

   if( opcode < ZH_P_LAST_PCODE &&
       ! zh_compHasJump( pFunc, nPCodePos + 1 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 1, ZH_FALSE, ZH_FALSE );
      if( opcode == ZH_P_NOOP )
         zh_compNOOPfill( pFunc, nPCodePos + 1, 1, ZH_FALSE, ZH_FALSE );
      else
         pFunc->pCode[ nPCodePos + 1 ] = opcode;
   }
   return 1;
}

static ZH_OPT_FUNC( zh_p_jumpfar )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_ISIZ nOffset = ZH_PCODE_MKINT24( pAddr );
   ZH_SIZE nNewPos = nPCodePos + nOffset;

   ZH_SYMBOL_UNUSED( cargo );

   if( nOffset == 4 )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 4, ZH_FALSE, ZH_FALSE );
   }
   else
   {
      ZH_BOOL fLine = ZH_FALSE;

      if( pFunc->pCode[ nNewPos ] == ZH_P_LINE )
      {
         fLine = ZH_TRUE;
         nNewPos += 3;
         nOffset += 3;
      }

      switch( pFunc->pCode[ nNewPos ] )
      {
         case ZH_P_JUMPFAR:
            nOffset += ZH_PCODE_MKINT24( &pFunc->pCode[ nNewPos + 1 ] );
            if( ! fLine || pFunc->pCode[ nPCodePos + nOffset ] == ZH_P_LINE )
               ZH_PUT_LE_UINT24( pAddr, nOffset );
            break;

         case ZH_P_JUMPFALSEFAR:
            nNewPos += ZH_PCODE_MKINT24( &pFunc->pCode[ nNewPos + 1 ] );
            if( nNewPos == nPCodePos + 4 && ( ! fLine ||
                ( pFunc->pCode[ nNewPos ] == ZH_P_LINE &&
                  pFunc->pCode[ nPCodePos + nOffset + 4 ] == ZH_P_LINE ) ) )
            {
               pFunc->pCode[ nPCodePos ] = ZH_P_JUMPTRUEFAR;
               ZH_PUT_LE_UINT24( pAddr, nOffset + 4 );
            }
            break;

         case ZH_P_JUMPTRUEFAR:
            nNewPos += ZH_PCODE_MKINT24( &pFunc->pCode[ nNewPos + 1 ] );
            if( nNewPos == nPCodePos + 4 && ( ! fLine ||
                ( pFunc->pCode[ nNewPos ] == ZH_P_LINE &&
                  pFunc->pCode[ nPCodePos + nOffset + 4 ] == ZH_P_LINE ) ) )
            {
               pFunc->pCode[ nPCodePos ] = ZH_P_JUMPFALSEFAR;
               ZH_PUT_LE_UINT24( pAddr, nOffset + 4 );
            }
            break;
      }
   }
   return 4;
}

static ZH_OPT_FUNC( zh_p_jumpfalsefar )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_ISIZ nOffset = ZH_PCODE_MKINT24( pAddr );
   ZH_SIZE nNewPos = nPCodePos + nOffset;
   ZH_BOOL fLine = ZH_FALSE;

   ZH_SYMBOL_UNUSED( cargo );

   if( nOffset == 8 && pFunc->pCode[ nPCodePos + 4 ] == ZH_P_JUMPFAR &&
       ! zh_compHasJump( pFunc, nPCodePos + 4 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 4, ZH_FALSE, ZH_FALSE );
      pFunc->pCode[ nPCodePos + 4 ] = ZH_P_JUMPTRUEFAR;
   }
   else if( nOffset == 11 && pFunc->pCode[ nPCodePos + 4 ] == ZH_P_LINE &&
            pFunc->pCode[ nPCodePos + 11 ] == ZH_P_LINE &&
            pFunc->pCode[ nPCodePos + 7 ] == ZH_P_JUMPFAR &&
            pFunc->pCode[ nPCodePos + 7 +
               ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 8 ] ) ] == ZH_P_LINE &&
            ! zh_compHasJump( pFunc, nPCodePos + 4 ) &&
            ! zh_compHasJump( pFunc, nPCodePos + 7 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 7, ZH_FALSE, ZH_FALSE );
      pFunc->pCode[ nPCodePos + 7 ] = ZH_P_JUMPTRUEFAR;
   }
   else
   {
      if( pFunc->pCode[ nNewPos ] == ZH_P_LINE )
      {
         fLine = ZH_TRUE;
         nNewPos += 3;
         nOffset += 3;
      }
      if( pFunc->pCode[ nNewPos ] == ZH_P_JUMPFAR )
      {
         nOffset += ZH_PCODE_MKINT24( &pFunc->pCode[ nNewPos + 1 ] );
         if( ! fLine || pFunc->pCode[ nPCodePos + nOffset ] == ZH_P_LINE )
            ZH_PUT_LE_UINT24( pAddr, nOffset );
      }
   }
   return 4;
}

static ZH_OPT_FUNC( zh_p_jumptruefar )
{
   ZH_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   ZH_ISIZ nOffset = ZH_PCODE_MKINT24( pAddr );
   ZH_SIZE nNewPos = nPCodePos + nOffset;
   ZH_BOOL fLine = ZH_FALSE;

   ZH_SYMBOL_UNUSED( cargo );

   if( nOffset == 8 && pFunc->pCode[ nPCodePos + 4 ] == ZH_P_JUMPFAR &&
       ! zh_compHasJump( pFunc, nPCodePos + 4 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 4, ZH_FALSE, ZH_FALSE );
      pFunc->pCode[ nPCodePos + 4 ] = ZH_P_JUMPFALSEFAR;
   }
   else if( nOffset == 11 && pFunc->pCode[ nPCodePos + 4 ] == ZH_P_LINE &&
            pFunc->pCode[ nPCodePos + 11 ] == ZH_P_LINE &&
            pFunc->pCode[ nPCodePos + 7 ] == ZH_P_JUMPFAR &&
            pFunc->pCode[ nPCodePos + 7 +
               ZH_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 8 ] ) ] == ZH_P_LINE &&
            ! zh_compHasJump( pFunc, nPCodePos + 4 ) &&
            ! zh_compHasJump( pFunc, nPCodePos + 7 ) )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 7, ZH_FALSE, ZH_FALSE );
      pFunc->pCode[ nPCodePos + 7 ] = ZH_P_JUMPFALSEFAR;
   }
   else
   {
      if( pFunc->pCode[ nNewPos ] == ZH_P_LINE )
      {
         fLine = ZH_TRUE;
         nNewPos += 3;
         nOffset += 3;
      }
      if( pFunc->pCode[ nNewPos ] == ZH_P_JUMPFAR )
      {
         nOffset += ZH_PCODE_MKINT24( &pFunc->pCode[ nNewPos + 1 ] );
         if( ! fLine || pFunc->pCode[ nPCodePos + nOffset ] == ZH_P_LINE )
            ZH_PUT_LE_UINT24( pAddr, nOffset );
      }
   }
   return 4;
}

static ZH_OPT_FUNC( zh_p_switch )
{
   ZH_USHORT usCases = ZH_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), us;
   ZH_SIZE nStart = nPCodePos;

   ZH_SYMBOL_UNUSED( cargo );

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
         case ZH_P_PUSHDATE:
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
            nPCodePos += 2;
            break;
         case ZH_P_JUMP:
            nPCodePos += 3;
            break;
         /*case ZH_P_JUMPFAR:*/
         default:
            nPCodePos += 4;
            break;
      }
   }

   return nPCodePos - nStart;
}

static ZH_OPT_FUNC( zh_p_function )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( pFunc->pCode[ nPCodePos + 3 ] == ZH_P_RETVALUE &&
       ! zh_compHasJump( pFunc, nPCodePos + 3 ) )
   {
      pFunc->pCode[ nPCodePos ] = ZH_P_DO;
      zh_compNOOPfill( pFunc, nPCodePos + 3, 1, ZH_FALSE, ZH_FALSE );
   }
   return 3;
}

static ZH_OPT_FUNC( zh_p_functionshort )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( pFunc->pCode[ nPCodePos + 2 ] == ZH_P_RETVALUE &&
       ! zh_compHasJump( pFunc, nPCodePos + 2 ) )
   {
      pFunc->pCode[ nPCodePos ] = ZH_P_DOSHORT;
      zh_compNOOPfill( pFunc, nPCodePos + 2, 1, ZH_FALSE, ZH_FALSE );
   }
   return 2;
}

static ZH_OPT_FUNC( zh_p_macrofunc )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( pFunc->pCode[ nPCodePos + 3 ] == ZH_P_RETVALUE &&
       ! zh_compHasJump( pFunc, nPCodePos + 3 ) )
   {
      pFunc->pCode[ nPCodePos ] = ZH_P_MACRODO;
      zh_compNOOPfill( pFunc, nPCodePos + 3, 1, ZH_FALSE, ZH_FALSE );
   }
   return 3;
}

static ZH_OPT_FUNC( zh_p_endblock )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( nPCodePos + 1 < pFunc->nPCodePos &&
       pFunc->pCode[ nPCodePos + 1 ] == ZH_P_ENDBLOCK )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 1, ZH_FALSE, ZH_FALSE );
   }
   return 1;
}

static ZH_OPT_FUNC( zh_p_endproc )
{
   ZH_SYMBOL_UNUSED( cargo );

   if( nPCodePos + 1 < pFunc->nPCodePos &&
       pFunc->pCode[ nPCodePos + 1 ] == ZH_P_ENDPROC )
   {
      zh_compNOOPfill( pFunc, nPCodePos, 1, ZH_FALSE, ZH_FALSE );
   }
   return 1;
}

/* NOTE: The  order of functions have to match the order of opcodes mnemonics
 */
static const PZH_OPT_FUNC s_opt_table[] =
{
   NULL,                       /* ZH_P_AND                   */
   NULL,                       /* ZH_P_ARRAYPUSH             */
   NULL,                       /* ZH_P_ARRAYPOP              */
   NULL,                       /* ZH_P_ARRAYDIM              */
   NULL,                       /* ZH_P_ARRAYGEN              */
   NULL,                       /* ZH_P_EQUAL                 */
   zh_p_endblock,              /* ZH_P_ENDBLOCK              */
   zh_p_endproc,               /* ZH_P_ENDPROC               */
   NULL,                       /* ZH_P_EXACTLYEQUAL          */
   zh_p_false,                 /* ZH_P_FALSE                 */
   NULL,                       /* ZH_P_FORTEST               */
   zh_p_function,              /* ZH_P_FUNCTION              */
   zh_p_functionshort,         /* ZH_P_FUNCTIONSHORT         */
   NULL,                       /* ZH_P_FRAME                 */
   NULL,                       /* ZH_P_FUNCPTR               */
   NULL,                       /* ZH_P_GREATER               */
   NULL,                       /* ZH_P_GREATEREQUAL          */
   NULL,                       /* ZH_P_DEC                   */
   NULL,                       /* ZH_P_DIVIDE                */
   NULL,                       /* ZH_P_DO                    */
   NULL,                       /* ZH_P_DOSHORT               */
   zh_p_duplicate,             /* ZH_P_DUPLICATE             */
   NULL,                       /* ZH_P_PUSHTIMESTAMP         */
   NULL,                       /* ZH_P_INC                   */
   NULL,                       /* ZH_P_INSTRING              */
   NULL,                       /* ZH_P_JUMPNEAR              */
   NULL,                       /* ZH_P_JUMP                  */
   zh_p_jumpfar,               /* ZH_P_JUMPFAR               */
   NULL,                       /* ZH_P_JUMPFALSENEAR         */
   NULL,                       /* ZH_P_JUMPFALSE             */
   zh_p_jumpfalsefar,          /* ZH_P_JUMPFALSEFAR          */
   NULL,                       /* ZH_P_JUMPTRUENEAR          */
   NULL,                       /* ZH_P_JUMPTRUE              */
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
   NULL,                       /* ZH_P_MPUSHALIASEDFIELD     */
   NULL,                       /* ZH_P_MPUSHALIASEDVAR       */
   NULL,                       /* ZH_P_MPUSHBLOCK            */
   NULL,                       /* ZH_P_MPUSHFIELD            */
   NULL,                       /* ZH_P_MPUSHMEMVAR           */
   NULL,                       /* ZH_P_MPUSHMEMVARREF        */
   NULL,                       /* ZH_P_MPUSHSYM              */
   NULL,                       /* ZH_P_MPUSHVARIABLE         */
                               /* end: */
   NULL,                       /* ZH_P_MULT                  */
   NULL,                       /* ZH_P_NEGATE                */
   NULL,                       /* ZH_P_NOOP                  */
   zh_p_not,                   /* ZH_P_NOT                   */
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
   zh_p_poplocal,              /* ZH_P_POPLOCAL              */
   NULL,                       /* ZH_P_POPLOCALNEAR          */
   NULL,                       /* ZH_P_POPMEMVAR             */
   NULL,                       /* ZH_P_POPSTATIC             */
   NULL,                       /* ZH_P_POPVARIABLE           */
   NULL,                       /* ZH_P_POWER                 */
   NULL,                       /* ZH_P_PUSHALIAS             */
   NULL,                       /* ZH_P_PUSHALIASEDFIELD      */
   NULL,                       /* ZH_P_PUSHALIASEDFIELDNEAR  */
   NULL,                       /* ZH_P_PUSHALIASEDVAR        */
   NULL,                       /* ZH_P_PUSHBLOCK             */
   NULL,                       /* ZH_P_PUSHBLOCKSHORT        */
   NULL,                       /* ZH_P_PUSHFIELD             */
   NULL,                       /* ZH_P_PUSHBYTE              */
   NULL,                       /* ZH_P_PUSHINT               */
   zh_p_pushlocal,             /* ZH_P_PUSHLOCAL             */
   zh_p_pushlocalnear,         /* ZH_P_PUSHLOCALNEAR         */
   NULL,                       /* ZH_P_PUSHLOCALREF          */
   NULL,                       /* ZH_P_PUSHLONG              */
   zh_p_pushmemvar,            /* ZH_P_PUSHMEMVAR            */
   NULL,                       /* ZH_P_PUSHMEMVARREF         */
   zh_p_pushnil,               /* ZH_P_PUSHNIL               */
   NULL,                       /* ZH_P_PUSHDOUBLE            */
   NULL,                       /* ZH_P_PUSHSELF              */
   zh_p_pushstatic,            /* ZH_P_PUSHSTATIC            */
   NULL,                       /* ZH_P_PUSHSTATICREF         */
   NULL,                       /* ZH_P_PUSHSTR               */
   NULL,                       /* ZH_P_PUSHSTRSHORT          */
   NULL,                       /* ZH_P_PUSHSYM               */
   NULL,                       /* ZH_P_PUSHSYMNEAR           */
   NULL,                       /* ZH_P_PUSHVARIABLE          */
   NULL,                       /* ZH_P_RETVALUE              */
   NULL,                       /* ZH_P_SEND                  */
   NULL,                       /* ZH_P_SENDSHORT             */
   NULL,                       /* ZH_P_SEQBEGIN              */
   NULL,                       /* ZH_P_SEQEND                */
   NULL,                       /* ZH_P_SEQRECOVER            */
   NULL,                       /* ZH_P_SFRAME                */
   NULL,                       /* ZH_P_STATICS               */
   NULL,                       /* ZH_P_STATICNAME            */
   NULL,                       /* ZH_P_SWAPALIAS             */
   zh_p_true,                  /* ZH_P_TRUE                  */
   NULL,                       /* ZH_P_ZERO                  */
   NULL,                       /* ZH_P_ONE                   */
   zh_p_macrofunc,             /* ZH_P_MACROFUNC             */
   NULL,                       /* ZH_P_MACRODO               */
   NULL,                       /* ZH_P_MPUSHSTR              */
   NULL,                       /* ZH_P_LOCALNEARADDINT       */
   NULL,                       /* ZH_P_MACROPUSHREF          */
   NULL,                       /* ZH_P_PUSHLONGLONG          */
   NULL,                       /* ZH_P_ENUMSTART             */
   NULL,                       /* ZH_P_ENUMNEXT              */
   NULL,                       /* ZH_P_ENUMPREV              */
   NULL,                       /* ZH_P_ENUMEND               */
   zh_p_switch,                /* ZH_P_SWITCH                */
   NULL,                       /* ZH_P_PUSHDATE              */
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
   NULL,                       /* ZH_P_MACROSEND             */
   NULL,                       /* ZH_P_PUSHOVARREF           */
   NULL,                       /* ZH_P_ARRAYPUSHREF          */
   NULL,                       /* ZH_P_VFRAME                */
   NULL,                       /* ZH_P_LARGEFRAME            */
   NULL,                       /* ZH_P_LARGEVFRAME           */
   NULL,                       /* ZH_P_PUSHSTRHIDDEN         */
   zh_p_localaddint,           /* ZH_P_LOCALADDINT           */
   NULL,                       /* ZH_P_MODEQPOP              */
   NULL,                       /* ZH_P_EXPEQPOP              */
   NULL,                       /* ZH_P_MODEQ                 */
   NULL,                       /* ZH_P_EXPEQ                 */
   NULL,                       /* ZH_P_DUPLUNREF             */
   NULL,                       /* ZH_P_MPUSHBLOCKLARGE       */
   NULL,                       /* ZH_P_MPUSHSTRLARGE         */
   NULL,                       /* ZH_P_PUSHBLOCKLARGE        */
   NULL,                       /* ZH_P_PUSHSTRLARGE          */
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
   NULL,                       /* ZH_P_THREADSTATICS         */
   NULL                        /* ZH_P_PUSHAPARAMS           */
};

void zh_compOptimizePCode( ZH_COMP_DECL, PZH_ZFUNC pFunc )
{
   const PZH_OPT_FUNC * pFuncTable = s_opt_table;

   ZH_SYMBOL_UNUSED( ZH_COMP_PARAM );

   assert( ZH_P_LAST_PCODE == sizeof( s_opt_table ) / sizeof( PZH_OPT_FUNC ) );

   zh_compPCodeEval( pFunc, ( const PZH_PCODE_FUNC * ) pFuncTable, NULL );
}


/*
 *   PCode trace optimizer
 */

#define OPT_LOCAL_FLAG_BLOCK    1
#define OPT_LOCAL_FLAG_PUSH     2
#define OPT_LOCAL_FLAG_POP      4
#define OPT_LOCAL_FLAG_PUSHREF  8
#define OPT_LOCAL_FLAG_POPSELF  16
#define OPT_LOCAL_FLAG_CHANGE   32

typedef struct
{
   ZH_SHORT isNumber;
   ZH_BYTE  bFlags;
} ZH_OPT_LOCAL, * PZH_OPT_LOCAL;


static ZH_BOOL zh_compIsJump( ZH_BYTE bPCode )
{
   return ( bPCode >= ZH_P_JUMPNEAR && bPCode <= ZH_P_JUMPTRUEFAR ) || /* All jumps */
            bPCode == ZH_P_SEQBEGIN || bPCode == ZH_P_SEQEND ||
            bPCode == ZH_P_SEQALWAYS || bPCode == ZH_P_ALWAYSBEGIN;
}


static ZH_BOOL zh_compIsUncondJump( ZH_BYTE bPCode )
{
   return bPCode == ZH_P_JUMPNEAR ||
          bPCode == ZH_P_JUMP ||
          bPCode == ZH_P_JUMPFAR;
/*     || bPCode == ZH_P_SEQEND;
   BEGIN SEQUENCE/END SEQUENCE logic could not be processed using conditional/unconditional
   jumps. I set ZH_P_SEQEND as conditional jump though this PCode instruction is processed
   as unconditional jump by Ziher VM. This hack solves 'Variable is assigned but not used'
   warning false positive in code:
     BEGIN SEQUENCE
        nI := 1
        Break( NIL )
     RECOVER
        ? nI
     END SEQUENCE
   [Mindaugas]
 */
}

#if 0
static ZH_SHORT zh_compIsLocalOp( ZH_BYTE bCode )
{
   return bCode == ZH_P_POPLOCAL ||
          bCode == ZH_P_POPLOCALNEAR ||
          bCode == ZH_P_PUSHLOCAL ||
          bCode == ZH_P_PUSHLOCALNEAR ||
          bCode == ZH_P_PUSHLOCALREF ||
          bCode == ZH_P_LOCALNEARADDINT ||
          bCode == ZH_P_LOCALADDINT ||
          bCode == ZH_P_LOCALDEC ||
          bCode == ZH_P_LOCALINC ||
          bCode == ZH_P_LOCALINCPUSH;
}
#endif

static ZH_SHORT zh_compLocalGetNumber( ZH_BYTE * pCode )
{
   switch( *pCode )
   {
      case ZH_P_POPLOCALNEAR:
      case ZH_P_PUSHLOCALNEAR:
      case ZH_P_LOCALNEARADDINT:
         return *( ( signed char * ) pCode + 1 );

      case ZH_P_POPLOCAL:
      case ZH_P_PUSHLOCAL:
      case ZH_P_PUSHLOCALREF:
      case ZH_P_LOCALDEC:
      case ZH_P_LOCALINC:
      case ZH_P_LOCALINCPUSH:
      case ZH_P_LOCALADDINT:
         return ZH_PCODE_MKSHORT( pCode + 1 );
   }
   assert( 0 );
   return 0;
}


static ZH_ISIZ zh_compJumpGetOffset( ZH_BYTE * pCode )
{
   switch( *pCode )
   {
      case ZH_P_JUMPNEAR:
      case ZH_P_JUMPFALSENEAR:
      case ZH_P_JUMPTRUENEAR:
         return *( ( signed char * ) pCode + 1 );

      case ZH_P_JUMP:
      case ZH_P_JUMPFALSE:
      case ZH_P_JUMPTRUE:
         return ZH_PCODE_MKSHORT( pCode + 1 );

      case ZH_P_JUMPFAR:
      case ZH_P_JUMPFALSEFAR:
      case ZH_P_JUMPTRUEFAR:
      case ZH_P_SEQBEGIN:
      case ZH_P_SEQEND:
      case ZH_P_SEQALWAYS:
      case ZH_P_ALWAYSBEGIN:
         return ZH_PCODE_MKINT24( pCode + 1 );
   }
   assert( 0 );
   return 0;
}


static void zh_compPCodeEnumScanLocals( PZH_ZFUNC pFunc, PZH_OPT_LOCAL pLocals )
{
   ZH_SIZE nPos = 0, nLastPos = 0;
   ZH_SHORT isVar = 0;
   ZH_BOOL fWasJump = 0;

   while( nPos < pFunc->nPCodePos )
   {
      if( zh_compIsJump( pFunc->pCode[ nPos ] ) )
         fWasJump = 1;

      switch( pFunc->pCode[ nPos ] )
      {
         case ZH_P_POPLOCALNEAR:
         case ZH_P_PUSHLOCALNEAR:
         case ZH_P_LOCALNEARADDINT:
            isVar = ( signed char ) pFunc->pCode[ nPos + 1 ];
            break;

         case ZH_P_LOCALNAME:
         case ZH_P_POPLOCAL:
         case ZH_P_PUSHLOCAL:
         case ZH_P_PUSHLOCALREF:
         case ZH_P_LOCALADDINT:
         case ZH_P_LOCALDEC:
         case ZH_P_LOCALINC:
         case ZH_P_LOCALINCPUSH:
            isVar = ZH_PCODE_MKSHORT( &pFunc->pCode[ nPos + 1 ] );
            break;
      }

      switch( pFunc->pCode[ nPos ] )
      {
         case ZH_P_POPLOCALNEAR:
         case ZH_P_POPLOCAL:
            if( isVar > 0 )
            {
               if( nPos > 0 && pFunc->pCode[ nLastPos ] == ZH_P_PUSHSELF &&
                   ! zh_compHasJump( pFunc, nPos ) && ! fWasJump )
               {
                  /* For real POPSELF support we need to do backward tree
                     tracing. This is not implemented, but using fWasJump
                     we can easy optimize Self := QSelf() at the beginning
                     of functions. [Mindaugas]
                   */
                  pLocals[ isVar - 1 ].bFlags |= OPT_LOCAL_FLAG_POPSELF;
               }
               else
                  pLocals[ isVar - 1 ].bFlags |= OPT_LOCAL_FLAG_POP;
            }
            break;

         case ZH_P_PUSHLOCALNEAR:
         case ZH_P_PUSHLOCAL:
            if( isVar > 0 )
               pLocals[ isVar - 1 ].bFlags |= OPT_LOCAL_FLAG_PUSH;
            break;

         case ZH_P_PUSHLOCALREF:
            if( isVar > 0 )
            {
               ZH_SIZE nPosNext = nPos + zh_compPCodeSize( pFunc, nPos );
               ZH_BYTE bCodeNext = pFunc->pCode[ nPosNext ];
               ZH_BYTE bCodeNext2 = pFunc->pCode[ nPosNext + zh_compPCodeSize( pFunc, nPosNext ) ];

               if( ( bCodeNext == ZH_P_PUSHTIMESTAMP ||
                     bCodeNext == ZH_P_PUSHBLOCK ||
                     bCodeNext == ZH_P_PUSHBLOCKSHORT ||
                     bCodeNext == ZH_P_PUSHFIELD ||
                     bCodeNext == ZH_P_PUSHBYTE ||
                     bCodeNext == ZH_P_PUSHINT ||
                     bCodeNext == ZH_P_PUSHLOCAL ||
                     bCodeNext == ZH_P_PUSHLOCALNEAR ||
                     bCodeNext == ZH_P_PUSHLONG ||
                     bCodeNext == ZH_P_PUSHMEMVAR ||
                     bCodeNext == ZH_P_PUSHNIL ||
                     bCodeNext == ZH_P_PUSHDOUBLE ||
                     bCodeNext == ZH_P_PUSHSELF ||
                     bCodeNext == ZH_P_PUSHSTATIC ||
                     bCodeNext == ZH_P_PUSHSTR ||
                     bCodeNext == ZH_P_PUSHSTRSHORT ||
                     bCodeNext == ZH_P_PUSHVARIABLE ||
                     bCodeNext == ZH_P_ONE ||
                     bCodeNext == ZH_P_ZERO ||
                     bCodeNext == ZH_P_PUSHLONGLONG ||
                     bCodeNext == ZH_P_PUSHDATE ||
                     bCodeNext == ZH_P_PUSHSTRHIDDEN ||
                     bCodeNext == ZH_P_PUSHBLOCKLARGE ||
                     bCodeNext == ZH_P_PUSHSTRLARGE ||
                     bCodeNext == ZH_P_LOCALINCPUSH ) &&
                   ( bCodeNext2 == ZH_P_PLUSEQPOP ||
                     bCodeNext2 == ZH_P_MINUSEQPOP ||
                     bCodeNext2 == ZH_P_MULTEQPOP ||
                     bCodeNext2 == ZH_P_DIVEQPOP ||
                     bCodeNext2 == ZH_P_MODEQPOP ||
                     bCodeNext2 == ZH_P_EXPEQPOP ||
                     bCodeNext2 == ZH_P_DECEQPOP ||
                     bCodeNext2 == ZH_P_INCEQPOP ||
                     bCodeNext2 == ZH_P_PLUSEQ ||
                     bCodeNext2 == ZH_P_MINUSEQ ||
                     bCodeNext2 == ZH_P_MULTEQ ||
                     bCodeNext2 == ZH_P_DIVEQ ||
                     bCodeNext2 == ZH_P_MODEQ ||
                     bCodeNext2 == ZH_P_EXPEQ ||
                     bCodeNext2 == ZH_P_DECEQ ||
                     bCodeNext2 == ZH_P_INCEQ ) )
               {
                  pLocals[ isVar - 1 ].bFlags |= OPT_LOCAL_FLAG_CHANGE;
               }
               else
               {
                  pLocals[ isVar - 1 ].bFlags |= OPT_LOCAL_FLAG_PUSHREF;
               }
            }
            break;

         case ZH_P_LOCALADDINT:
         case ZH_P_LOCALNEARADDINT:
         case ZH_P_LOCALDEC:
         case ZH_P_LOCALINC:
            if( isVar > 0 )
               pLocals[ isVar - 1 ].bFlags |= OPT_LOCAL_FLAG_CHANGE;
            break;

         case ZH_P_LOCALINCPUSH:
            if( isVar > 0 )
               pLocals[ isVar - 1 ].bFlags |= ( OPT_LOCAL_FLAG_CHANGE | OPT_LOCAL_FLAG_PUSH );
            break;

         case ZH_P_PUSHBLOCK:
         case ZH_P_PUSHBLOCKLARGE:
         {
            ZH_BYTE * pCode = &pFunc->pCode[ nPos + 5 ];
            ZH_USHORT usVarCount;

            if( pFunc->pCode[ nPos ] == ZH_P_PUSHBLOCKLARGE )
               pCode++;

            usVarCount = ZH_PCODE_MKUSHORT( pCode );
            while( usVarCount-- )
            {
               ZH_USHORT usVar;
               pCode += 2;
               usVar = ZH_PCODE_MKUSHORT( pCode );
               if( usVar > 0 )
                 pLocals[ usVar - 1 ].bFlags |= OPT_LOCAL_FLAG_BLOCK;
            }
            break;
         }

         /* local name is not a big usage...
         case ZH_P_LOCALNAME:
         */
      }
      nLastPos = nPos;
      nPos += zh_compPCodeSize( pFunc, nPos );
   }
}


static void zh_compPCodeEnumSelfifyLocal( PZH_ZFUNC pFunc, ZH_SHORT isLocal )
{
   ZH_SIZE nPos = 0, nLastPos = 0;

   while( nPos < pFunc->nPCodePos )
   {
      switch( pFunc->pCode[ nPos ] )
      {
         case ZH_P_PUSHLOCALNEAR:
            if( isLocal == ( signed char ) pFunc->pCode[ nPos + 1 ] )
            {
               pFunc->pCode[ nPos ] = ZH_P_PUSHSELF;
               zh_compNOOPfill( pFunc, nPos + 1, 1, ZH_FALSE, ZH_FALSE );
            }
            break;

         case ZH_P_PUSHLOCAL:
            if( isLocal == ZH_PCODE_MKSHORT( &pFunc->pCode[ nPos + 1 ] ) )
            {
               pFunc->pCode[ nPos ] = ZH_P_PUSHSELF;
               zh_compNOOPfill( pFunc, nPos + 1, 2, ZH_FALSE, ZH_FALSE );
            }
            break;

         case ZH_P_POPLOCALNEAR:
            if( isLocal == ( signed char ) pFunc->pCode[ nPos + 1 ] )
            {
               assert( nPos > 0 && pFunc->pCode[ nLastPos ] == ZH_P_PUSHSELF &&
                       ! zh_compHasJump( pFunc, nPos ) );

               zh_compNOOPfill( pFunc, nLastPos, 1, ZH_FALSE, ZH_FALSE );
               zh_compNOOPfill( pFunc, nPos, 2, ZH_FALSE, ZH_FALSE );
            }
            break;

         case ZH_P_POPLOCAL:
            if( isLocal == ZH_PCODE_MKSHORT( &pFunc->pCode[ nPos + 1 ] ) )
            {
               assert( nPos > 0 && pFunc->pCode[ nLastPos ] == ZH_P_PUSHSELF &&
                       ! zh_compHasJump( pFunc, nPos ) );

               zh_compNOOPfill( pFunc, nLastPos, 1, ZH_FALSE, ZH_FALSE );
               zh_compNOOPfill( pFunc, nPos, 3, ZH_FALSE, ZH_FALSE );
            }
            break;
      }
      nLastPos = nPos;
      nPos += zh_compPCodeSize( pFunc, nPos );
   }
}


static int zh_compPCodeTraceAssignedUnused( PZH_ZFUNC pFunc, ZH_SIZE nPos, ZH_BYTE * pMap, ZH_SHORT isLocal, ZH_BOOL fCanBreak )
{
   for( ;; )
   {
      if( pMap[ nPos ] )
         return 0;

      pMap[ nPos ] = 1;


      if( pFunc->pCode[ nPos ] == ZH_P_FUNCTION ||
          pFunc->pCode[ nPos ] == ZH_P_FUNCTIONSHORT ||
          pFunc->pCode[ nPos ] == ZH_P_DO ||
          pFunc->pCode[ nPos ] == ZH_P_DOSHORT ||
          pFunc->pCode[ nPos ] == ZH_P_SEND ||
          pFunc->pCode[ nPos ] == ZH_P_SENDSHORT ||
          pFunc->pCode[ nPos ] == ZH_P_MACRODO ||
          pFunc->pCode[ nPos ] == ZH_P_MACROFUNC ||
          pFunc->pCode[ nPos ] == ZH_P_MACROSEND )
      {
         fCanBreak = ZH_TRUE;
      }
      else if( pFunc->pCode[ nPos ] == ZH_P_POPLOCAL ||
               pFunc->pCode[ nPos ] == ZH_P_POPLOCALNEAR ||
               pFunc->pCode[ nPos ] == ZH_P_PUSHLOCAL ||
               pFunc->pCode[ nPos ] == ZH_P_PUSHLOCALNEAR ||
               pFunc->pCode[ nPos ] == ZH_P_PUSHLOCALREF ||
               pFunc->pCode[ nPos ] == ZH_P_LOCALINCPUSH ||
               pFunc->pCode[ nPos ] == ZH_P_LOCALDEC ||
               pFunc->pCode[ nPos ] == ZH_P_LOCALINC ||
               pFunc->pCode[ nPos ] == ZH_P_LOCALADDINT ||
               pFunc->pCode[ nPos ] == ZH_P_LOCALNEARADDINT )

      {
         if( zh_compLocalGetNumber( pFunc->pCode + nPos ) == isLocal )
         {
            if( pFunc->pCode[ nPos ] == ZH_P_POPLOCAL ||
                pFunc->pCode[ nPos ] == ZH_P_POPLOCALNEAR )
            {
               if( fCanBreak )
               {
                  nPos += zh_compPCodeSize( pFunc, nPos );
                  while( pFunc->pCode[ nPos ] != ZH_P_ENDPROC && pFunc->pCode[ nPos ] != ZH_P_ENDBLOCK &&
                         pFunc->pCode[ nPos ] != ZH_P_SEQBEGIN && pFunc->pCode[ nPos ] != ZH_P_SEQEND )
                  {
                     nPos += zh_compPCodeSize( pFunc, nPos );
                  }
                  if( pFunc->pCode[ nPos ] == ZH_P_SEQEND )
                  {
                     /* Process RECOVER */
                     fCanBreak = ZH_FALSE;
                     nPos += zh_compPCodeSize( pFunc, nPos );
                     continue;
                  }
                  return 0;
               }
               else
                  return 0;
            }
            else
               return 1;
         }
      }

      /* The following part of the function is standard for all recursive pcode
         tracing, except recursive function calls are hardcoded. We can implement
         universal recursive tracer by putting all parameters into "cargo"
         structure. [Mindaugas] */

      if( zh_compIsJump( pFunc->pCode[ nPos ] ) )
      {
         ZH_SIZE nPos2 = nPos + zh_compJumpGetOffset( &pFunc->pCode[ nPos ] );

         if( zh_compIsUncondJump( pFunc->pCode[ nPos ] ) )
         {
            nPos = nPos2;
            continue;
         }

         if( zh_compPCodeTraceAssignedUnused( pFunc, nPos2, pMap, isLocal, fCanBreak ) )
            return 1;
      }
      else if( pFunc->pCode[ nPos ] == ZH_P_SWITCH ) /* Switch is multi-place jump */
      {
         ZH_USHORT us, usCount = ZH_PCODE_MKUSHORT( pFunc->pCode + nPos + 1 );

         nPos += 3;
         for( us = 0; us < usCount; us++ )
         {
            if( zh_compPCodeTraceAssignedUnused( pFunc, nPos, pMap, isLocal, fCanBreak ) )
               return 1;

            nPos += zh_compPCodeSize( pFunc, nPos );
            nPos += zh_compPCodeSize( pFunc, nPos );
         }
         continue;
      }

      if( pFunc->pCode[ nPos ] == ZH_P_ENDPROC || pFunc->pCode[ nPos ] == ZH_P_ENDBLOCK )
         break;

      nPos += zh_compPCodeSize( pFunc, nPos );
   }
   return 0;
}


static void zh_compPCodeEnumAssignedUnused( ZH_COMP_DECL, PZH_ZFUNC pFunc, PZH_OPT_LOCAL pLocals )
{
   ZH_BYTE * pMap;
   ZH_SIZE nPos = 0, nLastPos = 0;
   ZH_USHORT usLine = 0;

   pMap = ( ZH_BYTE * ) zh_xgrab( pFunc->nPCodePos );

   while( nPos < pFunc->nPCodePos )
   {
      ZH_SHORT isLocal;
      int iCheck = 0;

      if( pFunc->pCode[ nPos ] == ZH_P_POPLOCAL ||
          pFunc->pCode[ nPos ] == ZH_P_POPLOCALNEAR )
      {
         /* skip pop NIL (var := NIL), to allow force garbage collection */
         if( nPos == 0 || pFunc->pCode[ nLastPos ] != ZH_P_PUSHNIL )
            iCheck = 1;
      }
      else if( pFunc->pCode[ nPos ] == ZH_P_LOCALDEC ||
               pFunc->pCode[ nPos ] == ZH_P_LOCALINC ||
               pFunc->pCode[ nPos ] == ZH_P_LOCALADDINT ||
               pFunc->pCode[ nPos ] == ZH_P_LOCALNEARADDINT )
      {
         iCheck = 1;
      }
      else if( pFunc->pCode[ nPos ] == ZH_P_PUSHLOCALREF )
      {
         ZH_SIZE nPosNext = nPos + zh_compPCodeSize( pFunc, nPos );

         switch( pFunc->pCode[ nPosNext ] )
         {
            case ZH_P_PUSHTIMESTAMP:
            case ZH_P_PUSHBLOCK:
            case ZH_P_PUSHBLOCKSHORT:
            case ZH_P_PUSHFIELD:
            case ZH_P_PUSHBYTE:
            case ZH_P_PUSHINT:
            case ZH_P_PUSHLOCAL:
            case ZH_P_PUSHLOCALNEAR:
            case ZH_P_PUSHLONG:
            case ZH_P_PUSHMEMVAR:
            case ZH_P_PUSHNIL:
            case ZH_P_PUSHDOUBLE:
            case ZH_P_PUSHSELF:
            case ZH_P_PUSHSTATIC:
            case ZH_P_PUSHSTR:
            case ZH_P_PUSHSTRSHORT:
            case ZH_P_PUSHVARIABLE:
            case ZH_P_ONE:
            case ZH_P_ZERO:
            case ZH_P_PUSHLONGLONG:
            case ZH_P_PUSHDATE:
            case ZH_P_PUSHSTRHIDDEN:
            case ZH_P_PUSHBLOCKLARGE:
            case ZH_P_PUSHSTRLARGE:
            case ZH_P_LOCALINCPUSH:
               switch( pFunc->pCode[ nPosNext + zh_compPCodeSize( pFunc, nPosNext ) ] )
               {
                  case ZH_P_PLUSEQPOP:
                  case ZH_P_MINUSEQPOP:
                  case ZH_P_MULTEQPOP:
                  case ZH_P_DIVEQPOP:
                  case ZH_P_MODEQPOP:
                  case ZH_P_EXPEQPOP:
                  case ZH_P_DECEQPOP:
                  case ZH_P_INCEQPOP:
                     iCheck = 1;
                     break;
                  case ZH_P_PLUSEQ:
                  case ZH_P_MINUSEQ:
                  case ZH_P_MULTEQ:
                  case ZH_P_DIVEQ:
                  case ZH_P_MODEQ:
                  case ZH_P_EXPEQ:
                  case ZH_P_DECEQ:
                  case ZH_P_INCEQ:
                     iCheck = 2;
                     break;
               }
         }
      }

      if( iCheck != 0 && ( isLocal = zh_compLocalGetNumber( &pFunc->pCode[ nPos ] ) ) > ( ZH_SHORT ) pFunc->wParamCount )
      {
         PZH_HVAR pVar = pFunc->pLocals;
         ZH_SHORT is;

         for( is = 1; is < isLocal; is++ )
            pVar = pVar->pNext;

         assert( pLocals[ isLocal - 1 ].bFlags != 0 );

         /* Skip detachables, referenced, optimizable self */
         if( ( pLocals[ isLocal - 1 ].bFlags & ( OPT_LOCAL_FLAG_BLOCK | OPT_LOCAL_FLAG_PUSHREF ) ) == 0 &&
             pLocals[ isLocal - 1 ].bFlags != OPT_LOCAL_FLAG_POPSELF &&
             pLocals[ isLocal - 1 ].bFlags != ( OPT_LOCAL_FLAG_PUSH | OPT_LOCAL_FLAG_POPSELF ) )
         {
            memset( pMap, 0, pFunc->nPCodePos );
            if( iCheck == 1 )
               pMap[ nPos ] = 1;

            if( ! zh_compPCodeTraceAssignedUnused( pFunc, nPos + zh_compPCodeSize( pFunc, nPos ),
                                                   pMap, isLocal, ZH_FALSE ) )
            {
               char szFun[ 256 ];

               /* FIXME: We calculate line number by simple tracking last ZH_P_LINE,
                  but it can work bad, if line number optimizer is clever enough.
                  To obtain real line number we need one more tree scan or other
                  algorithm. [Mindaugas] */

               if( ZH_COMP_PARAM->iErrorFmt == ZH_ERRORFMT_CLIPPER )
                  zh_snprintf( szFun, sizeof( szFun ), "%s(%i)", pFunc->szName, usLine );
               else
                  zh_snprintf( szFun, sizeof( szFun ), "%i:%s", usLine, pFunc->szName );
               zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_ASSIGNED_UNUSED, pVar->szName, szFun );
            }
         }
      }
      else if( pFunc->pCode[ nPos ] == ZH_P_LINE )
      {
         usLine = ZH_PCODE_MKUSHORT( pFunc->pCode + nPos + 1 );
      }
      nLastPos = nPos;
      nPos += zh_compPCodeSize( pFunc, nPos );
   }
   zh_xfree( pMap );
}


static void zh_compPCodeEnumRenumberLocals( PZH_ZFUNC pFunc, PZH_OPT_LOCAL pLocals )
{
   ZH_SIZE nPos = 0;

   while( nPos < pFunc->nPCodePos )
   {
      switch( pFunc->pCode[ nPos ] )
      {
         case ZH_P_POPLOCALNEAR:
         case ZH_P_PUSHLOCALNEAR:
         case ZH_P_LOCALNEARADDINT:
         {
            ZH_BYTE * pVar = &pFunc->pCode[ nPos + 1 ];
            ZH_SHORT isVar = ( signed char ) pVar[ 0 ];

            if( isVar > 0 && pLocals[ isVar - 1 ].isNumber != isVar )
            {
               isVar = pLocals[ isVar - 1 ].isNumber;
               if( isVar > 0 )
               {
                  pVar[ 0 ] = ZH_LOBYTE( isVar );
                  pVar[ 1 ] = ZH_HIBYTE( isVar );
               }
               else
               {
                  zh_compNOOPfill( pFunc, nPos, zh_compPCodeSize( pFunc, nPos ), ZH_FALSE, ZH_FALSE );
               }
            }
            break;
         }

         case ZH_P_LOCALNAME:
         case ZH_P_POPLOCAL:
         case ZH_P_PUSHLOCAL:
         case ZH_P_PUSHLOCALREF:
         case ZH_P_LOCALADDINT:
         case ZH_P_LOCALDEC:
         case ZH_P_LOCALINC:
         case ZH_P_LOCALINCPUSH:
         {
            ZH_BYTE * pVar = &pFunc->pCode[ nPos + 1 ];
            ZH_SHORT isVar = ZH_PCODE_MKSHORT( pVar );

            if( isVar > 0 && pLocals[ isVar - 1 ].isNumber != isVar )
            {
               isVar = pLocals[ isVar - 1 ].isNumber;
               if( isVar > 0 )
               {
                  pVar[ 0 ] = ZH_LOBYTE( isVar );
                  pVar[ 1 ] = ZH_HIBYTE( isVar );
               }
               else
               {
                  zh_compNOOPfill( pFunc, nPos, zh_compPCodeSize( pFunc, nPos ), ZH_FALSE, ZH_FALSE );
               }
            }
            break;
         }

         case ZH_P_PUSHBLOCK:
         case ZH_P_PUSHBLOCKLARGE:
         {
            ZH_BYTE * pVar = &pFunc->pCode[ nPos + 5 ];
            ZH_USHORT usVarCount;

            if( pFunc->pCode[ nPos ] == ZH_P_PUSHBLOCKLARGE )
               pVar++;

            usVarCount = ZH_PCODE_MKUSHORT( pVar );
            while( usVarCount-- )
            {
               ZH_SHORT isVar;

               pVar += 2;
               isVar = ZH_PCODE_MKSHORT( pVar );

               if( isVar > 0 && pLocals[ isVar - 1 ].isNumber != isVar )
               {
                  isVar = pLocals[ isVar - 1 ].isNumber;

                  assert( isVar > 0 );  /* We do not allow removal of detached locals */

                  pVar[ 0 ] = ZH_LOBYTE( isVar );
                  pVar[ 1 ] = ZH_HIBYTE( isVar );
               }
            }
            break;
         }
      }
      nPos += zh_compPCodeSize( pFunc, nPos );
   }
}


void zh_compPCodeTraceOptimizer( ZH_COMP_DECL )
{
   PZH_ZFUNC     pFunc = ZH_COMP_PARAM->functions.pLast;
   PZH_OPT_LOCAL pLocals;
   PZH_HVAR      pVar;
   ZH_USHORT     usLocalCount, usIndex;

   /* Many (perhaps ALL) functions of pcode trace optimization depends on pcodes.
      Please, check these functions if new pcode is added, or existing changed.
      Special attention should be paid, if new pcode introduces branching, codeblocks,
      or are related to parameters, local variables. [Mindaugas] */

   assert( ZH_P_LAST_PCODE == 181 );

   usLocalCount = 0;
   pVar = pFunc->pLocals;
   while( pVar )
   {
      pVar = pVar->pNext;
      usLocalCount++;
   }

   if( ! usLocalCount )
      return;

   /* FIXME: Support for PARAMETER sentence is not implemented.
             The temporary solution is to disable optimization at all if PARAMETER is used.  */
   {
      ZH_SIZE nPos = 0;

      while( nPos < pFunc->nPCodePos )
      {
         if( pFunc->pCode[ nPos ] == ZH_P_PARAMETER )
            return;
         nPos += zh_compPCodeSize( pFunc, nPos );
      }
   }

   /* Initial scan */
   pLocals = ( PZH_OPT_LOCAL ) zh_xgrabz( sizeof( ZH_OPT_LOCAL ) * usLocalCount );
   zh_compPCodeEnumScanLocals( pFunc, pLocals );

   /* Check */
   usIndex = 0;
   pVar = pFunc->pLocals;
   while( pVar )
   {
      /* Compiler and optimizer should have the same opinion about variable usage */
      /* I hope that it will in the future but now compiler does not detect some
       * dead code before marking variables as used so this conditions fails in
       * code like:
       *    proc main()
       *       local x
       *       if .F.
       *          x := 1
       *       endif
       *       return
       * [druzus]
       */
#if 0
      assert( ( ! ( pVar->iUsed & ZH_VU_USED ) && pLocals[ usIndex ].bFlags == 0 ) ||
              (   ( pVar->iUsed & ZH_VU_USED ) && pLocals[ usIndex ].bFlags != 0 ) );
#endif

      if( usIndex >= pFunc->wParamCount && pLocals[ usIndex ].bFlags == OPT_LOCAL_FLAG_PUSH )
      {
         char szFun[ 256 ];

         if( ZH_COMP_PARAM->iErrorFmt == ZH_ERRORFMT_CLIPPER )
            zh_snprintf( szFun, sizeof( szFun ), "%s(%i)", pFunc->szName, pVar->iDeclLine );
         else
            zh_snprintf( szFun, sizeof( szFun ), "%i:%s", pVar->iDeclLine, pFunc->szName );
         zh_compGenWarning( ZH_COMP_PARAM, zh_comp_szWarnings, 'W', ZH_COMP_WARN_NEVER_ASSIGNED, pVar->szName, szFun );
      }

      pVar = pVar->pNext;
      usIndex++;
   }

   /* Selfifying */
   if( ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_OPTJUMP ) && ! ZH_COMP_PARAM->fDebugInfo )
   {
      pVar = pFunc->pLocals;
      for( usIndex = 0; usIndex < pFunc->wParamCount; usIndex++ )
         pVar = pVar->pNext;

      for( usIndex = pFunc->wParamCount; usIndex < usLocalCount; usIndex++ )
      {
         if( pLocals[ usIndex ].bFlags == ( OPT_LOCAL_FLAG_PUSH | OPT_LOCAL_FLAG_POPSELF ) ||
             pLocals[ usIndex ].bFlags == OPT_LOCAL_FLAG_POPSELF )
         {
            #if 0
            printf( "Info: %s(%d) selfifying variable '%s'\n", pFunc->szName, pVar->iDeclLine, pVar->szName );
            #endif
            zh_compPCodeEnumSelfifyLocal( pFunc, usIndex + 1 );
            pLocals[ usIndex ].bFlags = 0;
         }
         pVar = pVar->pNext;
      }
   }

   /* Scan assigned, but not used */
   zh_compPCodeEnumAssignedUnused( ZH_COMP_PARAM, pFunc, pLocals );

   /* Delete unused */
   if( ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_OPTJUMP ) && ! ZH_COMP_PARAM->fDebugInfo )
   {
      ZH_BOOL fBool = ZH_FALSE;

      for( usIndex = pFunc->wParamCount; usIndex < usLocalCount; usIndex++ )
      {
         if( pLocals[ usIndex ].bFlags == 0 )
         {
            fBool = ZH_TRUE;
            break;
         }
      }

      if( fBool )
      {
         PZH_HVAR * ppVar;

         usIndex = usLocalCount = 0;
         ppVar = & pFunc->pLocals;
         pVar = pFunc->pLocals;
         while( pVar )
         {
            if( usLocalCount < pFunc->wParamCount || pLocals[ usLocalCount ].bFlags != 0 )
            {
               pLocals[ usLocalCount ].isNumber = ++usIndex;
               ppVar = & pVar->pNext;
               pVar = pVar->pNext;
            }
            else
            {
               #if 0
               printf( "Info: %s(%d) removing unused variable '%s'\n", pFunc->szName, pVar->iDeclLine, pVar->szName );
               #endif

               /* Delete pVar from the linked list */
               *ppVar = pVar->pNext;
               zh_xfree( pVar );
               pVar = *ppVar;
            }
            usLocalCount++;
         }
         zh_compPCodeEnumRenumberLocals( pFunc, pLocals );
      }
   }
   zh_xfree( pLocals );
}
