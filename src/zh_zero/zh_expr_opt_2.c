/*
 * Compiler Expression Optimizer - reducing expressions
 *
 * Copyright 1999 Ryszard Glab
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

/* NOTE: This must be the first definition
 *    This is a common code shared by macro and standalone compiler
 */
#define  ZH_COMMON_SUPPORT

#include "zh_macro.h"
#include "zh_comp.h"
#include "zh_date.h"
#include "zh_math.h"

static ZH_BOOL zh_compExprHasMacro( const char * szText, ZH_SIZE nLen, ZH_COMP_DECL )
{
   while( nLen-- )
   {
      if( *szText++ == '&' )
      {
         if( ! ZH_SUPPORT_ZIHER || ( nLen && ( *szText == '_' ||
             ( *szText >= 'A' && *szText <= 'Z' ) ||
             ( *szText >= 'a' && *szText <= 'z' ) ) ) )
         {
            return ZH_TRUE;
         }
      }
   }
   return ZH_FALSE;
}

static PZH_EXPR zh_compExprReducePlusStrings( PZH_EXPR pLeft, PZH_EXPR pRight, ZH_COMP_DECL )
{
   if( pLeft->value.asString.dealloc )
   {
      pLeft->value.asString.string = ( char * ) zh_xrealloc( pLeft->value.asString.string, pLeft->nLength + pRight->nLength + 1 );
      memcpy( pLeft->value.asString.string + pLeft->nLength,
              pRight->value.asString.string, pRight->nLength );
      pLeft->nLength += pRight->nLength;
      pLeft->value.asString.string[ pLeft->nLength ] = '\0';
   }
   else
   {
      char * szString;
      szString = ( char * ) zh_xgrab( pLeft->nLength + pRight->nLength + 1 );
      memcpy( szString, pLeft->value.asString.string, pLeft->nLength );
      memcpy( szString + pLeft->nLength, pRight->value.asString.string, pRight->nLength );
      pLeft->nLength += pRight->nLength;
      szString[ pLeft->nLength ] = '\0';
      pLeft->value.asString.string = szString;
      pLeft->value.asString.dealloc = ZH_TRUE;
   }
   ZH_COMP_EXPR_FREE( pRight );
   return pLeft;
}

static PZH_EXPR zh_compExprReduceMinusStrings( PZH_EXPR pLeft, PZH_EXPR pRight, ZH_COMP_DECL )
{
   char * szText = pLeft->value.asString.string;
   ZH_SIZE nLen = pLeft->nLength;

   while( nLen && szText[ nLen - 1 ] == ' ' )
      --nLen;

   if( pLeft->value.asString.dealloc )
   {
      pLeft->value.asString.string = ( char * ) zh_xrealloc( pLeft->value.asString.string, pLeft->nLength + pRight->nLength + 1 );
      memcpy( pLeft->value.asString.string + nLen,
              pRight->value.asString.string, pRight->nLength );
      memset( pLeft->value.asString.string + nLen + pRight->nLength, ' ',
              pLeft->nLength - nLen );
      pLeft->nLength += pRight->nLength;
      pLeft->value.asString.string[ pLeft->nLength ] = '\0';
   }
   else
   {
      char * szString;
      szString = ( char * ) zh_xgrab( pLeft->nLength + pRight->nLength + 1 );
      memcpy( szString, pLeft->value.asString.string, nLen );
      memcpy( szString + nLen, pRight->value.asString.string, pRight->nLength );
      memset( szString + nLen + pRight->nLength, ' ', pLeft->nLength - nLen );
      pLeft->nLength += pRight->nLength;
      szString[ pLeft->nLength ] = '\0';
      pLeft->value.asString.string = szString;
      pLeft->value.asString.dealloc = ZH_TRUE;
   }
   ZH_COMP_EXPR_FREE( pRight );
   return pLeft;
}

PZH_EXPR zh_compExprReduceMod( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == ZH_ET_NUMERIC && pRight->ExprType == ZH_ET_NUMERIC )
   {
      switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
      {
         case ZH_ET_LONG:
            if( pRight->value.asNum.val.l )
            {
               pSelf->value.asNum.val.l = pLeft->value.asNum.val.l % pRight->value.asNum.val.l;
               pSelf->value.asNum.bDec = 0;
               pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
               pSelf->value.asNum.NumType = ZH_ET_LONG;
               pSelf->ExprType = ZH_ET_NUMERIC;
               pSelf->ValType  = ZH_EV_NUMERIC;
               ZH_COMP_EXPR_FREE( pLeft );
               ZH_COMP_EXPR_FREE( pRight );
            }
            break;

         default:
            if( ZH_SUPPORT_ZIHER )
            {
               double dDivisor = pRight->value.asNum.NumType == ZH_ET_LONG ?
                                 ( double ) pRight->value.asNum.val.l :
                                 pRight->value.asNum.val.d;
               if( dDivisor )
               {
                  double dValue = pLeft->value.asNum.NumType == ZH_ET_LONG ?
                                  ( double ) pLeft->value.asNum.val.l :
                                  pLeft->value.asNum.val.d;
                  pSelf->value.asNum.val.d = fmod( dValue, dDivisor );
                  pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
                  pSelf->value.asNum.bDec = ZH_DEFAULT_DECIMALS;
                  pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
                  pSelf->ExprType = ZH_ET_NUMERIC;
                  pSelf->ValType  = ZH_EV_NUMERIC;
                  ZH_COMP_EXPR_FREE( pLeft );
                  ZH_COMP_EXPR_FREE( pRight );
               }
            }
      }
   }
   else
   {
      /* TODO: Check for incompatible types e.g.  3 % "txt"
       */
   }
   return pSelf;
}

PZH_EXPR zh_compExprReduceDiv( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == ZH_ET_NUMERIC && pRight->ExprType == ZH_ET_NUMERIC )
   {
      ZH_BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

      switch( bType )
      {
         case ZH_ET_LONG:

            if( pRight->value.asNum.val.l )
            {
               if( pLeft->value.asNum.val.l % pRight->value.asNum.val.l == 0 )
               {
                  /* Return integer results as long */
                  pSelf->value.asNum.val.l = pLeft->value.asNum.val.l / pRight->value.asNum.val.l;
                  pSelf->value.asNum.bDec = 0;
                  pSelf->value.asNum.NumType = ZH_ET_LONG;
               }
               else
               {
                  /* Return non-integer results as double */
                  pSelf->value.asNum.val.d = ( double ) pLeft->value.asNum.val.l / ( double ) pRight->value.asNum.val.l;
                  pSelf->value.asNum.bDec = ZH_DEFAULT_DECIMALS;
                  pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
               }
               pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
               pSelf->ExprType = ZH_ET_NUMERIC;
            }
            break;

         case ZH_ET_DOUBLE:

            if( pRight->value.asNum.val.d != 0.0 )
            {
               pSelf->value.asNum.val.d = pLeft->value.asNum.val.d / pRight->value.asNum.val.d;
               pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
               pSelf->value.asNum.bDec = ZH_DEFAULT_DECIMALS;
               pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
               pSelf->ExprType = ZH_ET_NUMERIC;
            }
            break;

         default:

            if( pLeft->value.asNum.NumType == ZH_ET_DOUBLE )
            {
               if( pRight->value.asNum.val.l )
               {
                  pSelf->value.asNum.val.d = pLeft->value.asNum.val.d / ( double ) pRight->value.asNum.val.l;
                  pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
                  pSelf->value.asNum.bDec = ZH_DEFAULT_DECIMALS;
                  pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
                  pSelf->ExprType = ZH_ET_NUMERIC;
               }
            }
            else
            {
               if( pRight->value.asNum.val.d != 0.0 )
               {
                  pSelf->value.asNum.val.d = ( double ) pLeft->value.asNum.val.l / pRight->value.asNum.val.d;
                  pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
                  pSelf->value.asNum.bDec = ZH_DEFAULT_DECIMALS;
                  pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
                  pSelf->ExprType = ZH_ET_NUMERIC;
               }
            }

      } /* switch bType */

      if( pSelf->ExprType == ZH_ET_NUMERIC )
      {
         /* The expression was reduced - delete old components */
         pSelf->ValType = ZH_EV_NUMERIC;
         ZH_COMP_EXPR_FREE( pLeft );
         ZH_COMP_EXPR_FREE( pRight );
      }
   }
   else
   {
      /* TODO: Check for incompatible types e.g.  3 / "txt"
       */
   }
   return pSelf;
}

PZH_EXPR zh_compExprReduceMult( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == ZH_ET_NUMERIC && pRight->ExprType == ZH_ET_NUMERIC )
   {
      ZH_BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

      switch( bType )
      {
         case ZH_ET_LONG:
         {
            ZH_MAXDBL dVal = ( ZH_MAXDBL ) pLeft->value.asNum.val.l * ( ZH_MAXDBL ) pRight->value.asNum.val.l;

            if( ZH_DBL_LIM_LONG( dVal ) )
            {
               pSelf->value.asNum.val.l = pLeft->value.asNum.val.l * pRight->value.asNum.val.l;
               pSelf->value.asNum.NumType = ZH_ET_LONG;
            }
            else
            {
               pSelf->value.asNum.val.d = ( double ) dVal;
               pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
            }
            pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
            pSelf->value.asNum.bDec = 0;
            break;
         }

         case ZH_ET_DOUBLE:

            pSelf->value.asNum.val.d   = pLeft->value.asNum.val.d * pRight->value.asNum.val.d;
            pSelf->value.asNum.bWidth  = ZH_DEFAULT_WIDTH;
            pSelf->value.asNum.bDec    = ( ZH_UCHAR ) ZH_MIN( pLeft->value.asNum.bDec + pRight->value.asNum.bDec, ZH_DEFAULT_DECIMALS );
            pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
            break;

         default:

            if( pLeft->value.asNum.NumType == ZH_ET_DOUBLE )
            {
               pSelf->value.asNum.val.d = pLeft->value.asNum.val.d * ( double ) pRight->value.asNum.val.l;
               pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
            }
            else
            {
               pSelf->value.asNum.val.d = ( double ) pLeft->value.asNum.val.l * pRight->value.asNum.val.d;
               pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
            }
            pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
            pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
      }
      pSelf->ExprType = ZH_ET_NUMERIC;
      pSelf->ValType  = ZH_EV_NUMERIC;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   else
   {
      /* TODO: Check for incompatible types e.g. 3 * "txt"
       */
   }
   return pSelf;
}

PZH_EXPR zh_compExprReducePower( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == ZH_ET_NUMERIC && pRight->ExprType == ZH_ET_NUMERIC )
   {
      ZH_BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

      switch( bType )
      {
         case ZH_ET_LONG:
            pSelf->value.asNum.val.d = pow( ( double ) pLeft->value.asNum.val.l,
                                            ( double ) pRight->value.asNum.val.l );
            break;

         case ZH_ET_DOUBLE:
            pSelf->value.asNum.val.d = pow( pLeft->value.asNum.val.d,
                                            pRight->value.asNum.val.d );
            break;

         default:
            if( pLeft->value.asNum.NumType == ZH_ET_DOUBLE )
               pSelf->value.asNum.val.d = pow( pLeft->value.asNum.val.d,
                                               ( double ) pRight->value.asNum.val.l );
            else
               pSelf->value.asNum.val.d = pow( ( double ) pLeft->value.asNum.val.l,
                                               pRight->value.asNum.val.d );
            break;
      }
      pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
      pSelf->value.asNum.bDec = ZH_DEFAULT_DECIMALS;
      pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
      pSelf->ExprType = ZH_ET_NUMERIC;
      pSelf->ValType  = ZH_EV_NUMERIC;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   else
   {
      /* TODO: Check for incompatible types e.g. 3 * "txt"
       */
   }
   return pSelf;
}

static void zh_compExprReduceTimeStampPut( PZH_EXPR pExpr, long lJulian, long lMilliSec )
{
   /* timestamp normalization */
   if( lJulian < 0 )
   {
      if( lMilliSec <= -ZH_MILLISECS_PER_DAY )
      {
         lMilliSec += ZH_MILLISECS_PER_DAY;
         --lJulian;
      }
      else if( lMilliSec > 0 )
      {
         lMilliSec -= ZH_MILLISECS_PER_DAY;
         ++lJulian;
         if( lMilliSec > 0 )
         {
            lMilliSec -= ZH_MILLISECS_PER_DAY;
            ++lJulian;
         }
      }
   }
   else
   {
      if( lMilliSec >= ZH_MILLISECS_PER_DAY )
      {
         lMilliSec -= ZH_MILLISECS_PER_DAY;
         ++lJulian;
      }
      else if( lMilliSec < 0 )
      {
         lMilliSec += ZH_MILLISECS_PER_DAY;
         --lJulian;
         if( lMilliSec < 0 )
         {
            lMilliSec += ZH_MILLISECS_PER_DAY;
            --lJulian;
         }
      }
   }

   pExpr->value.asDate.lDate = lJulian;
   pExpr->value.asDate.lTime = lMilliSec;
   pExpr->ExprType = ZH_ET_TIMESTAMP;
   pExpr->ValType  = ZH_EV_TIMESTAMP;
}

static void zh_compExprReduceTimeStampAdd( PZH_EXPR pExpr, PZH_EXPR pTimeStamp, double dValue )
{
   long lJulian, lMilliSec;

   zh_timeStampUnpackDT( dValue, &lJulian, &lMilliSec );

   lJulian   += pTimeStamp->value.asDate.lDate;
   lMilliSec += pTimeStamp->value.asDate.lTime;

   zh_compExprReduceTimeStampPut( pExpr, lJulian, lMilliSec );
}

PZH_EXPR zh_compExprReduceMinus( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == ZH_ET_NUMERIC && pRight->ExprType == ZH_ET_NUMERIC )
   {
      ZH_BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

      switch( bType )
      {
         case ZH_ET_LONG:
         {
            ZH_MAXDBL dVal = ( ZH_MAXDBL ) pLeft->value.asNum.val.l - ( ZH_MAXDBL ) pRight->value.asNum.val.l;

            if( ZH_DBL_LIM_LONG( dVal ) )
            {
               pSelf->value.asNum.val.l = pLeft->value.asNum.val.l - pRight->value.asNum.val.l;
               pSelf->value.asNum.NumType = ZH_ET_LONG;
            }
            else
            {
               pSelf->value.asNum.val.d = ( double ) dVal;
               pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
            }
            pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
            pSelf->value.asNum.bDec = 0;

            break;
         }

         case ZH_ET_DOUBLE:

            pSelf->value.asNum.val.d = pLeft->value.asNum.val.d - pRight->value.asNum.val.d;
            pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
            if( pLeft->value.asNum.bDec < pRight->value.asNum.bDec )
               pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
            else
               pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
            pSelf->value.asNum.NumType = ZH_ET_DOUBLE;

            break;

         default:

            if( pLeft->value.asNum.NumType == ZH_ET_DOUBLE )
            {
               pSelf->value.asNum.val.d = pLeft->value.asNum.val.d - ( double ) pRight->value.asNum.val.l;
               pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
            }
            else
            {
               pSelf->value.asNum.val.d = ( double ) pLeft->value.asNum.val.l - pRight->value.asNum.val.d;
               pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
            }
            pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
            pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
      }
      pSelf->ExprType = ZH_ET_NUMERIC;
      pSelf->ValType  = ZH_EV_NUMERIC;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   else if( ( pLeft->ExprType == ZH_ET_DATE || pLeft->ExprType == ZH_ET_TIMESTAMP ) &&
            ( pRight->ExprType == ZH_ET_DATE || pRight->ExprType == ZH_ET_TIMESTAMP ) )
   {
      long lTime = pLeft->value.asDate.lTime - pRight->value.asDate.lTime,
           lDate = pLeft->value.asDate.lDate - pRight->value.asDate.lDate;
      if( lTime == 0 )
      {
         pSelf->value.asNum.val.l = lDate;
         pSelf->value.asNum.bDec = 0;
         pSelf->value.asNum.NumType = ZH_ET_LONG;
      }
      else
      {
         pSelf->value.asNum.val.d = zh_timeStampPackDT( lDate, lTime );
         pSelf->value.asNum.bDec = ZH_TIMEDIFF_DEC;
         pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
      }
      pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
      pSelf->ExprType = ZH_ET_NUMERIC;
      pSelf->ValType  = ZH_EV_NUMERIC;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   else if( pLeft->ExprType == ZH_ET_DATE && pRight->ExprType == ZH_ET_NUMERIC )
   {
      if( pRight->value.asNum.NumType == ZH_ET_LONG )
         pSelf->value.asDate.lDate =  pLeft->value.asDate.lDate - ( long ) pRight->value.asNum.val.l;
      else
         pSelf->value.asDate.lDate = pLeft->value.asDate.lDate - ZH_CAST_LONG( pRight->value.asNum.val.d );
      pSelf->value.asDate.lTime = 0;
      pSelf->ExprType = ZH_ET_DATE;
      pSelf->ValType  = ZH_EV_DATE;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   else if( pLeft->ExprType == ZH_ET_TIMESTAMP && pRight->ExprType == ZH_ET_NUMERIC )
   {
      if( pRight->value.asNum.NumType == ZH_ET_LONG )
         zh_compExprReduceTimeStampPut( pSelf, pLeft->value.asDate.lDate - ( long ) pRight->value.asNum.val.l,
                                        pLeft->value.asDate.lTime );
      else
         zh_compExprReduceTimeStampAdd( pSelf, pLeft, -pRight->value.asNum.val.d );
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   else if( pLeft->ExprType == ZH_ET_STRING && pRight->ExprType == ZH_ET_STRING )
   {
      if( pRight->nLength == 0 )
      {
         pSelf->ExprType = ZH_ET_NONE; /* suppress deletion of operator components */
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft;
         ZH_COMP_EXPR_FREE( pRight );
      }
      else if( pLeft->nLength == 0 )
      {
         pSelf->ExprType = ZH_ET_NONE; /* suppress deletion of operator components */
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pRight;
         ZH_COMP_EXPR_FREE( pLeft );
      }
      else
      {
         ZH_BOOL fReduce = ZH_TRUE;

         /* Do not reduce strings with the macro operator '&'
          */
         if( ZH_SUPPORT_MACROTEXT )
         {
            char *  szText = pLeft->value.asString.string;
            ZH_SIZE nLen   = pLeft->nLength;
            while( nLen && szText[ nLen - 1 ] == ' ' )
               --nLen;
            while( nLen-- )
            {
               if( *szText++ == '&' )
               {
                  char ch = nLen ? *szText : *pRight->value.asString.string;
                  if( ( ch >= 'A' && ch <= 'Z' ) ||
                      ( ch >= 'a' && ch <= 'z' ) || ch == '_' ||
                      ! ZH_SUPPORT_ZIHER )
                  {
                     fReduce = ZH_FALSE;
                     break;
                  }
               }
            }
         }

         if( fReduce )
         {
            pSelf->ExprType = ZH_ET_NONE; /* suppress deletion of operator components */
            ZH_COMP_EXPR_FREE( pSelf );
            pSelf = zh_compExprReduceMinusStrings( pLeft, pRight, ZH_COMP_PARAM );
         }
      }
   }
   else
   {
      /* TODO: Check for incompatible types e.g. "txt" - 3
       */
   }
   return pSelf;
}

static ZH_BOOL zh_compExprReducePlusNums( PZH_EXPR pSelf, PZH_EXPR pAdd )
{
   PZH_EXPR pLeft, pRight, pNum;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == ZH_ET_NUMERIC )
      pNum = pLeft;
   else if( pRight->ExprType == ZH_ET_NUMERIC )
      pNum = pRight;
   else if( pLeft->ExprType == ZH_EO_PLUS )
      return zh_compExprReducePlusNums( pLeft, pAdd );
   else if( pRight->ExprType == ZH_EO_PLUS )
      return zh_compExprReducePlusNums( pRight, pAdd );
   else
      return ZH_FALSE;

   switch( pNum->value.asNum.NumType & pAdd->value.asNum.NumType )
   {
      case ZH_ET_LONG:
      {
         ZH_MAXDBL dVal = ( ZH_MAXDBL ) pNum->value.asNum.val.l + ( ZH_MAXDBL ) pAdd->value.asNum.val.l;
         if( ZH_DBL_LIM_LONG( dVal ) )
            pNum->value.asNum.val.l += pAdd->value.asNum.val.l;
         else
         {
            pNum->value.asNum.val.d = ( double ) dVal;
            pNum->value.asNum.NumType = ZH_ET_DOUBLE;
         }
         pNum->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
         pNum->value.asNum.bDec = 0;
         break;
      }

      case ZH_ET_DOUBLE:
         pNum->value.asNum.val.d += pAdd->value.asNum.val.d;
         pNum->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
         if( pNum->value.asNum.bDec < pAdd->value.asNum.bDec )
            pNum->value.asNum.bDec = pAdd->value.asNum.bDec;
         break;

      default:
         if( pNum->value.asNum.NumType == ZH_ET_DOUBLE )
            pNum->value.asNum.val.d += ( double ) pAdd->value.asNum.val.l;
         else
         {
            pNum->value.asNum.val.d = ( double ) pNum->value.asNum.val.l + pAdd->value.asNum.val.d;
            pNum->value.asNum.bDec = pAdd->value.asNum.bDec;
            pNum->value.asNum.NumType = ZH_ET_DOUBLE;
         }
         pNum->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
         break;
   }

   return ZH_TRUE;
}

PZH_EXPR zh_compExprReducePlus( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == ZH_ET_NUMERIC )
   {
      if( pRight->ExprType == ZH_ET_NUMERIC )
      {
         ZH_BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

         switch( bType )
         {
            case ZH_ET_LONG:
            {
               ZH_MAXDBL dVal = ( ZH_MAXDBL ) pLeft->value.asNum.val.l + ( ZH_MAXDBL ) pRight->value.asNum.val.l;

               if( ZH_DBL_LIM_LONG( dVal ) )
               {
                  pSelf->value.asNum.val.l = pLeft->value.asNum.val.l + pRight->value.asNum.val.l;
                  pSelf->value.asNum.NumType = ZH_ET_LONG;
               }
               else
               {
                  pSelf->value.asNum.val.d = ( double ) dVal;
                  pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
               }
               pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
               pSelf->value.asNum.bDec = 0;
               break;
            }

            case ZH_ET_DOUBLE:
               pSelf->value.asNum.val.d = pLeft->value.asNum.val.d + pRight->value.asNum.val.d;
               pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
               if( pLeft->value.asNum.bDec < pRight->value.asNum.bDec )
                  pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
               else
                  pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
               pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
               break;

            default:
               if( pLeft->value.asNum.NumType == ZH_ET_DOUBLE )
               {
                  pSelf->value.asNum.val.d = pLeft->value.asNum.val.d + ( double ) pRight->value.asNum.val.l;
                  pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
               }
               else
               {
                  pSelf->value.asNum.val.d = ( double ) pLeft->value.asNum.val.l + pRight->value.asNum.val.d;
                  pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
               }
               pSelf->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
               pSelf->value.asNum.NumType = ZH_ET_DOUBLE;
         }
         pSelf->ExprType = ZH_ET_NUMERIC;
         pSelf->ValType  = ZH_EV_NUMERIC;
         ZH_COMP_EXPR_FREE( pLeft );
         ZH_COMP_EXPR_FREE( pRight );
      }
      else if( pRight->ExprType == ZH_ET_DATE )
      {
         if( pLeft->value.asNum.NumType == ZH_ET_LONG )
            pSelf->value.asDate.lDate = pRight->value.asDate.lDate + ( long ) pLeft->value.asNum.val.l;
         else
            pSelf->value.asDate.lDate = pRight->value.asDate.lDate + ZH_CAST_LONG( pLeft->value.asNum.val.d );
         pSelf->value.asDate.lTime = 0;
         pSelf->ExprType = ZH_ET_DATE;
         pSelf->ValType  = ZH_EV_DATE;
         ZH_COMP_EXPR_FREE( pLeft );
         ZH_COMP_EXPR_FREE( pRight );
      }
      else if( pRight->ExprType == ZH_ET_TIMESTAMP )
      {
         if( pLeft->value.asNum.NumType == ZH_ET_LONG )
            zh_compExprReduceTimeStampPut( pSelf, pRight->value.asDate.lDate + ( long ) pLeft->value.asNum.val.l,
                                           pRight->value.asDate.lTime );
         else
            zh_compExprReduceTimeStampAdd( pSelf, pRight, pLeft->value.asNum.val.d );
         ZH_COMP_EXPR_FREE( pLeft );
         ZH_COMP_EXPR_FREE( pRight );
      }
      else if( ZH_SUPPORT_EXTOPT &&
               ( pLeft->value.asNum.NumType == ZH_ET_LONG ?
                 pLeft->value.asNum.val.l == 0 :
                 pLeft->value.asNum.val.d == 0 ) )
      {
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         pSelf->ExprType = ZH_ET_NONE; /* suppress deletion of operator components */
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pRight;
         ZH_COMP_EXPR_FREE( pLeft );
      }
      else if( ZH_SUPPORT_EXTOPT && pRight->ExprType == ZH_EO_PLUS )
      {
         if( zh_compExprReducePlusNums( pRight, pLeft ) )
         {
            pSelf->ExprType = ZH_ET_NONE; /* suppress deletion of operator components */
            ZH_COMP_EXPR_FREE( pSelf );
            pSelf = pRight;
            ZH_COMP_EXPR_FREE( pLeft );
         }
      }
      else
      {
         /* TODO: Check for incompatible types e.g. "txt" + 3
          */
      }
   }
   else if( pRight->ExprType == ZH_ET_NUMERIC )
   {
      if( pLeft->ExprType == ZH_ET_DATE )
      {
         if( pRight->value.asNum.NumType == ZH_ET_LONG )
            pSelf->value.asDate.lDate = pLeft->value.asDate.lDate + ( long ) pRight->value.asNum.val.l;
         else
            pSelf->value.asDate.lDate = pLeft->value.asDate.lDate + ZH_CAST_LONG( pRight->value.asNum.val.d );
         pSelf->value.asDate.lTime = 0;
         pSelf->ExprType = ZH_ET_DATE;
         pSelf->ValType  = ZH_EV_DATE;
         ZH_COMP_EXPR_FREE( pLeft );
         ZH_COMP_EXPR_FREE( pRight );
      }
      else if( pLeft->ExprType == ZH_ET_TIMESTAMP )
      {
         if( pRight->value.asNum.NumType == ZH_ET_LONG )
            zh_compExprReduceTimeStampPut( pSelf, pLeft->value.asDate.lDate + ( long ) pRight->value.asNum.val.l,
                                           pLeft->value.asDate.lTime );
         else
            zh_compExprReduceTimeStampAdd( pSelf, pLeft, pRight->value.asNum.val.d );
         ZH_COMP_EXPR_FREE( pLeft );
         ZH_COMP_EXPR_FREE( pRight );
      }
      else if( ZH_SUPPORT_EXTOPT &&
               ( pRight->value.asNum.NumType == ZH_ET_LONG ?
                 pRight->value.asNum.val.l == 0 :
                 pRight->value.asNum.val.d == 0 ) )
      {
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         pSelf->ExprType = ZH_ET_NONE; /* suppress deletion of operator components */
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft;
         ZH_COMP_EXPR_FREE( pRight );
      }
      else if( ZH_SUPPORT_EXTOPT && pLeft->ExprType == ZH_EO_PLUS )
      {
         if( zh_compExprReducePlusNums( pLeft, pRight ) )
         {
            pSelf->ExprType = ZH_ET_NONE; /* suppress deletion of operator components */
            ZH_COMP_EXPR_FREE( pSelf );
            pSelf = pLeft;
            ZH_COMP_EXPR_FREE( pRight );
         }
      }
      else
      {
         /* TODO: Check for incompatible types e.g. "txt" + 3
          */
      }
   }
   else if( ( pLeft->ExprType == ZH_ET_DATE || pLeft->ExprType == ZH_ET_TIMESTAMP ) &&
            ( pRight->ExprType == ZH_ET_DATE || pRight->ExprType == ZH_ET_TIMESTAMP ) )
   {
      if( pLeft->ExprType == ZH_ET_TIMESTAMP || pRight->ExprType == ZH_ET_TIMESTAMP )
      {
         zh_compExprReduceTimeStampPut( pSelf,
                           pLeft->value.asDate.lDate + pRight->value.asDate.lDate,
                           pLeft->value.asDate.lTime + pRight->value.asDate.lTime );
      }
      else
      {
         /* NOTE: This is not a bug. CA-Cl*pper does exactly that for DATEs. */
         pSelf->value.asDate.lDate = pLeft->value.asDate.lDate + pRight->value.asDate.lDate;
         pSelf->value.asDate.lTime = 0;
         pSelf->ExprType = ZH_ET_DATE;
         pSelf->ValType  = ZH_EV_DATE;
      }
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   else if( pLeft->ExprType == ZH_ET_STRING && pRight->ExprType == ZH_ET_STRING )
   {
      if( pRight->nLength == 0 )
      {
         pSelf->ExprType = ZH_ET_NONE; /* suppress deletion of operator components */
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft;
         ZH_COMP_EXPR_FREE( pRight );
      }
      else if( pLeft->nLength == 0 )
      {
         pSelf->ExprType = ZH_ET_NONE; /* suppress deletion of operator components */
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pRight;
         ZH_COMP_EXPR_FREE( pLeft );
      }
      else
      {
         ZH_BOOL fReduce = ZH_TRUE;

         /* Do not reduce strings with the macro operator '&'
          */
         if( ZH_SUPPORT_MACROTEXT )
         {
            char * szText = pLeft->value.asString.string;
            ZH_SIZE nLen = pLeft->nLength;

            while( nLen-- )
            {
               if( *szText++ == '&' )
               {
                  char ch = nLen ? *szText : *pRight->value.asString.string;
                  if( ( ch >= 'A' && ch <= 'Z' ) ||
                      ( ch >= 'a' && ch <= 'z' ) || ch == '_' ||
                      ! ZH_SUPPORT_ZIHER )
                  {
                     fReduce = ZH_FALSE;
                     break;
                  }
               }
            }
         }
         if( fReduce )
         {
            pSelf->ExprType = ZH_ET_NONE; /* suppress deletion of operator components */
            ZH_COMP_EXPR_FREE( pSelf );
            pSelf = zh_compExprReducePlusStrings( pLeft, pRight, ZH_COMP_PARAM );
         }
      }
   }
   else
   {
      /* TODO: Check for incompatible types e.g. "txt" + 3
       */
   }
   return pSelf;
}

PZH_EXPR zh_compExprReduceNegate( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   pExpr = pSelf->value.asOperator.pLeft;

   if( pExpr->ExprType == ZH_ET_NUMERIC )
   {
      if( pExpr->value.asNum.NumType == ZH_ET_DOUBLE )
      {
         pExpr->value.asNum.val.d = - pExpr->value.asNum.val.d;
         pExpr->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
      }
      else
      {
#if -ZH_VMLONG_MAX > ZH_VMLONG_MIN
         if( pExpr->value.asNum.val.l < -ZH_VMLONG_MAX )
         {
            pExpr->value.asNum.NumType = ZH_ET_DOUBLE;
            pExpr->value.asNum.val.d = - ( double ) pExpr->value.asNum.val.l;
            pExpr->value.asNum.bDec = 0;
         }
         else
#endif
         {
            pExpr->value.asNum.val.l = - pExpr->value.asNum.val.l;
         }
         pExpr->value.asNum.bWidth = ZH_DEFAULT_WIDTH;
      }
      pSelf->ExprType = ZH_ET_NONE; /* suppress deletion of operator components */
      ZH_COMP_EXPR_FREE( pSelf );
      pSelf = pExpr;
   }
   else if( pExpr->ExprType == ZH_EO_NEGATE && ZH_SUPPORT_EXTOPT )
   {
      /* NOTE: This will not generate a runtime error if incompatible
       * data type is used
       */
      pExpr->ExprType = ZH_ET_NONE; /* suppress deletion of operator components */
      pExpr = pExpr->value.asOperator.pLeft;
      ZH_COMP_EXPR_FREE( pSelf );
      pSelf = pExpr;
   }

   return pSelf;
}


PZH_EXPR zh_compExprReduceIN( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == pRight->ExprType && pLeft->ExprType == ZH_ET_STRING )
   {
      /* Both arguments are literal strings
       */

      /* NOTE: If macro substitution is not disabled (-kM compiler
       *       switch) then we cannot reduce also strings which
       *       have macro operator '&'
       */
      if( ! ZH_SUPPORT_MACROTEXT ||
          ( ! zh_compExprHasMacro( pLeft->value.asString.string,
                                   pLeft->nLength, ZH_COMP_PARAM ) &&
            ! zh_compExprHasMacro( pRight->value.asString.string,
                                   pRight->nLength, ZH_COMP_PARAM ) ) )
      {
         ZH_BOOL bResult;

         /* NOTE: CA-Cl*pper has a bug where the $ operator returns .T.
          *       when an empty string is searched [vszakats]
          *
          *       But this bug exist only in compiler and CA-Cl*pper macro
          *       compiler does not have optimizer. This bug is replicated
          *       by us only when Ziher extensions in compiler (-kh) are
          *       not enabled e.g. in strict Clipper compatible mode (-kc)
          *       [druzus]
          */
         if( pLeft->nLength == 0 )
            bResult = ZH_COMP_PARAM->mode == ZH_MODE_COMPILER &&
                      ! ZH_SUPPORT_ZIHER;
         else
            bResult = ( zh_strAt( pLeft->value.asString.string, pLeft->nLength,
                                  pRight->value.asString.string, pRight->nLength ) != 0 );

         ZH_COMP_EXPR_FREE( pLeft );
         ZH_COMP_EXPR_FREE( pRight );
         pSelf->ExprType = ZH_ET_LOGICAL;
         pSelf->ValType  = ZH_EV_LOGICAL;
         pSelf->value.asLogical = bResult;
      }
   }
   /* TODO: add checking for incompatible types
    */
   return pSelf;
}

PZH_EXPR zh_compExprReduceNE( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == pRight->ExprType )
   {
      switch( pLeft->ExprType )
      {
         case ZH_ET_LOGICAL:
         {
            /* .F. != .T.  = .T.
             * .T. != .T.  = .F.
             * .F. != .F.  = .F.
             * .T. != .F.  = .T.
             */
            ZH_BOOL bResult = ( pLeft->value.asLogical != pRight->value.asLogical );
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

         case ZH_ET_STRING:
            /* NOTE: the result depends on SET EXACT setting then it
             * cannot be optimized except the case when null strings are
             * compared - "" != "" is always ZH_FALSE regardless of EXACT
             * setting
             */
            if( ( pLeft->nLength | pRight->nLength ) == 0 )
            {
               ZH_COMP_EXPR_FREE( pLeft );
               ZH_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = ZH_ET_LOGICAL;
               pSelf->ValType  = ZH_EV_LOGICAL;
               pSelf->value.asLogical = ZH_FALSE;

               /* NOTE: COMPATIBILITY: Clipper doesn't optimize this */
            }
            break;

         case ZH_ET_NUMERIC:
         {
            ZH_BOOL bResult;

            switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
            {
               case ZH_ET_LONG:
                  bResult = ( pLeft->value.asNum.val.l != pRight->value.asNum.val.l );
                  break;
               case ZH_ET_DOUBLE:
                  bResult = ( pLeft->value.asNum.val.d != pRight->value.asNum.val.d );
                  break;
               default:
                  if( pLeft->value.asNum.NumType == ZH_ET_LONG )
                     bResult = ( pLeft->value.asNum.val.l != pRight->value.asNum.val.d );
                  else
                     bResult = ( pLeft->value.asNum.val.d != pRight->value.asNum.val.l );
                  break;
            }
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

         case ZH_ET_DATE:
         case ZH_ET_TIMESTAMP:
         {
            ZH_BOOL bResult = pLeft->value.asDate.lDate != pRight->value.asDate.lDate ||
                              pLeft->value.asDate.lTime != pRight->value.asDate.lTime;
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

         case ZH_ET_NIL:
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = ZH_FALSE;
            break;
      }
   }
   else if( ( pLeft->ExprType == ZH_ET_TIMESTAMP &&
              pRight->ExprType == ZH_ET_DATE ) ||
            ( pLeft->ExprType == ZH_ET_DATE &&
              pRight->ExprType == ZH_ET_TIMESTAMP ) )
   {
      pSelf->value.asLogical = pLeft->value.asDate.lDate != pRight->value.asDate.lDate;
      pSelf->ExprType        = ZH_ET_LOGICAL;
      pSelf->ValType         = ZH_EV_LOGICAL;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   else if( ZH_SUPPORT_EXTOPT &&
            ( pLeft->ExprType == ZH_ET_LOGICAL ||
              pRight->ExprType == ZH_ET_LOGICAL ) )
   {
      /* NOTE: This will not generate a runtime error if incompatible
       * data type is used
       */

      if( pLeft->ExprType == ZH_ET_LOGICAL )
      {
         pSelf->value.asOperator.pLeft = pRight;
         pRight = pLeft;
         pLeft  = pSelf->value.asOperator.pRight;
      }

      if( ! pRight->value.asLogical )
      {
         pSelf->ExprType = ZH_ET_NONE;
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft;
      }
      else if( pLeft->ExprType == ZH_EO_NOT )
      {
         pSelf->ExprType = ZH_ET_NONE;
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft->value.asOperator.pLeft;
         pLeft->ExprType = ZH_ET_NONE;
         ZH_COMP_EXPR_FREE( pLeft );
      }
      else
      {
         pSelf->ExprType = ZH_EO_NOT;
         pSelf->value.asOperator.pRight = NULL;
      }
      ZH_COMP_EXPR_FREE( pRight );
   }
   else if( ( pLeft->ExprType == ZH_ET_NIL &&
              ( pRight->ExprType == ZH_ET_NUMERIC ||
                pRight->ExprType == ZH_ET_LOGICAL ||
                pRight->ExprType == ZH_ET_DATE ||
                pRight->ExprType == ZH_ET_TIMESTAMP ||
                pRight->ExprType == ZH_ET_STRING ||
                pRight->ExprType == ZH_ET_CODEBLOCK ||
                pRight->ExprType == ZH_ET_ARRAY ||
                pRight->ExprType == ZH_ET_HASH ||
                pRight->ExprType == ZH_ET_FUNREF ) ) ||
            ( pRight->ExprType == ZH_ET_NIL &&
              ( pLeft->ExprType == ZH_ET_NUMERIC ||
                pLeft->ExprType == ZH_ET_LOGICAL ||
                pLeft->ExprType == ZH_ET_DATE ||
                pLeft->ExprType == ZH_ET_TIMESTAMP ||
                pLeft->ExprType == ZH_ET_STRING ||
                pLeft->ExprType == ZH_ET_CODEBLOCK ||
                pLeft->ExprType == ZH_ET_ARRAY ||
                pLeft->ExprType == ZH_ET_HASH ||
                pLeft->ExprType == ZH_ET_FUNREF ) ) )
   {
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
      pSelf->ExprType        = ZH_ET_LOGICAL;
      pSelf->ValType         = ZH_EV_LOGICAL;
      pSelf->value.asLogical = ZH_TRUE;
   }
   /* TODO: add checking of incompatible types
      else
      {
      }
    */
   return pSelf;
}

PZH_EXPR zh_compExprReduceGE( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == pRight->ExprType )
      switch( pLeft->ExprType )
      {
         case ZH_ET_LOGICAL:
         {
            /* .T. >= .F.  = .T.
             * .T. >= .T.  = .T.
             * .F. >= .F.  = .T.
             * .F. >= .T.  = .f.
             */
            ZH_BOOL bResult = ! ( ! pLeft->value.asLogical && pRight->value.asLogical );
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

         case ZH_ET_NUMERIC:
         {
            ZH_BOOL bResult;

            switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
            {
               case ZH_ET_LONG:
                  bResult = ( pLeft->value.asNum.val.l >= pRight->value.asNum.val.l );
                  break;
               case ZH_ET_DOUBLE:
                  bResult = ( pLeft->value.asNum.val.d >= pRight->value.asNum.val.d );
                  break;
               default:
                  if( pLeft->value.asNum.NumType == ZH_ET_LONG )
                     bResult = ( pLeft->value.asNum.val.l >= pRight->value.asNum.val.d );
                  else
                     bResult = ( pLeft->value.asNum.val.d >= pRight->value.asNum.val.l );
                  break;
            }
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

         case ZH_ET_DATE:
         case ZH_ET_TIMESTAMP:
         {
            ZH_BOOL bResult = ( pLeft->value.asDate.lDate > pRight->value.asDate.lDate ) ||
                              ( pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                                pLeft->value.asDate.lTime >= pRight->value.asDate.lTime );
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

      }
   else if( ( pLeft->ExprType == ZH_ET_TIMESTAMP &&
              pRight->ExprType == ZH_ET_DATE ) ||
            ( pLeft->ExprType == ZH_ET_DATE &&
              pRight->ExprType == ZH_ET_TIMESTAMP ) )
   {
      pSelf->value.asLogical = pLeft->value.asDate.lDate >= pRight->value.asDate.lDate;
      pSelf->ExprType        = ZH_ET_LOGICAL;
      pSelf->ValType         = ZH_EV_LOGICAL;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   /* TODO: add checking of incompatible types
      else
      {
      }
    */
   return pSelf;
}

PZH_EXPR zh_compExprReduceLE( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == pRight->ExprType )
      switch( pLeft->ExprType )
      {
         case ZH_ET_LOGICAL:
         {
            /* .T. <= .F.  = .F.
             * .T. <= .T.  = .T.
             * .F. <= .F.  = .T.
             * .F. <= .T.  = .T.
             */
            ZH_BOOL bResult = ! ( pLeft->value.asLogical && ! pRight->value.asLogical );
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

         case ZH_ET_NUMERIC:
         {
            ZH_BOOL bResult;

            switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
            {
               case ZH_ET_LONG:
                  bResult = ( pLeft->value.asNum.val.l <= pRight->value.asNum.val.l );
                  break;
               case ZH_ET_DOUBLE:
                  bResult = ( pLeft->value.asNum.val.d <= pRight->value.asNum.val.d );
                  break;
               default:
                  if( pLeft->value.asNum.NumType == ZH_ET_LONG )
                     bResult = ( pLeft->value.asNum.val.l <= pRight->value.asNum.val.d );
                  else
                     bResult = ( pLeft->value.asNum.val.d <= pRight->value.asNum.val.l );
                  break;
            }
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

         case ZH_ET_DATE:
         case ZH_ET_TIMESTAMP:
         {
            ZH_BOOL bResult = ( pLeft->value.asDate.lDate < pRight->value.asDate.lDate ) ||
                              ( pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                                pLeft->value.asDate.lTime <= pRight->value.asDate.lTime );
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

      }
   else if( ( pLeft->ExprType == ZH_ET_TIMESTAMP &&
              pRight->ExprType == ZH_ET_DATE ) ||
            ( pLeft->ExprType == ZH_ET_DATE &&
              pRight->ExprType == ZH_ET_TIMESTAMP ) )
   {
      pSelf->value.asLogical = pLeft->value.asDate.lDate <= pRight->value.asDate.lDate;
      pSelf->ExprType        = ZH_ET_LOGICAL;
      pSelf->ValType         = ZH_EV_LOGICAL;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   /* TODO: add checking of incompatible types
      else
      {
      }
    */
   return pSelf;
}

PZH_EXPR zh_compExprReduceGT( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == pRight->ExprType )
      switch( pLeft->ExprType )
      {
         case ZH_ET_LOGICAL:
         {
            /* .T. > .F.  = .T.
             * .T. > .T.  = .F.
             * .F. > .F.  = .F.
             * .F. > .T.  = .F.
             */
            ZH_BOOL bResult = ( pLeft->value.asLogical && ! pRight->value.asLogical );
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

         case ZH_ET_NUMERIC:
         {
            ZH_BOOL bResult;

            switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
            {
               case ZH_ET_LONG:
                  bResult = ( pLeft->value.asNum.val.l > pRight->value.asNum.val.l );
                  break;
               case ZH_ET_DOUBLE:
                  bResult = ( pLeft->value.asNum.val.d > pRight->value.asNum.val.d );
                  break;
               default:
                  if( pLeft->value.asNum.NumType == ZH_ET_LONG )
                     bResult = ( pLeft->value.asNum.val.l > pRight->value.asNum.val.d );
                  else
                     bResult = ( pLeft->value.asNum.val.d > pRight->value.asNum.val.l );
                  break;
            }
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

         case ZH_ET_DATE:
         case ZH_ET_TIMESTAMP:
         {
            ZH_BOOL bResult = ( pLeft->value.asDate.lDate > pRight->value.asDate.lDate ) ||
                              ( pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                                pLeft->value.asDate.lTime > pRight->value.asDate.lTime );
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

      }
   else if( ( pLeft->ExprType == ZH_ET_TIMESTAMP &&
              pRight->ExprType == ZH_ET_DATE ) ||
            ( pLeft->ExprType == ZH_ET_DATE &&
              pRight->ExprType == ZH_ET_TIMESTAMP ) )
   {
      pSelf->value.asLogical = pLeft->value.asDate.lDate > pRight->value.asDate.lDate;
      pSelf->ExprType        = ZH_ET_LOGICAL;
      pSelf->ValType         = ZH_EV_LOGICAL;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   /* TODO: add checking of incompatible types
      else
      {
      }
    */
   return pSelf;
}

PZH_EXPR zh_compExprReduceLT( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == pRight->ExprType )
      switch( pLeft->ExprType )
      {
         case ZH_ET_LOGICAL:
         {
            /* .F. < .T.  = .T.
             * .T. < .T.  = .F.
             * .F. < .F.  = .F.
             * .T. < .F.  = .F.
             */
            ZH_BOOL bResult = ( ! pLeft->value.asLogical && pRight->value.asLogical );
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

         case ZH_ET_NUMERIC:
         {
            ZH_BOOL bResult;

            switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
            {
               case ZH_ET_LONG:
                  bResult = ( pLeft->value.asNum.val.l < pRight->value.asNum.val.l );
                  break;
               case ZH_ET_DOUBLE:
                  bResult = ( pLeft->value.asNum.val.d < pRight->value.asNum.val.d );
                  break;
               default:
                  if( pLeft->value.asNum.NumType == ZH_ET_LONG )
                     bResult = ( pLeft->value.asNum.val.l < pRight->value.asNum.val.d );
                  else
                     bResult = ( pLeft->value.asNum.val.d < pRight->value.asNum.val.l );
                  break;
            }
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

         case ZH_ET_DATE:
         case ZH_ET_TIMESTAMP:
         {
            ZH_BOOL bResult = ( pLeft->value.asDate.lDate < pRight->value.asDate.lDate ) ||
                              ( pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                                pLeft->value.asDate.lTime < pRight->value.asDate.lTime );
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
         }
         break;

      }
   else if( ( pLeft->ExprType == ZH_ET_TIMESTAMP &&
              pRight->ExprType == ZH_ET_DATE ) ||
            ( pLeft->ExprType == ZH_ET_DATE &&
              pRight->ExprType == ZH_ET_TIMESTAMP ) )
   {
      pSelf->value.asLogical = pLeft->value.asDate.lDate < pRight->value.asDate.lDate;
      pSelf->ExprType        = ZH_ET_LOGICAL;
      pSelf->ValType         = ZH_EV_LOGICAL;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   /* TODO: add checking of incompatible types
      else
      {
      }
    */
   return pSelf;
}

PZH_EXPR zh_compExprReduceEQ( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == pRight->ExprType )
   {
      switch( pLeft->ExprType )
      {
         case ZH_ET_LOGICAL:
         {
            ZH_BOOL bResult = ( pLeft->value.asLogical == pRight->value.asLogical );
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
            break;
         }

         case ZH_ET_STRING:
            /* NOTE: when not exact comparison (==) is used
             * the result depends on SET EXACT setting then it
             * cannot be optimized except the case when null strings are
             * compared - "" = "" is always TRUE regardless of EXACT
             * setting.
             * If macro substitution is not disabled (-kM compiler
             * switch) then we cannot reduce also strings which
             * have macro operator '&'
             */
            if( ( pLeft->nLength | pRight->nLength ) == 0 ||
                ( pSelf->ExprType == ZH_EO_EQ &&
                  ( ! ZH_SUPPORT_MACROTEXT ||
                    ( ! zh_compExprHasMacro( pLeft->value.asString.string,
                                             pLeft->nLength, ZH_COMP_PARAM ) &&
                      ! zh_compExprHasMacro( pRight->value.asString.string,
                                             pRight->nLength, ZH_COMP_PARAM ) ) ) ) )
            {
               ZH_BOOL bResult = pLeft->nLength == pRight->nLength &&
                                 memcmp( pLeft->value.asString.string,
                                         pRight->value.asString.string,
                                         pLeft->nLength ) == 0;
               ZH_COMP_EXPR_FREE( pLeft );
               ZH_COMP_EXPR_FREE( pRight );
               pSelf->ExprType        = ZH_ET_LOGICAL;
               pSelf->ValType         = ZH_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case ZH_ET_NUMERIC:
         {
            ZH_BOOL bResult;

            switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
            {
               case ZH_ET_LONG:
                  bResult = ( pLeft->value.asNum.val.l == pRight->value.asNum.val.l );
                  break;
               case ZH_ET_DOUBLE:
                  bResult = ( pLeft->value.asNum.val.d == pRight->value.asNum.val.d );
                  break;
               default:
                  if( pLeft->value.asNum.NumType == ZH_ET_LONG )
                     bResult = ( pLeft->value.asNum.val.l == pRight->value.asNum.val.d );
                  else
                     bResult = ( pLeft->value.asNum.val.d == pRight->value.asNum.val.l );
                  break;
            }
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
            break;
         }

         case ZH_ET_DATE:
         case ZH_ET_TIMESTAMP:
         {
            ZH_BOOL bResult = ( pLeft->value.asDate.lDate == pRight->value.asDate.lDate ) &&
                              ( pLeft->value.asDate.lTime == pRight->value.asDate.lTime );
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
            break;
         }

         case ZH_ET_NIL:
            ZH_COMP_EXPR_FREE( pLeft );
            ZH_COMP_EXPR_FREE( pRight );
            pSelf->ExprType        = ZH_ET_LOGICAL;
            pSelf->ValType         = ZH_EV_LOGICAL;
            pSelf->value.asLogical = ZH_TRUE;
            break;
      }
   }
   else if( ( pLeft->ExprType == ZH_ET_TIMESTAMP &&
              pRight->ExprType == ZH_ET_DATE ) ||
            ( pLeft->ExprType == ZH_ET_DATE &&
              pRight->ExprType == ZH_ET_TIMESTAMP ) )
   {
      pSelf->value.asLogical = pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                               ( pLeft->value.asDate.lTime == pRight->value.asDate.lTime ||
                                 pSelf->ExprType != ZH_EO_EQ );
      pSelf->ExprType = ZH_ET_LOGICAL;
      pSelf->ValType  = ZH_EV_LOGICAL;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
   }
   else if( ZH_SUPPORT_EXTOPT &&
            ( pLeft->ExprType == ZH_ET_LOGICAL ||
              pRight->ExprType == ZH_ET_LOGICAL ) )
   {
      /* NOTE: This will not generate a runtime error if incompatible
       * data type is used
       */

      if( pLeft->ExprType == ZH_ET_LOGICAL )
      {
         pSelf->value.asOperator.pLeft = pRight;
         pRight = pLeft;
         pLeft  = pSelf->value.asOperator.pRight;
      }

      if( pRight->value.asLogical )
      {
         pSelf->ExprType = ZH_ET_NONE;
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft;
      }
      else if( pLeft->ExprType == ZH_EO_NOT )
      {
         pSelf->ExprType = ZH_ET_NONE;
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft->value.asOperator.pLeft;
         pLeft->ExprType = ZH_ET_NONE;
         ZH_COMP_EXPR_FREE( pLeft );
      }
      else
      {
         pSelf->ExprType = ZH_EO_NOT;
         pSelf->value.asOperator.pRight = NULL;
      }
      ZH_COMP_EXPR_FREE( pRight );
   }
   else if( ( pLeft->ExprType == ZH_ET_NIL &&
              ( pRight->ExprType == ZH_ET_NUMERIC ||
                pRight->ExprType == ZH_ET_LOGICAL ||
                pRight->ExprType == ZH_ET_DATE ||
                pRight->ExprType == ZH_ET_TIMESTAMP ||
                pRight->ExprType == ZH_ET_STRING ||
                pRight->ExprType == ZH_ET_CODEBLOCK ||
                pRight->ExprType == ZH_ET_ARRAY ||
                pRight->ExprType == ZH_ET_HASH ||
                pRight->ExprType == ZH_ET_FUNREF ) ) ||
            ( pRight->ExprType == ZH_ET_NIL &&
              ( pLeft->ExprType == ZH_ET_NUMERIC ||
                pLeft->ExprType == ZH_ET_LOGICAL ||
                pLeft->ExprType == ZH_ET_DATE ||
                pLeft->ExprType == ZH_ET_TIMESTAMP ||
                pLeft->ExprType == ZH_ET_STRING ||
                pLeft->ExprType == ZH_ET_CODEBLOCK ||
                pLeft->ExprType == ZH_ET_ARRAY ||
                pLeft->ExprType == ZH_ET_HASH ||
                pLeft->ExprType == ZH_ET_FUNREF ) ) )
   {
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
      pSelf->ExprType        = ZH_ET_LOGICAL;
      pSelf->ValType         = ZH_EV_LOGICAL;
      pSelf->value.asLogical = ZH_FALSE;
   }
   /* TODO: add checking of incompatible types
      else
      {
      }
    */
   return pSelf;
}

PZH_EXPR zh_compExprReduceAnd( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == ZH_ET_LOGICAL && pRight->ExprType == ZH_ET_LOGICAL )
   {
      ZH_BOOL bResult;

      bResult = pLeft->value.asLogical && pRight->value.asLogical;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
      pSelf->ExprType        = ZH_ET_LOGICAL;
      pSelf->ValType         = ZH_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
   }
   else if( pLeft->ExprType == ZH_ET_LOGICAL &&
            ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_SHORTCUTS ) )
   {
      if( pLeft->value.asLogical )
      {
         /* .T. .AND. expr => expr
          */
         ZH_COMP_EXPR_FREE( pLeft );
         pSelf->ExprType = ZH_ET_NONE;    /* don't delete expression components */
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pRight;
      }
      else
      {
         /* .F. .AND. expr => .F.
          */
         ZH_COMP_EXPR_FREE( pLeft );
         ZH_COMP_EXPR_FREE( pRight );         /* discard expression */
         pSelf->ExprType        = ZH_ET_LOGICAL;
         pSelf->ValType         = ZH_EV_LOGICAL;
         pSelf->value.asLogical = ZH_FALSE;
      }
   }
   else if( pRight->ExprType == ZH_ET_LOGICAL &&
            ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_SHORTCUTS ) &&
            ( ZH_COMP_PARAM->mode == ZH_MODE_COMPILER || ZH_SUPPORT_ZIHER ) )
   {
      if( pRight->value.asLogical )
      {
         /* expr .AND. .T. => expr
          */
         ZH_COMP_EXPR_FREE( pRight );
         pSelf->ExprType = ZH_ET_NONE;    /* don't delete expression components */
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft;
      }
      else
      {
         /* expr .AND. .F. => .F.
          */
         ZH_COMP_EXPR_FREE( pLeft );      /* discard expression */
         ZH_COMP_EXPR_FREE( pRight );
         pSelf->ExprType        = ZH_ET_LOGICAL;
         pSelf->ValType         = ZH_EV_LOGICAL;
         pSelf->value.asLogical = ZH_FALSE;
      }
   }
   return pSelf;
}

PZH_EXPR zh_compExprReduceOr( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == ZH_ET_LOGICAL && pRight->ExprType == ZH_ET_LOGICAL )
   {
      ZH_BOOL bResult;

      bResult = pLeft->value.asLogical || pRight->value.asLogical;
      ZH_COMP_EXPR_FREE( pLeft );
      ZH_COMP_EXPR_FREE( pRight );
      pSelf->ExprType        = ZH_ET_LOGICAL;
      pSelf->ValType         = ZH_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
   }
   else if( pLeft->ExprType == ZH_ET_LOGICAL &&
            ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_SHORTCUTS ) )
   {
      if( pLeft->value.asLogical )
      {
         /* .T. .OR. expr => .T.
          */
         ZH_COMP_EXPR_FREE( pLeft );
         ZH_COMP_EXPR_FREE( pRight );     /* discard expression */
         pSelf->ExprType        = ZH_ET_LOGICAL;
         pSelf->ValType         = ZH_EV_LOGICAL;
         pSelf->value.asLogical = ZH_TRUE;
      }
      else
      {
         /* .F. .OR. expr => expr
          */
         ZH_COMP_EXPR_FREE( pLeft );
         pSelf->ExprType = ZH_ET_NONE;    /* don't delete expression components */
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pRight;
      }
   }
   else if( pRight->ExprType == ZH_ET_LOGICAL &&
            ZH_COMP_ISSUPPORTED( ZH_COMPFLAG_SHORTCUTS ) &&
            ( ZH_COMP_PARAM->mode == ZH_MODE_COMPILER || ZH_SUPPORT_ZIHER ) )
   {
      if( pRight->value.asLogical )
      {
         /* expr .OR. .T. => .T.
          */
         ZH_COMP_EXPR_FREE( pLeft );      /* discard expression */
         ZH_COMP_EXPR_FREE( pRight );
         pSelf->ExprType        = ZH_ET_LOGICAL;
         pSelf->ValType         = ZH_EV_LOGICAL;
         pSelf->value.asLogical = ZH_TRUE;
      }
      else
      {
         /* expr .OR. .F. => expr
          */
         ZH_COMP_EXPR_FREE( pRight );
         pSelf->ExprType = ZH_ET_NONE;    /* don't delete expression components */
         ZH_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft;
      }
   }
   return pSelf;
}

PZH_EXPR zh_compExprReduceIIF( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pExpr;

   /* get conditional expression */
   pExpr = pSelf->value.asList.pExprList;

   if( pExpr->ExprType == ZH_ET_LOGICAL )
   {
      /* the condition was reduced to a logical value: .T. or .F.
       */
      if( pExpr->value.asLogical )
      {
         /* .T. was specified
          */
         pExpr = pExpr->pNext;   /* skip to TRUE expression */
         /* delete condition  - it is no longer needed
          */
         ZH_COMP_EXPR_FREE( pSelf->value.asList.pExprList );
         /* assign NULL to a start of expressions list to suppress
          * deletion of expression's components - we are deleting them
          * here
          */
         pSelf->value.asList.pExprList = NULL;
         ZH_COMP_EXPR_FREE( pSelf );
         /* store the TRUE expression as a result of reduction
          */
         pSelf = pExpr;
         pExpr = pExpr->pNext;       /* skip to FALSE expression */
         ZH_COMP_EXPR_FREE( pExpr ); /* delete FALSE expression */
         pSelf->pNext = NULL;
      }
      else
      {
         /* .F. was specified
          */
         pExpr = pExpr->pNext;   /* skip to TRUE expression */
         /* delete condition  - it is no longer needed
          */
         ZH_COMP_EXPR_FREE( pSelf->value.asList.pExprList );
         /* assign NULL to a start of expressions list to suppress
          * deletion of expression's components - we are deleting them
          * here
          */
         pSelf->value.asList.pExprList = NULL;
         ZH_COMP_EXPR_FREE( pSelf );
         /* store the FALSE expression as a result of reduction
          */
         pSelf = pExpr->pNext;
         ZH_COMP_EXPR_FREE( pExpr );      /* delete TRUE expression */
         pSelf->pNext = NULL;
      }

      /* this will cause warning when IIF is used as statement */
      /*
      if( pSelf->ExprType == ZH_ET_NONE )
      {
         pSelf->ExprType = ZH_ET_NIL;
         pSelf->ValType = ZH_EV_NIL;
      }
      */
   }
   /* check if valid expression is passed
    */
   else if( pExpr->ExprType == ZH_ET_NIL ||
            pExpr->ExprType == ZH_ET_NUMERIC ||
            pExpr->ExprType == ZH_ET_DATE ||
            pExpr->ExprType == ZH_ET_TIMESTAMP ||
            pExpr->ExprType == ZH_ET_STRING ||
            pExpr->ExprType == ZH_ET_CODEBLOCK ||
            pExpr->ExprType == ZH_ET_ARRAY ||
            pExpr->ExprType == ZH_ET_HASH ||
            pExpr->ExprType == ZH_ET_VARREF ||
            pExpr->ExprType == ZH_ET_REFERENCE ||
            pExpr->ExprType == ZH_ET_FUNREF )
   {
      ZH_COMP_ERROR_TYPE( pExpr );
   }
   return pSelf;
}

/* replace the list containing a single expression with a simple expression
 * - strips parenthesis
 *  ( EXPR ) -> EXPR
 */
PZH_EXPR zh_compExprListStrip( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   while( pSelf->ExprType == ZH_ET_LIST &&
          zh_compExprListLen( pSelf ) == 1 &&
          pSelf->value.asList.pExprList->ExprType <= ZH_ET_VARIABLE &&
          ! zh_compExprIsArrayToParams( pSelf->value.asList.pExprList ) )
   {
      /* replace the list with a simple expression
       *  ( EXPR ) -> EXPR
       */
      PZH_EXPR pExpr = pSelf;

      pSelf = pSelf->value.asList.pExprList;
      pExpr->value.asList.pExprList = NULL;
      ZH_COMP_EXPR_FREE( pExpr );
   }

   return pSelf;
}

ZH_BOOL zh_compExprReduceAT( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pSub   = pParms->value.asList.pExprList;
   PZH_EXPR pText  = pSub->pNext;

   if( pSub->ExprType == ZH_ET_STRING && pText->ExprType == ZH_ET_STRING &&
       ! ZH_SUPPORT_USERCP )
   {
      PZH_EXPR pReduced;

      /* NOTE: CA-Cl*pper has a bug in At( "", cText ) compile time
       *       optimization and always set 1 as result in such cases.
       *       This bug exist only in compiler and CA-Cl*pper macro
       *       compiler does not have optimizer. This bug is replicated
       *       by us only when Ziher extensions in compiler (-kh) are
       *       not enabled e.g. in strict Clipper compatible mode (-kc)
       *       [druzus]
       */
      if( pSub->nLength == 0 )
      {
         pReduced = zh_compExprNewLong( ( ZH_COMP_PARAM->mode == ZH_MODE_COMPILER &&
                                          ! ZH_SUPPORT_ZIHER ) ? 1 : 0, ZH_COMP_PARAM );
      }
      else
      {
         pReduced = zh_compExprNewLong( zh_strAt( pSub->value.asString.string,
                               pSub->nLength, pText->value.asString.string,
                               pText->nLength ), ZH_COMP_PARAM );
      }

      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pParms );

      memcpy( pSelf, pReduced, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pReduced );
      return ZH_TRUE;
   }
   else
      return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceCHR( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   ZH_BOOL fDoOpt = ZH_FALSE;
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == ZH_ET_NUMERIC )
   {
      if( ZH_SUPPORT_USERCP )
      {
         int iVal = pArg->value.asNum.NumType == ZH_ET_LONG ?
                    ( int ) pArg->value.asNum.val.l :
                    ( int ) pArg->value.asNum.val.d;
         fDoOpt = iVal >= 0 && iVal <= 127;
      }
      else
         fDoOpt = ZH_TRUE;
   }

   /* try to change it into a string */
   if( fDoOpt )
   {
      /* NOTE: CA-Cl*pper's compiler optimizer will be wrong for those
       *       Chr() cases where the passed parameter is a constant which
       *       can be divided by 256 but it's not zero, in this case it
       *       will return an empty string instead of a Chr( 0 ). [vszakats]
       *
       *       But this bug exist only in compiler and CA-Cl*pper macro
       *       compiler does not have optimizer. This bug is replicated
       *       by us only when Ziher extensions in compiler (-kh) are
       *       not enabled e.g. in strict Clipper compatible mode (-kc)
       *       [druzus]
       */

      PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_ET_STRING );

      pExpr->ValType = ZH_EV_STRING;
      if( pArg->value.asNum.NumType == ZH_ET_LONG )
      {
         if( ZH_COMP_PARAM->mode == ZH_MODE_COMPILER &&
             ! ZH_SUPPORT_ZIHER &&
             ( pArg->value.asNum.val.l & 0xff ) == 0 &&
               pArg->value.asNum.val.l != 0 )
         {
            pExpr->value.asString.string = ( char * ) "";
            pExpr->value.asString.dealloc = ZH_FALSE;
            pExpr->nLength = 0;
         }
         else
         {
            pExpr->value.asString.string = ( char * ) ZH_UNCONST( zh_szAscii[ ( int ) pArg->value.asNum.val.l & 0xff ] );
            pExpr->value.asString.dealloc = ZH_FALSE;
            pExpr->nLength = 1;
         }
      }
      else
      {
         pExpr->value.asString.string = ( char * ) ZH_UNCONST( zh_szAscii[ ZH_CAST_INT( pArg->value.asNum.val.d ) & 0xff ] );
         pExpr->value.asString.dealloc = ZH_FALSE;
         pExpr->nLength = 1;
      }

      ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceBCHAR( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == ZH_ET_NUMERIC )
   {
      PZH_EXPR pExpr = ZH_COMP_EXPR_NEW( ZH_ET_STRING );

      pExpr->ValType = ZH_EV_STRING;
      pExpr->value.asString.string =
         ( char * ) ZH_UNCONST( zh_szAscii[ ( pArg->value.asNum.NumType == ZH_ET_LONG ?
                                ( int ) pArg->value.asNum.val.l :
                                ZH_CAST_INT( pArg->value.asNum.val.d ) ) & 0xff ] );
      pExpr->value.asString.dealloc = ZH_FALSE;
      pExpr->nLength = 1;

      ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceLEN( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pArg = pParms->value.asList.pExprList;

   /* FIXME: do not optimize when array/hash args have user expressions */
   if( ( pArg->ExprType == ZH_ET_STRING && ! ZH_SUPPORT_USERCP ) ||
       pArg->ExprType == ZH_ET_ARRAY ||
       pArg->ExprType == ZH_ET_HASH )
   {
      PZH_EXPR pExpr = zh_compExprNewLong( pArg->ExprType == ZH_ET_HASH ?
                          pArg->nLength >> 1 : pArg->nLength, ZH_COMP_PARAM );

      ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceEMPTY( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pArg = pParms->value.asList.pExprList;
   ZH_BOOL fReduced = ZH_TRUE, fResult = ZH_FALSE;

   switch( pArg->ExprType )
   {
      case ZH_ET_STRING:
         fResult = zh_strEmpty( pArg->value.asString.string, pArg->nLength );
         break;

      case ZH_ET_ARRAY:
      case ZH_ET_HASH:
         /* FIXME: do not optimize when array/hash args have user expressions */
         fResult = pArg->nLength == 0;
         break;

      case ZH_ET_NUMERIC:
         if( pArg->value.asNum.NumType == ZH_ET_DOUBLE )
            fResult = pArg->value.asNum.val.d == 0.0;
         else
            fResult = pArg->value.asNum.val.l == 0;
         break;

      case ZH_ET_LOGICAL:
         fResult = ! pArg->value.asLogical;
         break;

      case ZH_ET_NIL:
         fResult = ZH_TRUE;
         break;

      case ZH_ET_DATE:
         fResult = pArg->value.asDate.lDate == 0;
         break;

      case ZH_ET_TIMESTAMP:
         fResult = pArg->value.asDate.lDate == 0 &&
                   pArg->value.asDate.lTime == 0;
         break;

      case ZH_ET_CODEBLOCK:
         break;

      /* case ZH_ET_FUNREF: */
      default:
         fReduced = ZH_FALSE;
   }

   if( fReduced )
   {
      PZH_EXPR pExpr = zh_compExprNewLogical( fResult, ZH_COMP_PARAM );

      ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceASC( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == ZH_ET_STRING &&
       ( ! ZH_SUPPORT_USERCP ||
         ( ZH_UCHAR ) pArg->value.asString.string[ 0 ] <= 127 ) )
   {
      PZH_EXPR pExpr = zh_compExprNewLong(
         ( ZH_UCHAR ) pArg->value.asString.string[ 0 ], ZH_COMP_PARAM );

      ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceBCODE( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == ZH_ET_STRING )
   {
      PZH_EXPR pExpr = zh_compExprNewLong(
         ( ZH_UCHAR ) pArg->value.asString.string[ 0 ], ZH_COMP_PARAM );

      ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceINT( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == ZH_ET_NUMERIC )
   {
      PZH_EXPR pExpr;

      if( pArg->value.asNum.NumType == ZH_ET_LONG )
         pExpr = zh_compExprNewLong( pArg->value.asNum.val.l, ZH_COMP_PARAM );
      else
      {
         ZH_MAXDBL dVal = ( ZH_MAXDBL ) pArg->value.asNum.val.d;
         if( ZH_DBL_LIM_LONG( dVal ) )
            pExpr = zh_compExprNewLong( ( ZH_MAXINT ) pArg->value.asNum.val.d, ZH_COMP_PARAM );
         else
            pExpr = zh_compExprNewDouble( pArg->value.asNum.val.d,
                                          pArg->value.asNum.bWidth, 0,
                                          ZH_COMP_PARAM );
      }
      ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }
   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceSTOT( PZH_EXPR pSelf, ZH_USHORT usCount, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pArg = pParms ? pParms->value.asList.pExprList : NULL;
   PZH_EXPR pExpr = NULL;

   if( usCount == 0 )
   {
      pExpr = zh_compExprNewTimeStamp( 0, 0, ZH_COMP_PARAM );
   }
   else if( pArg && pArg->ExprType == ZH_ET_STRING )
   {
      long lDate, lTime;

      zh_timeStampStrRawGet( pArg->value.asString.string, &lDate, &lTime );
      pExpr = zh_compExprNewTimeStamp( lDate, lTime, ZH_COMP_PARAM );
   }

   if( pExpr )
   {
      if( pSelf->value.asFunCall.pParms )
         ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceSTOD( PZH_EXPR pSelf, ZH_USHORT usCount, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pArg = pParms ? pParms->value.asList.pExprList : NULL;
   PZH_EXPR pExpr = NULL;

   if( usCount == 0 )
   {
      pExpr = zh_compExprNewDate( 0, ZH_COMP_PARAM );
   }
   else if( pArg && pArg->ExprType == ZH_ET_STRING &&
            ( pArg->nLength >= 7 || pArg->nLength == 0 ) )
   {
      pExpr = zh_compExprNewDate( pArg->nLength == 0 ? 0 :
                                  zh_dateEncStr( pArg->value.asString.string ),
                                  ZH_COMP_PARAM );
   }

   if( pExpr )
   {
      if( pSelf->value.asFunCall.pParms )
         ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceDTOS( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == ZH_ET_DATE || pArg->ExprType == ZH_ET_TIMESTAMP )
   {
      char szBuffer[ 9 ], * szDate;
      PZH_EXPR pExpr;

      szDate = ( char * ) memcpy( zh_xgrab( 9 ),
            zh_dateDecStr( szBuffer, ( long ) pArg->value.asDate.lDate ), 9 );
      pExpr = zh_compExprNewString( szDate, 8, ZH_TRUE, ZH_COMP_PARAM );

      ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceCTOD( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == ZH_ET_STRING && pArg->nLength == 0 )
   {
      PZH_EXPR pExpr = zh_compExprNewDate( 0, ZH_COMP_PARAM );

      ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceUPPER( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == ZH_ET_STRING )
   {
      ZH_SIZE nLen = pArg->nLength;
      ZH_BOOL fLower = ZH_FALSE;

      if( nLen )
      {
         const char * szValue = pArg->value.asString.string;
         do
         {
            char c = *szValue++;
            if( c >= 'a' && c <= 'z' )
               fLower = ZH_TRUE;
            else if( ! ( ( c >= 'A' && c <= 'Z' ) ||
                         ( c >= '0' && c <= '9' ) || c == ' ' ) )
               break;
         }
         while( --nLen );
      }

      if( nLen == 0 )
      {
         PZH_EXPR pExpr;
         char * szValue;
         ZH_BOOL fDealloc;

         if( fLower )
         {
            if( pArg->nLength == 1 )
            {
               szValue = ( char * ) ZH_UNCONST( zh_szAscii[ ZH_TOUPPER( ( unsigned char )
                                                   pArg->value.asString.string[ 0 ] ) ] );
               fDealloc = ZH_FALSE;
            }
            else
            {
               if( pArg->value.asString.dealloc )
               {
                  szValue = pArg->value.asString.string;
                  pArg->value.asString.dealloc = ZH_FALSE;
                  fDealloc = ZH_TRUE;
               }
               else
               {
                  szValue = ( char * ) zh_xgrab( pArg->nLength + 1 );
                  memcpy( szValue, pArg->value.asString.string, pArg->nLength + 1 );
                  fDealloc = ZH_TRUE;
               }
               do
               {
                  szValue[ nLen ] = ( char ) ZH_TOUPPER( ( unsigned char ) szValue[ nLen ] );
               }
               while( ++nLen < pArg->nLength );
            }
         }
         else
         {
            szValue = pArg->value.asString.string;
            fDealloc = pArg->value.asString.dealloc;
            pArg->value.asString.dealloc = ZH_FALSE;
         }

         pExpr = ZH_COMP_EXPR_NEW( ZH_ET_STRING );
         pExpr->ValType = ZH_EV_STRING;
         pExpr->value.asString.string = szValue;
         pExpr->value.asString.dealloc = fDealloc;
         pExpr->nLength = pArg->nLength;

         ZH_COMP_EXPR_FREE( pParms );
         ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
         memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
         ZH_COMP_EXPR_CLEAR( pExpr );

         return ZH_TRUE;
      }
   }

   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceMIN( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pFirst = pParms->value.asList.pExprList;
   PZH_EXPR pNext = pFirst->pNext;
   PZH_EXPR pExpr = NULL;

   if( pFirst->ExprType == pNext->ExprType )
   {

      if( pFirst->ExprType == ZH_ET_NUMERIC )
      {
         ZH_BYTE bType = ( pFirst->value.asNum.NumType & pNext->value.asNum.NumType );

         switch( bType )
         {
            case ZH_ET_LONG:
               pExpr = pFirst->value.asNum.val.l <= pNext->value.asNum.val.l ?
                       pFirst : pNext;
               break;

            case ZH_ET_DOUBLE:
               pExpr = pFirst->value.asNum.val.d <= pNext->value.asNum.val.d ?
                       pFirst : pNext;
               break;

            default:
               if( pFirst->value.asNum.NumType == ZH_ET_DOUBLE )
                  pExpr = ( pFirst->value.asNum.val.d <= ( double ) pNext->value.asNum.val.l ) ?
                          pFirst : pNext;
               else
                  pExpr = ( ( double ) pFirst->value.asNum.val.l <= pNext->value.asNum.val.d ) ?
                          pFirst : pNext;
         }
      }
      else if( pFirst->ExprType == ZH_ET_DATE )
      {
         pExpr = pFirst->value.asDate.lDate <= pNext->value.asDate.lDate ?
                 pFirst : pNext;
      }
      else if( pFirst->ExprType == ZH_ET_TIMESTAMP )
      {
         pExpr = ( pFirst->value.asDate.lDate < pNext->value.asDate.lDate ||
                   ( pFirst->value.asDate.lDate == pNext->value.asDate.lDate &&
                     pFirst->value.asDate.lTime <= pNext->value.asDate.lTime ) ) ?
                 pFirst : pNext;
      }
      else if( pFirst->ExprType == ZH_ET_LOGICAL )
      {
         pExpr = ! pFirst->value.asLogical ? pFirst : pNext;
      }
   }
   else if( pFirst->ExprType == ZH_ET_DATE && pNext->ExprType == ZH_ET_TIMESTAMP )
   {
      pExpr = pFirst->value.asDate.lDate <= pNext->value.asDate.lDate ?
              pFirst : pNext;
   }
   else if( pFirst->ExprType == ZH_ET_TIMESTAMP && pNext->ExprType == ZH_ET_DATE )
   {
      pExpr = pFirst->value.asDate.lDate < pNext->value.asDate.lDate ?
              pFirst : pNext;
   }

   if( pExpr )
   {
      PZH_EXPR * pExprPtr = &pParms->value.asList.pExprList;

      while( *pExprPtr )
      {
         if( *pExprPtr == pExpr )
         {
            *pExprPtr = pExpr->pNext;
            break;
         }
         pExprPtr = &( *pExprPtr )->pNext;
      }
      ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceMAX( PZH_EXPR pSelf, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pFirst = pParms->value.asList.pExprList;
   PZH_EXPR pNext = pFirst->pNext;
   PZH_EXPR pExpr = NULL;

   if( pFirst->ExprType == pNext->ExprType )
   {

      if( pFirst->ExprType == ZH_ET_NUMERIC )
      {
         ZH_BYTE bType = ( pFirst->value.asNum.NumType & pNext->value.asNum.NumType );

         switch( bType )
         {
            case ZH_ET_LONG:
               pExpr = pFirst->value.asNum.val.l >= pNext->value.asNum.val.l ?
                       pFirst : pNext;
               break;

            case ZH_ET_DOUBLE:
               pExpr = pFirst->value.asNum.val.d >= pNext->value.asNum.val.d ?
                       pFirst : pNext;
               break;

            default:
               if( pFirst->value.asNum.NumType == ZH_ET_DOUBLE )
                  pExpr = ( pFirst->value.asNum.val.d >= ( double ) pNext->value.asNum.val.l ) ?
                          pFirst : pNext;
               else
                  pExpr = ( ( double ) pFirst->value.asNum.val.l >= pNext->value.asNum.val.d ) ?
                          pFirst : pNext;
         }
      }
      else if( pFirst->ExprType == ZH_ET_DATE )
      {
         pExpr = pFirst->value.asDate.lDate >= pNext->value.asDate.lDate ?
                 pFirst : pNext;
      }
      else if( pFirst->ExprType == ZH_ET_TIMESTAMP )
      {
         pExpr = ( pFirst->value.asDate.lDate > pNext->value.asDate.lDate ||
                   ( pFirst->value.asDate.lDate == pNext->value.asDate.lDate &&
                     pFirst->value.asDate.lTime >= pNext->value.asDate.lTime ) ) ?
                 pFirst : pNext;
      }
      else if( pFirst->ExprType == ZH_ET_LOGICAL )
      {
         pExpr = pFirst->value.asLogical ? pFirst : pNext;
      }

   }
   else if( pFirst->ExprType == ZH_ET_DATE && pNext->ExprType == ZH_ET_TIMESTAMP )
   {
      pExpr = pFirst->value.asDate.lDate >= pNext->value.asDate.lDate ?
              pFirst : pNext;
   }
   else if( pFirst->ExprType == ZH_ET_TIMESTAMP && pNext->ExprType == ZH_ET_DATE )
   {
      pExpr = pFirst->value.asDate.lDate > pNext->value.asDate.lDate ?
              pFirst : pNext;
   }

   if( pExpr )
   {
      PZH_EXPR * pExprPtr = &pParms->value.asList.pExprList;

      while( *pExprPtr )
      {
         if( *pExprPtr == pExpr )
         {
            *pExprPtr = pExpr->pNext;
            break;
         }
         pExprPtr = &( *pExprPtr )->pNext;
      }
      ZH_COMP_EXPR_FREE( pParms );
      ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
      ZH_COMP_EXPR_CLEAR( pExpr );
      return ZH_TRUE;
   }

   return ZH_FALSE;
}

ZH_BOOL zh_compExprReduceBitFunc( PZH_EXPR pSelf, ZH_MAXINT nResult, ZH_BOOL fBool, ZH_COMP_DECL )
{
   PZH_EXPR pParms = pSelf->value.asFunCall.pParms;
   PZH_EXPR pExpr = fBool ? zh_compExprNewLogical( nResult != 0, ZH_COMP_PARAM ) :
                            zh_compExprNewLong( nResult, ZH_COMP_PARAM );

   ZH_COMP_EXPR_FREE( pParms );
   ZH_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
   memcpy( pSelf, pExpr, sizeof( ZH_EXPR ) );
   ZH_COMP_EXPR_CLEAR( pExpr );
   return ZH_TRUE;
}
