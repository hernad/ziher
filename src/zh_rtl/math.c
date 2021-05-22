/*
 * Math functions
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
 *
 * Functions for user defined math error handlers, changes and fixes
 * Copyright 2001/2002 IntTec GmbH, Freiburg, Germany,
 *                Author: Martin Vogel <vogel@inttec.de>
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

/* zh_float.h have to be included first */
#include "zh_float.h"
#include "zh_mather.h"
#include "zh_item_api.h"
#include "zh_error_api.h"
#include "zh_vm.h"
#include "zh_stack.h"


#if defined( ZH_MATH_ERRNO )
   #include <errno.h>
#endif

typedef struct
{
   int                  mode;
   PZH_ITEM             block;
   ZH_MATH_HANDLERPROC  handler;
   ZH_MATH_HANDLERPROC  prevHandler;
#if defined( ZH_MATH_HANDLER )
   ZH_MATH_EXCEPTION    exception;
#endif
} ZH_MATHERRDATA, * PZH_MATHERRDATA;

/* Ziher default math error handling routine */
static int zh_matherr( ZH_MATH_EXCEPTION * pexc )
{
   int mode = zh_mathGetErrMode();
   int iRet = 1;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_matherr(%p)", ( void * ) pexc ) );

   if( pexc == NULL || pexc->handled != 0 )
   {
      /* error already handled by other handlers ! */
      return 1;
   }

   if( mode == ZH_MATH_ERRMODE_USER || mode == ZH_MATH_ERRMODE_USERDEFAULT ||
       mode == ZH_MATH_ERRMODE_USERCDEFAULT )
   {
      PZH_ITEM pArg1, pArg2, pError;
      PZH_ITEM pMatherrResult;

      /* create an error object */
      /* NOTE: In case of ZH_MATH_ERRMODE_USER[C]DEFAULT, I am setting both
               EF_CANSUBSTITUTE and EF_CANDEFAULT to .T. here.
               This is forbidden according to the original Cl*pper docs, but
               I think this reflects the situation best here:
               The error handler can either substitute the erroneous value
               (by returning a numeric value) or choose the default error
               handling (by returning .F., as usual) [martin vogel] */
      pError = zh_errRT_New_Subst( ES_ERROR, "MATH", EG_NUMERR, pexc->type,
                                   pexc->error, pexc->funcname, 0, EF_CANSUBSTITUTE |
                                   ( mode == ZH_MATH_ERRMODE_USER ? 0 : EF_CANDEFAULT ) );

      /* Assign the new array to the object data item. */
      /* NOTE: Unfortunately, we cannot decide whether one or two parameters
               have been used when the math function has been called, so we
               always take two */
      pArg1 = zh_itemPutND( NULL, pexc->arg1 );
      pArg2 = zh_itemPutND( NULL, pexc->arg2 );
      zh_errPutArgs( pError, 2, pArg1, pArg2 );
      zh_itemRelease( pArg1 );
      zh_itemRelease( pArg2 );

      /* launch error codeblock */
      pMatherrResult = zh_errLaunchSubst( pError );
      zh_errRelease( pError );

      if( pMatherrResult )
      {
         if( ZH_IS_NUMERIC( pMatherrResult ) )
         {
            pexc->retval = zh_itemGetND( pMatherrResult );
            zh_itemGetNLen( pMatherrResult, &pexc->retvalwidth, &pexc->retvaldec );
            pexc->handled = 1;
         }
         zh_itemRelease( pMatherrResult );
      }
   }

   /* math exception not handled by Ziher error routine above ? */
   if( pexc->handled == 0 )
   {
      switch( mode )
      {
         case ZH_MATH_ERRMODE_USER:
            /* user failed to handle the math exception, so quit the app
               [yes, that's the meaning of this mode !!] */
            iRet = 0;
            zh_vmRequestQuit();
            break;

         case ZH_MATH_ERRMODE_DEFAULT:
         case ZH_MATH_ERRMODE_USERDEFAULT:
            /* return 1 to suppress C RTL error msgs, but leave error
               handling to the calling Ziher routine */
            break;

         case ZH_MATH_ERRMODE_CDEFAULT:
         case ZH_MATH_ERRMODE_USERCDEFAULT:
            /* use the correction value supplied in pexc->retval */
            pexc->handled = 1;
            break;
      }
   }

   return iRet;  /* error handling successful */
}

static void zh_mathErrDataInit( void * Cargo )
{
   PZH_MATHERRDATA pMathErr = ( PZH_MATHERRDATA ) Cargo;

   pMathErr->mode = ZH_MATH_ERRMODE_DEFAULT;

   pMathErr->handler = zh_matherr;

#if defined( ZH_MATH_HANDLER )
   pMathErr->exception.type = ZH_MATH_ERR_NONE;
   pMathErr->exception.funcname = "";
   pMathErr->exception.error = "";
   pMathErr->exception.arg1 = 0.0;
   pMathErr->exception.arg2 = 0.0;
   pMathErr->exception.retval = 0.0;
   pMathErr->exception.retvalwidth = -1;   /* we don't know */
   pMathErr->exception.retvaldec = -1;     /* use standard SET DECIMALS */
   pMathErr->exception.handled = 1;
#endif
}

static void zh_mathErrDataRelease( void * Cargo )
{
   PZH_MATHERRDATA pMathErr = ( PZH_MATHERRDATA ) Cargo;

   zh_itemRelease( pMathErr->block );
}

static ZH_TSD_NEW( s_mathErrData, sizeof( ZH_MATHERRDATA ),
                   zh_mathErrDataInit, zh_mathErrDataRelease );

#define zh_mathErrData()  ( ( PZH_MATHERRDATA ) zh_stackGetTSD( &s_mathErrData ) )


/* Ziher Math functions Part I:
 * handling math errors, C math lib redirection
 */

/* reset math error information */
void zh_mathResetError( ZH_MATH_EXCEPTION * pzh_exc )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mathResetError(%p)", ( void * ) pzh_exc ) );

   ZH_SYMBOL_UNUSED( pzh_exc );

#if defined( ZH_MATH_HANDLER )
   {
      PZH_MATHERRDATA pMathErr = zh_mathErrData();
      pMathErr->exception.type = ZH_MATH_ERR_NONE;
      pMathErr->exception.funcname = "";
      pMathErr->exception.error = "";
      pMathErr->exception.arg1 = 0.0;
      pMathErr->exception.arg2 = 0.0;
      pMathErr->exception.retval = 0.0;
      pMathErr->exception.retvalwidth = -1;   /* we don't know */
      pMathErr->exception.retvaldec = -1;     /* use standard SET DECIMALS */
      pMathErr->exception.handled = 1;
   }
#elif defined( ZH_MATH_ERRNO )
   errno = 0;
#endif
}

/* route C math lib errors to Ziher error handling */
#if defined( ZH_MATH_HANDLER )

int matherr( struct exception * err )
{
   int retval;
   ZH_MATH_HANDLERPROC mathHandler;
   ZH_MATH_EXCEPTION * pExc;

   ZH_TRACE( ZH_TR_DEBUG, ( "matherr(%p)", ( void * ) err ) );

   pExc = &zh_mathErrData()->exception;

   /* map math error types */
   switch( err->type )
   {
      case DOMAIN:
         pExc->type = ZH_MATH_ERR_DOMAIN;
         pExc->error = "Argument not in domain of function";
         break;

      case SING:
         pExc->type = ZH_MATH_ERR_SING;
         pExc->error = "Calculation results in singularity";
         break;

      case OVERFLOW:
         pExc->type = ZH_MATH_ERR_OVERFLOW;
         pExc->error = "Calculation result too large to represent";
         break;

      case UNDERFLOW:
         pExc->type = ZH_MATH_ERR_UNDERFLOW;
         pExc->error = "Calculation result too small to represent";
         break;

      case TLOSS:
         pExc->type = ZH_MATH_ERR_TLOSS;
         pExc->error = "Total loss of significant digits";
         break;

      case PLOSS:
         pExc->type = ZH_MATH_ERR_PLOSS;
         pExc->error = "Partial loss of significant digits";
         break;

      default:
         pExc->type = ZH_MATH_ERR_UNKNOWN;
         pExc->error = "Unknown math error";
         break;
   }

   pExc->funcname = err->name;
   pExc->arg1 = err->arg1;
   pExc->arg2 = err->arg2;
   pExc->retval = err->retval;
   pExc->handled = 0;

   mathHandler = zh_mathGetHandler();
   if( mathHandler )
   {
      retval = ( *( mathHandler ) ) ( pExc );
      err->retval = pExc->retval;
   }
   else
   {
      /* there is no custom math handler */
      retval = 1;  /* don't print any message, don't set errno and use return value provided by C RTL */
   }
   return retval;
}
#endif

ZH_BOOL zh_mathGetError( ZH_MATH_EXCEPTION * pzh_exc, const char * szFunc,
                         double arg1, double arg2, double dResult )
{
#if defined( ZH_MATH_ERRNO )

   int errCode, v;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mathGetError(%p,%s,%lf,%lf,%lf)", ( void * ) pzh_exc, szFunc, arg1, arg2, dResult ) );

   switch( errno )
   {
      case 0:
         return ZH_FALSE;
      case EDOM:
      case ERANGE:
   #if defined( EOVERFLOW )
      case EOVERFLOW:
   #endif
         errCode = errno;
         break;

      default:
         ZH_NUMTYPE( v, dResult );
         if( ( v & _ZH_NUM_NAN ) != 0 )
            errCode = EDOM;
         else if( ( v & ( _ZH_NUM_NINF | _ZH_NUM_PINF ) ) != 0 )
            errCode = ERANGE;
         else
            errCode = errno;
   }

   /* map math error types */
   switch( errCode )
   {
      case EDOM:
         pzh_exc->type = ZH_MATH_ERR_DOMAIN;
         pzh_exc->error = "Argument not in domain of function";
         break;

      case ERANGE:
         pzh_exc->type = ZH_MATH_ERR_SING;
         pzh_exc->error = "Calculation results in singularity";
         break;
   #if defined( EOVERFLOW )
      case EOVERFLOW:
         pzh_exc->type = ZH_MATH_ERR_OVERFLOW;
         pzh_exc->error = "Calculation result too large to represent";
         break;
   #endif
      default:
         pzh_exc->type = ZH_MATH_ERR_UNKNOWN;
         pzh_exc->error = "Unknown math error";
         break;
   }

   pzh_exc->funcname = szFunc;
   pzh_exc->arg1 = arg1;
   pzh_exc->arg2 = arg2;
   pzh_exc->retval = dResult;
   pzh_exc->handled = 0;
   pzh_exc->retvalwidth = -1; /* we don't know */
   pzh_exc->retvaldec = -1;   /* use standard SET DECIMALS */

   {
      ZH_MATH_HANDLERPROC mathHandler = zh_mathGetHandler();
      if( mathHandler )
         ( *mathHandler )( pzh_exc );
   }
   return ZH_TRUE;
#else
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mathGetError(%p,%s,%lf,%lf,%lf)", ( void * ) pzh_exc, szFunc, arg1, arg2, dResult ) );

   ZH_SYMBOL_UNUSED( dResult );
   ZH_SYMBOL_UNUSED( arg1 );
   ZH_SYMBOL_UNUSED( arg2 );
   ZH_SYMBOL_UNUSED( szFunc );

   #if defined( ZH_MATH_HANDLER )

   memcpy( pzh_exc, &zh_mathErrData()->exception, sizeof( ZH_MATH_EXCEPTION ) );
   return pzh_exc->type != ZH_MATH_ERR_NONE;

   #else

   ZH_SYMBOL_UNUSED( pzh_exc );
   return ZH_FALSE;

   #endif

#endif
}


/* Ziher Math functions Part II:
 * handling math errors, Ziher default handling routine
 */

/* set error handling mode of zh_matherr() */
int zh_mathSetErrMode( int imode )
{
   PZH_MATHERRDATA pMathErr;
   int oldmode;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mathSetErrMode (%i)", imode ) );

   pMathErr = zh_mathErrData();
   oldmode = pMathErr->mode;

   if( imode == ZH_MATH_ERRMODE_DEFAULT ||
       imode == ZH_MATH_ERRMODE_CDEFAULT ||
       imode == ZH_MATH_ERRMODE_USER ||
       imode == ZH_MATH_ERRMODE_USERDEFAULT ||
       imode == ZH_MATH_ERRMODE_USERCDEFAULT )
   {
      pMathErr->mode = imode;
   }

   return oldmode;
}

/* get error handling mode of zh_matherr() */
int zh_mathGetErrMode( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mathGetErrMode()" ) );
   return zh_mathErrData()->mode;
}

/* Ziher equivalent to mathSet/GetErrMode */
ZH_FUNC( ZH_MATHERMODE )        /* ([<nNewMode>]) --> <nOldMode> */
{
   zh_retni( zh_mathGetErrMode() );

   /* set new mode */
   if( ZH_IS_PARAM_NUM( 1 ) )
      zh_mathSetErrMode( zh_parni( 1 ) );
}


/* Ziher Math functions Part III:
 * (de)installing and (de)activating custom math error handlers
 */

/* install a ziher-like math error handler (that will be called by the
   matherr() function), return old handler */
ZH_MATH_HANDLERPROC zh_mathSetHandler( ZH_MATH_HANDLERPROC handlerproc )
{
   ZH_MATH_HANDLERPROC oldHandlerProc;
   PZH_MATHERRDATA pMathErr;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mathSetHandler (%p)", ( void * ) handlerproc ) );

   pMathErr = zh_mathErrData();
   oldHandlerProc = pMathErr->handler;
   pMathErr->handler = handlerproc;

   return oldHandlerProc;
}

/* get current ziher-like math error handler */
ZH_MATH_HANDLERPROC zh_mathGetHandler( void )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_mathGetHandler ()" ) );

   return zh_mathErrData()->handler;
}

/* Ziher Math functions Part IV:
 * example of zh_mathSet/GetHandler: add a new math handler that
 * calls a given codeblock for every math error
 */

static int zh_matherrblock( ZH_MATH_EXCEPTION * pexc )
{
   PZH_MATHERRDATA pMathErr = zh_mathErrData();
   int retval;

   /* call codeblock for both case: handled and unhandled exceptions */

   if( pMathErr->block )
   {
      PZH_ITEM pArray, pRet;
      PZH_ITEM pType, pFuncname, pError, pArg1, pArg2, pRetval, pHandled;
      const char * funcname = pexc->funcname;

      if( funcname == ZH_ERR_FUNCNAME )
      {
         PZH_SYMBOL pSym = zh_itemGetSymbol( zh_stackBaseItem() );
         if( pSym )
            funcname = pSym->szName;
      }

      pType = zh_itemPutNI( NULL, pexc->type );
      pFuncname = zh_itemPutC( NULL, funcname );
      pError = zh_itemPutC( NULL, pexc->error );
      pArg1 = zh_itemPutND( NULL, pexc->arg1 );
      pArg2 = zh_itemPutND( NULL, pexc->arg2 );
      pRetval = zh_itemPutNDLen( NULL, pexc->retval, pexc->retvalwidth, pexc->retvaldec );
      pHandled = zh_itemPutL( NULL, pexc->handled );

      pArray = zh_itemArrayNew( 2 );
      zh_itemArrayPut( pArray, 1, pRetval );
      zh_itemArrayPut( pArray, 2, pHandled );

      /* launch error codeblock that can
         a) change the members of the array = {dRetval, lHandled} to set the
            return value of the math C RTL routine and the <exception handled
            flag> and it
         b) can return an integer value to set the return value of matherr().
         NOTE that these values are only used if lHandled was .F. and is set
         to .T. within the codeblock */
      pRet = zh_itemDo( pMathErr->block, 6, pType, pFuncname, pError, pArg1, pArg2, pArray );

      zh_itemRelease( pType );
      zh_itemRelease( pFuncname );
      zh_itemRelease( pError );
      zh_itemRelease( pArg1 );
      zh_itemRelease( pArg2 );
      zh_itemRelease( pRetval );
      zh_itemRelease( pHandled );

      if( pexc->handled )
      {
         /* math exception has already been handled, so codeblock call above
            was only informative */
         retval = 1;
      }
      else
      {
         /* exception handled by codeblock ? */
         pHandled = zh_itemArrayGet( pArray, 2 );
         if( pHandled )
         {
            pexc->handled = zh_itemGetL( pHandled );
            zh_itemRelease( pHandled );
         }

         if( pexc->handled )
         {
            /* YES ! */
            /* extract retval for math routine and matherr() */
            pRetval = zh_itemArrayGet( pArray, 1 );
            if( pRetval )
            {
               pexc->retval = zh_itemGetND( pRetval );
               zh_itemGetNLen( pRetval, &pexc->retvalwidth, &pexc->retvaldec );
               zh_itemRelease( pRetval );
            }
            if( pRet && ZH_IS_NUMERIC( pRet ) )
            {
               retval = zh_itemGetNI( pRet );  /* block may also return 0 to force C math lib warnings */
               zh_itemRelease( pRet );
            }
            else
            {
               retval = 1;  /* default return value to suppress C math lib warnings */
            }
         }
         else
         {
            /* NO ! */
            retval = 1;
         }
      }
      zh_itemRelease( pArray );
   }
   else
   {
      retval = 1;  /* default return value to suppress C math lib warnings */
   }

   if( pMathErr->prevHandler )
   {
      if( pexc->handled )
      {
         /* the error is handled, so simply inform the previous handler */
         ( *pMathErr->prevHandler )( pexc );
      }
      else
      {
         /* else go on error handling within previous handler */
         retval = ( *pMathErr->prevHandler )( pexc );
      }
   }
   return retval;
}

/* set/get math error block */
ZH_FUNC( ZH_MATHERBLOCK )  /* ([<nNewErrorBlock>]) --> <nOldErrorBlock> */
{
   PZH_MATHERRDATA pMathErr = zh_mathErrData();

   /* immediately install zh_matherrblock and keep it permanently installed!
      This is not dangerous because zh_matherrorblock will always call the
      previous error handler */
   if( pMathErr->prevHandler == NULL )
   {
      pMathErr->prevHandler = zh_mathSetHandler( zh_matherrblock );
   }

   /* return old math handler */
   if( pMathErr->block == NULL )
   {
      zh_ret();
   }
   else
   {
      zh_itemReturn( pMathErr->block );
   }

   if( zh_pcount() > 0 )
   {
      /* set new error block */
      PZH_ITEM pNewErrorBlock = zh_param( 1, ZH_IT_EVALITEM );

      if( pNewErrorBlock )
      {
         if( pMathErr->block == NULL )
         {
            pMathErr->block = zh_itemNew( NULL );
         }
         zh_itemCopy( pMathErr->block, pNewErrorBlock );
      }
      else
      {
         /* a parameter other than a block has been passed
            -> delete error handler! */
         if( pMathErr->block )
         {
            zh_itemRelease( pMathErr->block );
            pMathErr->block = NULL;
         }
      }
   }
}

/* Ziher Math functions Part V:
 * Exp(), Log(), Sqrt()
 */

ZH_FUNC( EXP )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dResult, dArg = zh_parnd( 1 );

      zh_mathResetError( &zh_exc );
      dResult = exp( dArg );
      if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
      {
         if( zh_exc.handled )
            zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
         else
         {

            if( zh_exc.type == ZH_MATH_ERR_OVERFLOW )
               zh_retndlen( HUGE_VAL, -1, -1 );
            else
               zh_retnd( 0.0 );
         }
      }
      else
         zh_retnd( dResult );
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1096, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( LOG )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dArg = zh_parnd( 1 );

      if( dArg <= 0 )
         zh_retndlen( -HUGE_VAL, -1, -1 );  /* return -infinity */
      else
      {
         double dResult;
         zh_mathResetError( &zh_exc );
         dResult = log( dArg );
         if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
         {
            if( zh_exc.handled )
               zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
            else
            {
               switch( zh_exc.type )
               {
                  case ZH_MATH_ERR_SING:               /* argument to log was 0.0 */
                  case ZH_MATH_ERR_DOMAIN:             /* argument to log was < 0.0 */
                     zh_retndlen( -HUGE_VAL, -1, -1 ); /* return -infinity */
                     break;

                  default:
                     zh_retnd( 0.0 );
                     break;
               }
            }
         }
         else
            zh_retnd( dResult );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1095, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}

ZH_FUNC( SQRT )
{
   if( ZH_IS_PARAM_NUM( 1 ) )
   {
      ZH_MATH_EXCEPTION zh_exc;
      double dArg = zh_parnd( 1 );

      if( dArg <= 0 )
         zh_retnd( 0.0 );
      else
      {
         double dResult;
         zh_mathResetError( &zh_exc );
         dResult = sqrt( dArg );
         if( zh_mathGetError( &zh_exc, ZH_ERR_FUNCNAME, dArg, 0.0, dResult ) )
         {
            if( zh_exc.handled )
               zh_retndlen( zh_exc.retval, zh_exc.retvalwidth, zh_exc.retvaldec );
            else
               zh_retnd( 0.0 );  /* return 0.0 on all errors (all (?) of type DOMAIN) */
         }
         else
            zh_retnd( dResult );
      }
   }
   else
      zh_errRT_BASE_SubstR( EG_ARG, 1097, NULL, ZH_ERR_FUNCNAME, ZH_ERR_ARGS_BASEPARAMS );
}
