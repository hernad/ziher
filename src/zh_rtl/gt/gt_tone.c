/*
 * Low-level tone code common to some GT drivers
 *
 * Copyright 2006 Przemyslaw Czerpak
 *
 * the body of Tone() function from Windows taken from GTWIN created by
 * the following authors:
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 * Copyright 1999-2006 Paul Tucker <ptucker@sympatico.ca>
 * Copyright 2005 Andi Jahja <andij@aonlippo.co.id>
 * Copyright 2005 Przemyslaw Czerpak
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

/* NOTE: User programs should never call this layer directly! */

#include "zh_api.h"
#include "zh_gt_core.h"

#if defined( ZH_OS_WIN )

#include <windows.h>

#undef ZH_HAS_WIN9X_TONE

#if defined( ZH_CPU_X86 ) && \
    ( ( defined( _MSC_VER ) && _MSC_VER < 1900 ) || defined( __MINGW32__ ) )

#define ZH_HAS_WIN9X_TONE

#if defined( _MSC_VER )
   #include <conio.h>
#endif

static int zh_Inp9x( unsigned short int usPort )
{
   unsigned short int usVal;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_Inp9x(%hu)", usPort ) );

   
   #if defined( __MINGW32__ )

      __asm__ __volatile__ ("inb %w1,%b0":"=a" (usVal):"Nd" (usPort));

   #else

      usVal = ( unsigned short int ) _inp( usPort );

   #endif

   return usVal;
}

static int zh_Outp9x( unsigned short int usPort, unsigned short int usVal )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_Outp9x(%hu, %hu)", usPort, usVal ) );

   #if defined( __MINGW32__ )

      __asm__ __volatile__ ("outb %b0,%w1": :"a" (usVal), "Nd" (usPort));

   #else

      _outp( usPort, usVal );

   #endif

   return usVal;
}

/* dDurat is in seconds */
static void zh_gt_w9xTone( double dFreq, double dDurat )
{
   int uLSB, uMSB;
   unsigned long lAdjFreq;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_w9xtone(%lf, %lf)", dFreq, dDurat ) );

   /* sync with internal clock with very small time period */
   zh_idleSleep( 0.01 );

   if( dFreq >= 20.0 )
   {
      /* Setup Sound Control Port Registers and timer channel 2 */
      zh_Outp9x( 67, 182 );

      lAdjFreq = ( unsigned long ) ( 1193180 / dFreq );

      if( ( long ) lAdjFreq < 0 )
         uLSB = lAdjFreq + 65536;
      else
         uLSB = lAdjFreq % 256;

      if( ( long ) lAdjFreq < 0 )
         uMSB = lAdjFreq + 65536;
      else
         uMSB = lAdjFreq / 256;

      /* set the frequency ( LSB, MSB ) */

      zh_Outp9x( 66, ( unsigned short int ) uLSB );
      zh_Outp9x( 66, ( unsigned short int ) uMSB );

      /* Get current Port setting */
      /* enable Speaker Data & Timer gate bits */
      /* (00000011B is bitmask to enable sound) */
      /* Turn on Speaker - sound Tone for duration.. */

      zh_Outp9x( 97, ( unsigned short int ) zh_Inp9x( 97 ) | 3 );

      zh_idleSleep( dDurat );

      /* Read back current Port value for Reset */
      /* disable Speaker Data & Timer gate bits */
      /* (11111100B is bitmask to disable sound) */
      /* Turn off the Speaker ! */

      zh_Outp9x( 97, zh_Inp9x( 97 ) & 0xFC );
   }
   else
      zh_idleSleep( dDurat );
}

#endif

/* dDurat is in seconds */
static void zh_gt_wNtTone( double dFreq, double dDurat )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_wNtTone(%lf, %lf)", dFreq, dDurat ) );

   if( dFreq >= 37.0 )
      Beep( ( DWORD ) dFreq, ( DWORD ) ( dDurat * 1000 ) );  /* Beep wants Milliseconds */
   else
      zh_idleSleep( dDurat );
}

/* dDuration is in 'Ticks' (18.2 per second) */
void zh_gt_winapi_tone( double dFrequency, double dDuration )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_gt_winapi_tone(%lf, %lf)", dFrequency, dDuration ) );

   /* Convert from ticks to seconds */
   dDuration = ( ZH_MIN( ZH_MAX( 1.0, dDuration ), ULONG_MAX ) ) / 18.2;

   /* keep the frequency in an acceptable range */
   dFrequency = ZH_MIN( ZH_MAX( 0.0, dFrequency ), 32767.0 );

#if defined( ZH_HAS_WIN9X_TONE )
   if( zh_iswin9x() )
      /* If Windows 95 or 98, use w9xTone for chosen C compilers */
      zh_gt_w9xTone( dFrequency, dDuration );
   else
#endif
      /* If Windows NT or NT2k, use wNtTone, which redirects Tone() to
         Windows API Beep() function */
      zh_gt_wNtTone( dFrequency, dDuration );
}

#endif /* ZH_OS_WIN */
