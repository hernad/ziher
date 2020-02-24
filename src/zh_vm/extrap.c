/*
 * Exception handlers
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 2008 Mindaugas Kavaliauskas (dbtopas at dbtopas.lt) (zh_winExceptionHandler() Windows exception info dump code.)
 * Copyright 2008-2010 Viktor Szakats (zh_winExceptionHandler() Module listing code, x86_64/WinCE/ARM support, OS/2, MIPS32, MIPS64, SH, IA64 CPU dumps.)
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
#include "zh_fs_api.h"
#include "zh_date.h"
#include "zh_error_api.h"
#include "zh_set.h"

#if defined( ZH_OS_UNIX )
#  include <unistd.h>
#  include <signal.h>
#  if defined( SIGSTKSZ ) && \
      ( ( defined( _BSD_SOURCE ) && _BSD_SOURCE ) || \
        ( defined( _XOPEN_SOURCE ) && _XOPEN_SOURCE >= 500 ) )
#     define ZH_SIGNAL_EXCEPTION_HANDLER
#  endif
#elif defined( ZH_OS_WIN )
#  include <windows.h>
#  include <tlhelp32.h>
#  include "zh_win_uni.h"
#  ifndef TH32CS_SNAPMODULE32
#  define TH32CS_SNAPMODULE32  0x00000010
#  endif
#endif

#if defined( ZH_SIGNAL_EXCEPTION_HANDLER )
   static ZH_BYTE * s_signal_stack[ SIGSTKSZ ];
#endif

#if defined( ZH_OS_WIN )

static LONG WINAPI zh_winExceptionHandler( struct _EXCEPTION_POINTERS * pExceptionInfo )
{
   char errmsg[ 8192 ];
   int errmsglen = sizeof( errmsg ) - 1;

   errmsg[ 0 ] = '\0';

#if defined( ZH_OS_WIN_64 ) && defined( ZH_CPU_X86_64 )
   {
      char buf[ 32 ];
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;
      const char * szCode;

      /* two most common codes */
      switch( pExceptionInfo->ExceptionRecord->ExceptionCode )
      {
         case EXCEPTION_ACCESS_VIOLATION:
            szCode = " " "ACCESS_VIOLATION";
            break;
         case EXCEPTION_IN_PAGE_ERROR:
            szCode = " " "IN_PAGE_ERROR";
            break;
         default:
            szCode = "";
      }

      zh_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X%s\n"
         "    Exception Address:%016" PFLL "X\n"
         "    RAX:%016" PFLL "X  RBX:%016" PFLL "X  RCX:%016" PFLL "X  RDX:%016" PFLL "X\n"
         "    RSI:%016" PFLL "X  RDI:%016" PFLL "X  RBP:%016" PFLL "X\n"
         "    R8 :%016" PFLL "X  R9 :%016" PFLL "X  R10:%016" PFLL "X  R11:%016" PFLL "X\n"
         "    R12:%016" PFLL "X  R13:%016" PFLL "X  R14:%016" PFLL "X  R15:%016" PFLL "X\n"
         "    CS:RIP:%04X:%016" PFLL "X  SS:RSP:%04X:%016" PFLL "X\n"
         "    DS:%04X  ES:%04X  FS:%04X  GS:%04X\n"
         "    Flags:%08X\n",
         ( ZH_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode, szCode,
         ( ZH_PTRUINT ) pExceptionInfo->ExceptionRecord->ExceptionAddress,
         pCtx->Rax, pCtx->Rbx, pCtx->Rcx, pCtx->Rdx,
         pCtx->Rsi, pCtx->Rdi, pCtx->Rbp,
         pCtx->R8 , pCtx->R9 , pCtx->R10, pCtx->R11,
         pCtx->R12, pCtx->R13, pCtx->R14, pCtx->R15,
         ( ZH_U32 ) pCtx->SegCs, pCtx->Rip, ( ZH_U32 ) pCtx->SegSs, pCtx->Rsp,
         ( ZH_U32 ) pCtx->SegDs, ( ZH_U32 ) pCtx->SegEs, ( ZH_U32 ) pCtx->SegFs, ( ZH_U32 ) pCtx->SegGs,
         ( ZH_U32 ) pCtx->EFlags );

      if( pExceptionInfo->ExceptionRecord->NumberParameters &&
          pExceptionInfo->ExceptionRecord->NumberParameters < ( DWORD ) EXCEPTION_MAXIMUM_PARAMETERS )
      {
         DWORD arg;

         zh_strncat( errmsg, "    Exception Parameters:", errmsglen );
         for( arg = 0; arg < pExceptionInfo->ExceptionRecord->NumberParameters; ++arg )
         {
            zh_snprintf( buf, sizeof( buf ), " %016" PFLL "X", ( ZH_U64 ) pExceptionInfo->ExceptionRecord->ExceptionInformation[ arg ] );
            zh_strncat( errmsg, buf, errmsglen );
         }
         zh_strncat( errmsg, "\n", errmsglen );
      }

      /* TODO: 64-bit stack trace.
               See: - StackWalk64()
                    - https://www.codeproject.com/KB/threads/StackWalker.aspx?fid=202364 */
   }
#elif defined( ZH_OS_WIN_64 ) && defined( ZH_CPU_IA_64 )
   {
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;

      zh_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X\n"
         "    Exception Address:%016" PFLL "X\n"
         "    IS0 :%016" PFLL "X  IS1 :%016" PFLL "X  IS2 :%016" PFLL "X  IS3 :%016" PFLL "X\n"
         "    IT0 :%016" PFLL "X  IT1 :%016" PFLL "X  IT2 :%016" PFLL "X  IT3 :%016" PFLL "X\n"
         "    IT4 :%016" PFLL "X  IT5 :%016" PFLL "X  IT6 :%016" PFLL "X  IT7 :%016" PFLL "X\n"
         "    IT8 :%016" PFLL "X  IT9 :%016" PFLL "X  IT10:%016" PFLL "X  IT11:%016" PFLL "X\n"
         "    IT12:%016" PFLL "X  IT13:%016" PFLL "X  IT14:%016" PFLL "X  IT15:%016" PFLL "X\n"
         "    IT16:%016" PFLL "X  IT17:%016" PFLL "X  IT18:%016" PFLL "X  IT19:%016" PFLL "X\n"
         "    IT20:%016" PFLL "X  IT21:%016" PFLL "X  IT22:%016" PFLL "X\n"
         "    IGp :%016" PFLL "X  IV0 :%016" PFLL "X  ISp :%016" PFLL "X  ITeb:%016" PFLL "X\n"
         "    INat:%016" PFLL "X\n",
         ( ZH_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
         pExceptionInfo->ExceptionRecord->ExceptionAddress,
         pCtx->IntS0 , pCtx->IntS1 , pCtx->IntS2 , pCtx->IntS3 ,
         pCtx->IntT0 , pCtx->IntT1 , pCtx->IntT2 , pCtx->IntT3 ,
         pCtx->IntT4 , pCtx->IntT5 , pCtx->IntT6 , pCtx->IntT7 ,
         pCtx->IntT8 , pCtx->IntT9 , pCtx->IntT10, pCtx->IntT11,
         pCtx->IntT12, pCtx->IntT13, pCtx->IntT14, pCtx->IntT15,
         pCtx->IntT16, pCtx->IntT17, pCtx->IntT18, pCtx->IntT19,
         pCtx->IntT20, pCtx->IntT21, pCtx->IntT22,
         pCtx->IntGp , pCtx->IntV0 , pCtx->IntSp , pCtx->IntTeb,
         pCtx->IntNats );
   }
#elif defined( ZH_OS_WIN_CE ) && defined( ZH_CPU_ARM )
   {
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;

      zh_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X\n"
         "    Exception Address:%08X\n"
         "    R0 :%08X  R1 :%08X  R2 :%08X  R3 :%08X\n"
         "    R4 :%08X  R5 :%08X  R6 :%08X  R7 :%08X\n"
         "    R8 :%08X  R9 :%08X  R10:%08X  R11:%08X\n"
         "    R12:%08X\n"
         "    SP :%08X  LR :%08X  PC :%08X\n"
         "    Flags:%08X\n",
         ( ZH_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
         ( ZH_U32 ) pExceptionInfo->ExceptionRecord->ExceptionAddress,
         ( ZH_U32 ) pCtx->R0 , ( ZH_U32 ) pCtx->R1 , ( ZH_U32 ) pCtx->R2 , ( ZH_U32 ) pCtx->R3 ,
         ( ZH_U32 ) pCtx->R4 , ( ZH_U32 ) pCtx->R5 , ( ZH_U32 ) pCtx->R6 , ( ZH_U32 ) pCtx->R7 ,
         ( ZH_U32 ) pCtx->R8 , ( ZH_U32 ) pCtx->R9 , ( ZH_U32 ) pCtx->R10, ( ZH_U32 ) pCtx->R11,
         ( ZH_U32 ) pCtx->R12,
         ( ZH_U32 ) pCtx->Sp , ( ZH_U32 ) pCtx->Lr , ( ZH_U32 ) pCtx->Pc,
         ( ZH_U32 ) pCtx->Psr );
   }
#elif defined( ZH_OS_WIN_CE ) && defined( ZH_CPU_MIPS ) && defined( ZH_ARCH_32BIT )
   {
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;

      zh_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X\n"
         "    Exception Address:%08X\n"
         "    IZe:%08X  IAt:%08X  ILo:%08X  IHi:%08X\n"
         "    IA0:%08X  IA1:%08X  IA2:%08X  IA3:%08X\n"
         "    IT0:%08X  IT1:%08X  IT2:%08X  IT3:%08X\n"
         "    IT4:%08X  IT5:%08X  IT6:%08X  IT7:%08X\n"
         "    IT8:%08X  IT9:%08X  IV0:%08X  IV1:%08X\n"
         "    IS0:%08X  IS1:%08X  IS2:%08X  IS3:%08X\n"
         "    IS4:%08X  IS5:%08X  IS6:%08X  IS7:%08X\n"
         "    IS8:%08X  IK0:%08X  IK1:%08X\n"
         "    IGp:%08X  ISp:%08X  IRa:%08X\n"
         "    Fsr:%08X  Fir:%08X  Psr:%08X\n",
         ( ZH_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
         ( ZH_U32 ) pExceptionInfo->ExceptionRecord->ExceptionAddress,
         ( ZH_U32 ) pCtx->IntZero, ( ZH_U32 ) pCtx->IntAt, ( ZH_U32 ) pCtx->IntLo, ( ZH_U32 ) pCtx->IntHi,
         ( ZH_U32 ) pCtx->IntA0, ( ZH_U32 ) pCtx->IntA1, ( ZH_U32 ) pCtx->IntA2, ( ZH_U32 ) pCtx->IntA3,
         ( ZH_U32 ) pCtx->IntT0, ( ZH_U32 ) pCtx->IntT1, ( ZH_U32 ) pCtx->IntT2, ( ZH_U32 ) pCtx->IntT3,
         ( ZH_U32 ) pCtx->IntT4, ( ZH_U32 ) pCtx->IntT5, ( ZH_U32 ) pCtx->IntT6, ( ZH_U32 ) pCtx->IntT7,
         ( ZH_U32 ) pCtx->IntT8, ( ZH_U32 ) pCtx->IntT9, ( ZH_U32 ) pCtx->IntV0, ( ZH_U32 ) pCtx->IntV1,
         ( ZH_U32 ) pCtx->IntS0, ( ZH_U32 ) pCtx->IntS1, ( ZH_U32 ) pCtx->IntS2, ( ZH_U32 ) pCtx->IntS3,
         ( ZH_U32 ) pCtx->IntS4, ( ZH_U32 ) pCtx->IntS5, ( ZH_U32 ) pCtx->IntS6, ( ZH_U32 ) pCtx->IntS7,
         ( ZH_U32 ) pCtx->IntS8, ( ZH_U32 ) pCtx->IntK0, ( ZH_U32 ) pCtx->IntK1,
         ( ZH_U32 ) pCtx->IntGp, ( ZH_U32 ) pCtx->IntSp, ( ZH_U32 ) pCtx->IntRa,
         ( ZH_U32 ) pCtx->Fsr  , ( ZH_U32 ) pCtx->Fir  , ( ZH_U32 ) pCtx->Psr );
   }
#elif defined( ZH_OS_WIN_CE ) && defined( ZH_CPU_MIPS ) && defined( ZH_ARCH_64BIT ) /* Such platform doesn't currently exist [2010]. */
   {
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;

      zh_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X\n"
         "    Exception Address:%016" PFLL "X\n"
         "    IZe:%016" PFLL "X  IAt:%016" PFLL "X  ILo:%016" PFLL "X  IHi:%016" PFLL "X\n"
         "    IA0:%016" PFLL "X  IA1:%016" PFLL "X  IA2:%016" PFLL "X  IA3:%016" PFLL "X\n"
         "    IT0:%016" PFLL "X  IT1:%016" PFLL "X  IT2:%016" PFLL "X  IT3:%016" PFLL "X\n"
         "    IT4:%016" PFLL "X  IT5:%016" PFLL "X  IT6:%016" PFLL "X  IT7:%016" PFLL "X\n"
         "    IT8:%016" PFLL "X  IT9:%016" PFLL "X  IV0:%016" PFLL "X  IV1:%016" PFLL "X\n"
         "    IS0:%016" PFLL "X  IS1:%016" PFLL "X  IS2:%016" PFLL "X  IS3:%016" PFLL "X\n"
         "    IS4:%016" PFLL "X  IS5:%016" PFLL "X  IS6:%016" PFLL "X  IS7:%016" PFLL "X\n"
         "    IS8:%016" PFLL "X  IK0:%016" PFLL "X  IK1:%016" PFLL "X\n"
         "    IGp:%016" PFLL "X  ISp:%016" PFLL "X  IRa:%016" PFLL "X\n"
         "    Fsr:%016" PFLL "X  Fir:%016" PFLL "X  Psr:%016" PFLL "X\n",
         ( ZH_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
         pExceptionInfo->ExceptionRecord->ExceptionAddress,
         pCtx->IntZero, pCtx->IntAt, pCtx->IntLo, pCtx->IntHi,
         pCtx->IntA0, pCtx->IntA1, pCtx->IntA2, pCtx->IntA3,
         pCtx->IntT0, pCtx->IntT1, pCtx->IntT2, pCtx->IntT3,
         pCtx->IntT4, pCtx->IntT5, pCtx->IntT6, pCtx->IntT7,
         pCtx->IntT8, pCtx->IntT9, pCtx->IntV0, pCtx->IntV1,
         pCtx->IntS0, pCtx->IntS1, pCtx->IntS2, pCtx->IntS3,
         pCtx->IntS4, pCtx->IntS5, pCtx->IntS6, pCtx->IntS7,
         pCtx->IntS8, pCtx->IntK0, pCtx->IntK1,
         pCtx->IntGp, pCtx->IntSp, pCtx->IntRa,
         pCtx->Fsr  , pCtx->Fir  , pCtx->Psr );
   }
#elif defined( ZH_OS_WIN_CE ) && defined( ZH_CPU_SH )
   {
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;

      zh_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X\n"
         "    Exception Address:%08X\n"
         "    R0 :%08X  R1 :%08X  R2 :%08X  R3 :%08X\n"
         "    R4 :%08X  R5 :%08X  R6 :%08X  R7 :%08X\n"
         "    R8 :%08X  R9 :%08X  R10:%08X  R11:%08X\n"
         "    R12:%08X  R13:%08X  R14:%08X  R15:%08X\n"
         "    PR :%08X MACH:%08X MACL:%08X  GBR:%08X\n",
         ( ZH_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
         ( ZH_U32 ) pExceptionInfo->ExceptionRecord->ExceptionAddress,
         ( ZH_U32 ) pCtx->R0 , ( ZH_U32 ) pCtx->R1 , ( ZH_U32 ) pCtx->R2 , ( ZH_U32 ) pCtx->R3 ,
         ( ZH_U32 ) pCtx->R4 , ( ZH_U32 ) pCtx->R5 , ( ZH_U32 ) pCtx->R6 , ( ZH_U32 ) pCtx->R7 ,
         ( ZH_U32 ) pCtx->R8 , ( ZH_U32 ) pCtx->R9 , ( ZH_U32 ) pCtx->R10, ( ZH_U32 ) pCtx->R11,
         ( ZH_U32 ) pCtx->R12, ( ZH_U32 ) pCtx->R13, ( ZH_U32 ) pCtx->R14, ( ZH_U32 ) pCtx->R15,
         ( ZH_U32 ) pCtx->PR, ( ZH_U32 ) pCtx->MACH, ( ZH_U32 ) pCtx->MACL, ( ZH_U32 ) pCtx->GBR );
   }
#elif defined( ZH_CPU_X86 )
   {
      char         buf[ 64 + MAX_PATH ];
      PCONTEXT     pCtx = pExceptionInfo->ContextRecord;
      const char * szCode;

      /* two most common codes */
      switch( pExceptionInfo->ExceptionRecord->ExceptionCode )
      {
         case EXCEPTION_ACCESS_VIOLATION:
            szCode = " " "ACCESS_VIOLATION";
            break;
         case EXCEPTION_IN_PAGE_ERROR:
            szCode = " " "IN_PAGE_ERROR";
            break;
         default:
            szCode = "";
      }

      zh_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X%s\n"
         "    Exception Address:%08X\n"
         "    EAX:%08X  EBX:%08X  ECX:%08X  EDX:%08X\n"
         "    ESI:%08X  EDI:%08X  EBP:%08X\n"
         "    CS:EIP:%04X:%08X  SS:ESP:%04X:%08X\n"
         "    DS:%04X  ES:%04X  FS:%04X  GS:%04X\n"
         "    Flags:%08X\n",
         ( ZH_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode, szCode,
         ( ZH_U32 ) pExceptionInfo->ExceptionRecord->ExceptionAddress,
         ( ZH_U32 ) pCtx->Eax, ( ZH_U32 ) pCtx->Ebx, ( ZH_U32 ) pCtx->Ecx, ( ZH_U32 ) pCtx->Edx,
         ( ZH_U32 ) pCtx->Esi, ( ZH_U32 ) pCtx->Edi, ( ZH_U32 ) pCtx->Ebp,
         ( ZH_U32 ) pCtx->SegCs, ( ZH_U32 ) pCtx->Eip, ( ZH_U32 ) pCtx->SegSs, ( ZH_U32 ) pCtx->Esp,
         ( ZH_U32 ) pCtx->SegDs, ( ZH_U32 ) pCtx->SegEs, ( ZH_U32 ) pCtx->SegFs, ( ZH_U32 ) pCtx->SegGs,
         ( ZH_U32 ) pCtx->EFlags );

      if( pExceptionInfo->ExceptionRecord->NumberParameters &&
          pExceptionInfo->ExceptionRecord->NumberParameters < ( DWORD ) EXCEPTION_MAXIMUM_PARAMETERS )
      {
         DWORD arg;

         zh_strncat( errmsg, "    Exception Parameters:", errmsglen );
         for( arg = 0; arg < pExceptionInfo->ExceptionRecord->NumberParameters; ++arg )
         {
            zh_snprintf( buf, sizeof( buf ), " %08X", ( ZH_U32 ) pExceptionInfo->ExceptionRecord->ExceptionInformation[ arg ] );
            zh_strncat( errmsg, buf, errmsglen );
         }
         zh_strncat( errmsg, "\n", errmsglen );
      }

      {
         unsigned char * pc;
         unsigned int *  sc;
         unsigned int *  ebp;
         unsigned int    eip;
         unsigned int    j;
         int             i;

         zh_strncat( errmsg, "    CS:EIP:", errmsglen );
         pc = ( unsigned char * ) pCtx->Eip;
         for( i = 0; i < 16; i++ )
         {
            /* FIXME: Unsafe function. */
            if( IsBadReadPtr( pc, 1 ) )
               break;
            zh_snprintf( buf, sizeof( buf ), " %02X", ( int ) pc[ i ] );
            zh_strncat( errmsg, buf, errmsglen );
         }
         zh_strncat( errmsg, "\n    SS:ESP:", errmsglen );
         sc = ( unsigned int * ) pCtx->Esp;
         for( i = 0; i < 16; i++ )
         {
            /* FIXME: Unsafe function. */
            if( IsBadReadPtr( sc, 4 ) )
               break;
            zh_snprintf( buf, sizeof( buf ), " %08X", sc[ i ] );
            zh_strncat( errmsg, buf, errmsglen );
         }
         zh_strncat( errmsg, "\n\n", errmsglen );
         zh_strncat( errmsg, "    C stack:\n", errmsglen );
         zh_strncat( errmsg, "    EIP:     EBP:       Frame: OldEBP, RetAddr, Params...\n", errmsglen );
         eip = pCtx->Eip;
         ebp = ( unsigned int * ) pCtx->Ebp;
         /* FIXME: Unsafe function. */
         if( ! IsBadWritePtr( ebp, 8 ) )
         {
            for( i = 0; i < 20; i++ )
            {
               /* FIXME: Unsafe function. */
               if( ( unsigned int ) ebp % 4 != 0 || IsBadWritePtr( ebp, 40 ) || ( unsigned int ) ebp >= ebp[ 0 ] )
                  break;
               zh_snprintf( buf, sizeof( buf ), "    %08X %08X  ", ( int ) eip, ( int ) ebp );
               zh_strncat( errmsg, buf, errmsglen );
               for( j = 0; j < 10 && ( unsigned int ) ( ebp + j ) < ebp[ 0 ]; j++ )
               {
                  zh_snprintf( buf, sizeof( buf ), " %08X", ebp[ j ] );
                  zh_strncat( errmsg, buf, errmsglen );
               }
               zh_strncat( errmsg, "\n", errmsglen );
               eip = ebp[ 1 ];
               ebp = ( unsigned int * ) ebp[ 0 ];
            }
            zh_strncat( errmsg, "\n", errmsglen );
         }
      }
   }
#endif

   {
#if defined( ZH_OS_WIN_CE )
      HMODULE hToolhelp = GetModuleHandle( TEXT( "toolhelp.dll" ) );
#else
      /* NOTE: Several non-MS sources say that Win9x has these functions
               in tlhelp32.dll. Testing shows though, that in Win95, Win95b
               and Win98 they are in kernel32.dll, and tlhelp32.dll doesn't
               exist. [vszakats] */
      HMODULE hToolhelp = GetModuleHandle( TEXT( "kernel32.dll" ) );
#endif

      if( hToolhelp )
      {
         /* NOTE: Hack to force the ASCII versions of these types. [vszakats] */
         #if ! defined( ZH_OS_WIN_CE ) && defined( UNICODE )
            #undef MODULEENTRY32
            #undef LPMODULEENTRY32
         #endif

         typedef HANDLE ( WINAPI * P_CTH32SSH )( DWORD, DWORD ); /* CreateToolhelp32Snapshot() */
         typedef BOOL ( WINAPI * P_M32F )( HANDLE, LPMODULEENTRY32 ); /* Module32First() */
         typedef BOOL ( WINAPI * P_M32N )( HANDLE, LPMODULEENTRY32 ); /* Module32Next() */

         P_CTH32SSH pCreateToolhelp32Snapshot = ( P_CTH32SSH ) ZH_WINAPI_GETPROCADDRESS( hToolhelp, "CreateToolhelp32Snapshot" );
         P_M32F     pModule32First            = ( P_M32F     ) ZH_WINAPI_GETPROCADDRESS( hToolhelp, "Module32First" );
         P_M32N     pModule32Next             = ( P_M32N     ) ZH_WINAPI_GETPROCADDRESS( hToolhelp, "Module32Next" );

         if( pCreateToolhelp32Snapshot &&
             pModule32First &&
             pModule32Next )
         {
            /* Take a snapshot of all modules in the specified process. */
            HANDLE hModuleSnap = pCreateToolhelp32Snapshot( TH32CS_SNAPMODULE | TH32CS_SNAPMODULE32, GetCurrentProcessId() );

            if( hModuleSnap != INVALID_HANDLE_VALUE )
            {
               MODULEENTRY32 me32;

               /* Set the size of the structure before using it. */
               me32.dwSize = sizeof( MODULEENTRY32 );

               /* Retrieve information about the first module, and exit if unsuccessful */
               if( pModule32First( hModuleSnap, &me32 ) )
               {
                  zh_strncat( errmsg, "\nModules:\n", errmsglen );

                  /* Now walk the module list of the process, and display information about each module */
                  do
                  {
                     char buf[ 256 ];
#if defined( ZH_OS_WIN_64 )
                     /* FIXME: me32.szExePath seemed trashed in some (standalone) tests. */
                     zh_snprintf( buf, sizeof( buf ), "%016" PFLL "X %016" PFLL "X %s\n", ( ZH_PTRUINT ) me32.modBaseAddr, ( ZH_PTRUINT ) me32.modBaseSize, me32.szExePath );
#else
                     char szBuffer[ MAX_PATH ];
                     #if defined( ZH_OS_WIN_CE )
                        zh_wcntombcpy( szBuffer, me32.szExePath, ZH_SIZEOFARRAY( szBuffer ) - 1 );
                     #else
                        zh_strncpy( szBuffer, me32.szExePath, ZH_SIZEOFARRAY( szBuffer ) - 1 );
                     #endif
                     zh_snprintf( buf, sizeof( buf ), "%08lX %08lX %s\n", ( ZH_PTRUINT ) me32.modBaseAddr, ( ZH_PTRUINT ) me32.modBaseSize, szBuffer );
#endif
                     zh_strncat( errmsg, buf, errmsglen );
                  }
                  while( pModule32Next( hModuleSnap, &me32 ) );
               }

               /* Do not forget to clean up the snapshot object. */
               CloseHandle( hModuleSnap );
            }
         }
      }
   }

   zh_errInternalRaw( 6005, "Exception error:%s", errmsg, NULL );

   return zh_cmdargCheck( "BATCH" ) ? EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH;
}


#elif defined( ZH_SIGNAL_EXCEPTION_HANDLER )

static void zh_signalExceptionHandler( int sig, siginfo_t * si, void * ucp )
{
   char buffer[ 32 ];
   const char * signame;
   const char * sigaddr;

   ZH_SYMBOL_UNUSED( ucp );

   switch( sig )
   {
      case SIGSEGV:
         signame = "SIGSEGV";
         zh_snprintf( buffer, sizeof( buffer ), "%p", si->si_addr );
         sigaddr = buffer;
         break;
      case SIGILL:
         signame = "SIGILL";
         zh_snprintf( buffer, sizeof( buffer ), "%p", si->si_addr );
         sigaddr = buffer;
         break;
      case SIGFPE:
         signame = "SIGFPE";
         zh_snprintf( buffer, sizeof( buffer ), "%p", si->si_addr );
         sigaddr = buffer;
         break;
      case SIGBUS:
         signame = "SIGBUS";
         zh_snprintf( buffer, sizeof( buffer ), "%p", si->si_addr );
         sigaddr = buffer;
         break;
      default:
         zh_snprintf( buffer, sizeof( buffer ), "sig:%d", sig );
         signame = buffer;
         sigaddr = "UNKNOWN";
         break;
   }

   zh_errInternal( 6005, "Exception %s at address %s", signame, sigaddr );
}

#endif

void zh_vmSetExceptionHandler( void )
{
#if defined( ZH_OS_WIN )
   {
      LPTOP_LEVEL_EXCEPTION_FILTER ef = SetUnhandledExceptionFilter( zh_winExceptionHandler );
      ZH_SYMBOL_UNUSED( ef );
   }
#elif defined( ZH_SIGNAL_EXCEPTION_HANDLER )
   {
      stack_t ss;
      ss.ss_sp = ( void * ) s_signal_stack;
      ss.ss_size = SIGSTKSZ;
      ss.ss_flags = 0;
      /* set alternative stack for SIGSEGV executed on stack overflow */
      if( sigaltstack( &ss, NULL ) == 0 )
      {
         struct sigaction act;
         int i, sigs[] = { SIGSEGV, SIGILL, SIGFPE, SIGBUS, 0 };

         /* Ignore SIGPIPEs so they don't kill us. */
         signal( SIGPIPE, SIG_IGN );
         for( i = 0; sigs[ i ]; ++i )
         {
            sigaction( sigs[ i ], 0, &act );
            act.sa_sigaction = zh_signalExceptionHandler;
            act.sa_flags = SA_ONSTACK | SA_SIGINFO | SA_RESETHAND;
            sigaction( sigs[ i ], &act, 0 );
         }
      }
   }
#endif
}

void zh_vmUnsetExceptionHandler( void )
{
#if defined( ZH_SIGNAL_EXCEPTION_HANDLER )
   {
   }
#endif
}
