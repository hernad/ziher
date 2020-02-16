/*
 * Inkey GT API
 *
 * Copyright 2007 Przemyslaw Czerpak
 * Copyright 2002 Walter Negro <anegro@overnet.com.ar> (zh_inkeySetLast())
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
#include "zh_gt_core.h"

/* xZiher compatible definitions */
#if !defined( K_SH_LEFT )
#define K_SH_LEFT           K_LEFT            /* Shift-Left  == Left  */
#define K_SH_UP             K_UP              /* Shift-Up    == Up    */
#define K_SH_RIGHT          K_RIGHT           /* Shift-Right == Right */
#define K_SH_DOWN           K_DOWN            /* Shift-Down  == Down  */
#define K_SH_INS            K_INS             /* Shift-Ins   == Ins   */
#define K_SH_DEL            K_DEL             /* Shift-Del   == Del   */
#define K_SH_HOME           K_HOME            /* Shift-Home  == Home  */
#define K_SH_END            K_END             /* Shift-End   == End   */
#define K_SH_PGUP           K_PGUP            /* Shift-PgUp  == PgUp  */
#define K_SH_PGDN           K_PGDN            /* Shift-PgDn  == PgDn  */
#define K_SH_RETURN         K_RETURN          /* Shift-Enter == Enter */
#define K_SH_ENTER          K_ENTER           /* Shift-Enter == Enter */
#endif

typedef struct
{
   short key;
   short alt_key;
   short ctrl_key;
   short shift_key;
} ZH_KEY_VALUE;

static const int s_keyPadCtrl[] = {
   K_CTRL_INS, K_CTRL_END, K_CTRL_DOWN, K_CTRL_PGDN, K_CTRL_LEFT,
   KP_CTRL_5, K_CTRL_RIGHT, K_CTRL_HOME, K_CTRL_UP, K_CTRL_PGUP
};

static const ZH_KEY_VALUE s_transKeyFun[] = {
   { K_F1,      K_ALT_F1,     K_CTRL_F1,     K_SH_F1    }, /* 01 */
   { K_F2,      K_ALT_F2,     K_CTRL_F2,     K_SH_F2    }, /* 02 */
   { K_F3,      K_ALT_F3,     K_CTRL_F3,     K_SH_F3    }, /* 03 */
   { K_F4,      K_ALT_F4,     K_CTRL_F4,     K_SH_F4    }, /* 04 */
   { K_F5,      K_ALT_F5,     K_CTRL_F5,     K_SH_F5    }, /* 05 */
   { K_F6,      K_ALT_F6,     K_CTRL_F6,     K_SH_F6    }, /* 06 */
   { K_F7,      K_ALT_F7,     K_CTRL_F7,     K_SH_F7    }, /* 07 */
   { K_F8,      K_ALT_F8,     K_CTRL_F8,     K_SH_F8    }, /* 08 */
   { K_F9,      K_ALT_F9,     K_CTRL_F9,     K_SH_F9    }, /* 09 */
   { K_F10,     K_ALT_F10,    K_CTRL_F10,    K_SH_F10   }, /* 10 */
   { K_F11,     K_ALT_F11,    K_CTRL_F11,    K_SH_F11   }, /* 11 */
   { K_F12,     K_ALT_F12,    K_CTRL_F12,    K_SH_F12   }, /* 12 */
   { K_UP,      K_ALT_UP,     K_CTRL_UP,     K_SH_UP    }, /* 13 */
   { K_DOWN,    K_ALT_DOWN,   K_CTRL_DOWN,   K_SH_DOWN  }, /* 14 */
   { K_LEFT,    K_ALT_LEFT,   K_CTRL_LEFT,   K_SH_LEFT  }, /* 15 */
   { K_RIGHT,   K_ALT_RIGHT,  K_CTRL_RIGHT,  K_SH_RIGHT }, /* 16 */
   { K_HOME,    K_ALT_HOME,   K_CTRL_HOME,   K_SH_HOME  }, /* 17 */
   { K_END,     K_ALT_END,    K_CTRL_END,    K_SH_END   }, /* 18 */
   { K_PGUP,    K_ALT_PGUP,   K_CTRL_PGUP,   K_SH_PGUP  }, /* 19 */
   { K_PGDN,    K_ALT_PGDN,   K_CTRL_PGDN,   K_SH_PGDN  }, /* 20 */
   { K_INS,     K_ALT_INS,    K_CTRL_INS,    K_SH_INS   }, /* 21 */
   { K_DEL,     K_ALT_DEL,    K_CTRL_DEL,    K_SH_DEL   }, /* 22 */
   { K_BS,      K_ALT_BS,     K_CTRL_BS,     K_SH_BS    }, /* 23 */
   { K_TAB,     K_ALT_TAB,    K_CTRL_TAB,    K_SH_TAB   }, /* 24 */
   { K_ESC,     K_ALT_ESC,    K_ESC,         0          }, /* 25 */
   { K_ENTER,   K_ALT_ENTER,  K_CTRL_ENTER,  K_SH_ENTER }, /* 26 */
   { KP_CENTER, 0,            KP_CTRL_5,     '5'        }, /* 27 */
   { 0,         0,            K_CTRL_PRTSCR, 0          }, /* 28 */
   { 0,         0,            ZH_BREAK_FLAG, 0          }  /* 29 */
};

static const ZH_KEY_VALUE s_transKeyStd[] = {
   { K_SPACE,   0,               0,               0 },   /* 32 */
   { '!',       0,               0,               0 },   /* 33 */
   { '"',       0,               0,               0 },   /* 34 */
   { '#',       0,               0,               0 },   /* 35 */
   { '$',       0,               0,               0 },   /* 36 */
   { '%',       0,               0,               0 },   /* 37 */
   { '&',       0,               0,               0 },   /* 38 */
   { '\'',      K_ALT_QUOTE,     7,               0 },   /* 39 */
   { '(',       0,               0,               0 },   /* 40 */
   { ')',       0,               0,               0 },   /* 41 */
   { '*',       0,               0,               0 },   /* 42 */
   { '+',       0,               0,               0 },   /* 43 */
   { ',',       K_ALT_COMMA,     0,               0 },   /* 44 */
   { '-',       K_ALT_MINUS,     31,              0 },   /* 45 */
   { '.',       K_ALT_PERIOD,    0,               0 },   /* 46 */
   { '/',       K_CTRL_QUESTION, K_CTRL_BS,       0 },   /* 47 */
   { '0',       K_ALT_0,         0,               0 },   /* 48 */
   { '1',       K_ALT_1,         0,               0 },   /* 49 */
   { '2',       K_ALT_2,         259,             0 },   /* 50 */
   { '3',       K_ALT_3,         27,              0 },   /* 51 */
   { '4',       K_ALT_4,         28,              0 },   /* 52 */
   { '5',       K_ALT_5,         29,              0 },   /* 53 */
   { '6',       K_ALT_6,         30,              0 },   /* 54 */
   { '7',       K_ALT_7,         31,              0 },   /* 55 */
   { '8',       K_ALT_8,         127,             0 },   /* 56 */
   { '9',       K_ALT_9,         0,               0 },   /* 57 */
   { ':',       0,               0,               0 },   /* 58 */
   { ';',       K_ALT_SC,        0,               0 },   /* 59 */
   { '<',       0,               0,               0 },   /* 60 */
   { '=',       K_ALT_EQUALS,    0,               0 },   /* 61 */
   { '>',       0,               0,               0 },   /* 62 */
   { '?',       0,               K_CTRL_QUESTION, 0 },   /* 63 */
   { '@',       0,               259,             0 },   /* 64 */
   { 'A',       K_ALT_A,         K_CTRL_A,        0 },   /* 65 */
   { 'B',       K_ALT_B,         K_CTRL_B,        0 },   /* 66 */
   { 'C',       K_ALT_C,         K_CTRL_C,        0 },   /* 67 */
   { 'D',       K_ALT_D,         K_CTRL_D,        0 },   /* 68 */
   { 'E',       K_ALT_E,         K_CTRL_E,        0 },   /* 69 */
   { 'F',       K_ALT_F,         K_CTRL_F,        0 },   /* 70 */
   { 'G',       K_ALT_G,         K_CTRL_G,        0 },   /* 71 */
   { 'H',       K_ALT_H,         K_CTRL_H,        0 },   /* 72 */
   { 'I',       K_ALT_I,         K_CTRL_I,        0 },   /* 73 */
   { 'J',       K_ALT_J,         K_CTRL_J,        0 },   /* 74 */
   { 'K',       K_ALT_K,         K_CTRL_K,        0 },   /* 75 */
   { 'L',       K_ALT_L,         K_CTRL_L,        0 },   /* 76 */
   { 'M',       K_ALT_M,         K_CTRL_M,        0 },   /* 77 */
   { 'N',       K_ALT_N,         K_CTRL_N,        0 },   /* 78 */
   { 'O',       K_ALT_O,         K_CTRL_O,        0 },   /* 79 */
   { 'P',       K_ALT_P,         K_CTRL_P,        0 },   /* 80 */
   { 'Q',       K_ALT_Q,         K_CTRL_Q,        0 },   /* 81 */
   { 'R',       K_ALT_R,         K_CTRL_R,        0 },   /* 82 */
   { 'S',       K_ALT_S,         K_CTRL_S,        0 },   /* 83 */
   { 'T',       K_ALT_T,         K_CTRL_T,        0 },   /* 84 */
   { 'U',       K_ALT_U,         K_CTRL_U,        0 },   /* 85 */
   { 'V',       K_ALT_V,         K_CTRL_V,        0 },   /* 86 */
   { 'W',       K_ALT_W,         K_CTRL_W,        0 },   /* 87 */
   { 'X',       K_ALT_X,         K_CTRL_X,        0 },   /* 88 */
   { 'Y',       K_ALT_Y,         K_CTRL_Y,        0 },   /* 89 */
   { 'Z',       K_ALT_Z,         K_CTRL_Z,        0 },   /* 90 */
   { '[',       K_ALT_OSB,       27,              0 },   /* 91 */
   { '\\',      K_ALT_BACKSLASH, 28,              0 },   /* 92 */
   { ']',       K_ALT_CSB,       29,              0 },   /* 93 */
   { '^',       K_ALT_6,         30,              0 },   /* 94 */
   { '_',       K_ALT_MINUS,     31,              0 },   /* 95 */
   { '`',       K_ALT_BACKQUOTE, K_ALT_BACKQUOTE, 0 },   /* 96 */
   { 'a',       K_ALT_A,         K_CTRL_A,        0 },   /* 97 */
   { 'b',       K_ALT_B,         K_CTRL_B,        0 },   /* 98 */
   { 'c',       K_ALT_C,         K_CTRL_C,        0 },   /* 99 */
   { 'd',       K_ALT_D,         K_CTRL_D,        0 },   /* 100 */
   { 'e',       K_ALT_E,         K_CTRL_E,        0 },   /* 101 */
   { 'f',       K_ALT_F,         K_CTRL_F,        0 },   /* 102 */
   { 'g',       K_ALT_G,         K_CTRL_G,        0 },   /* 103 */
   { 'h',       K_ALT_H,         K_CTRL_H,        0 },   /* 104 */
   { 'i',       K_ALT_I,         K_CTRL_I,        0 },   /* 105 */
   { 'j',       K_ALT_J,         K_CTRL_J,        0 },   /* 106 */
   { 'k',       K_ALT_K,         K_CTRL_K,        0 },   /* 107 */
   { 'l',       K_ALT_L,         K_CTRL_L,        0 },   /* 108 */
   { 'm',       K_ALT_M,         K_CTRL_M,        0 },   /* 109 */
   { 'n',       K_ALT_N,         K_CTRL_N,        0 },   /* 110 */
   { 'o',       K_ALT_O,         K_CTRL_O,        0 },   /* 111 */
   { 'p',       K_ALT_P,         K_CTRL_P,        0 },   /* 112 */
   { 'q',       K_ALT_Q,         K_CTRL_Q,        0 },   /* 113 */
   { 'r',       K_ALT_R,         K_CTRL_R,        0 },   /* 114 */
   { 's',       K_ALT_S,         K_CTRL_S,        0 },   /* 115 */
   { 't',       K_ALT_T,         K_CTRL_T,        0 },   /* 116 */
   { 'u',       K_ALT_U,         K_CTRL_U,        0 },   /* 117 */
   { 'v',       K_ALT_V,         K_CTRL_V,        0 },   /* 118 */
   { 'w',       K_ALT_W,         K_CTRL_W,        0 },   /* 119 */
   { 'x',       K_ALT_X,         K_CTRL_X,        0 },   /* 120 */
   { 'y',       K_ALT_Y,         K_CTRL_Y,        0 },   /* 121 */
   { 'z',       K_ALT_Z,         K_CTRL_Z,        0 },   /* 122 */
   { '{',       K_ALT_OSB,       27,              0 },   /* 123 */
   { '|',       K_ALT_BACKSLASH, 28,              0 },   /* 124 */
   { '}',       K_ALT_CSB,       29,              0 },   /* 125 */
   { '~',       K_ALT_BACKQUOTE, K_ALT_BACKQUOTE, 0 },   /* 126 */
   { K_CTRL_BS, K_ALT_BS,        K_CTRL_BS,       0 },   /* 127 */
};

int zh_inkey( ZH_BOOL fWait, double dSeconds, int iEventMask )
{
   int iKey = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkey(%d, %f, %d)", ( int ) fWait, dSeconds, iEventMask ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iKey = ZH_GTSELF_INKEYGET( pGT, fWait, dSeconds, iEventMask );
      zh_gt_BaseFree( pGT );
   }
   return iKey;
}

void zh_inkeyPut( int iKey )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeyPut(%d)", iKey ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_INKEYPUT( pGT, iKey );
      zh_gt_BaseFree( pGT );
   }
}

void zh_inkeyIns( int iKey )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeyIns(%d)", iKey ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_INKEYINS( pGT, iKey );
      zh_gt_BaseFree( pGT );
   }
}

int zh_inkeyLast( int iEventMask )
{
   int iKey = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeyLast(%d)", iEventMask ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iKey = ZH_GTSELF_INKEYLAST( pGT, iEventMask );
      zh_gt_BaseFree( pGT );
   }
   return iKey;
}

int zh_inkeyNext( int iEventMask )
{
   int iKey = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeyNext(%d)", iEventMask ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iKey = ZH_GTSELF_INKEYNEXT( pGT, iEventMask );
      zh_gt_BaseFree( pGT );
   }
   return iKey;
}

void zh_inkeyPoll( void )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeyPoll()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_INKEYPOLL( pGT );
      zh_gt_BaseFree( pGT );
   }
}

int zh_inkeySetLast( int iKey )
{
   int iLast = 0;
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeySetLast(%d)", iKey ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      iLast = ZH_GTSELF_INKEYSETLAST( pGT, iKey );
      zh_gt_BaseFree( pGT );
   }
   return iLast;
}

void zh_inkeySetText( const char * szText, ZH_SIZE nLen )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeySetText(%s,%" ZH_PFS "u)", szText, nLen ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_INKEYSETTEXT( pGT, szText, nLen );
      zh_gt_BaseFree( pGT );
   }
}

void zh_inkeyReset( void )
{
   PZH_GT pGT;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeyReset()" ) );

   pGT = zh_gt_Base();
   if( pGT )
   {
      ZH_GTSELF_INKEYRESET( pGT );
      zh_gt_BaseFree( pGT );
   }
}

static int s_inkeyTransChar( int iKey, int iFlags, const ZH_KEY_VALUE * pKeyVal )
{
   if( ( iFlags & ZH_KF_KEYPAD ) != 0 &&
       ( iFlags & ( ZH_KF_ALT | ZH_KF_CTRL ) ) != 0 )
   {
      switch( iKey )
      {
         case ZH_KX_ENTER:
            if( iFlags & ZH_KF_ALT )
               return KP_ALT_ENTER;
            break;
         case '+':
            return ( iFlags & ZH_KF_ALT ) ? KP_ALT_PLUS : KP_CTRL_PLUS;
         case '-':
            return ( iFlags & ZH_KF_ALT ) ? KP_ALT_MINUS : KP_CTRL_MINUS;
         case '*':
            return ( iFlags & ZH_KF_ALT ) ? KP_ALT_ASTERISK : KP_CTRL_ASTERISK;
         case '/':
            return ( iFlags & ZH_KF_ALT ) ? KP_ALT_SLASH : KP_CTRL_SLASH;
         case '.':
         case ',':
            if( iFlags & ZH_KF_CTRL )
               return K_CTRL_DEL;
            break;
         case '0':
         case '1':
         case '2':
         case '3':
         case '4':
         case '5':
         case '6':
         case '7':
         case '8':
         case '9':
            if( iFlags & ZH_KF_CTRL )
               return s_keyPadCtrl[ iKey - '0' ];
            break;
      }
   }

   if( ( iFlags & ZH_KF_ALT ) != 0 && pKeyVal->alt_key )
      return pKeyVal->alt_key;
   else if( ( iFlags & ZH_KF_CTRL ) != 0 && ( pKeyVal->ctrl_key || pKeyVal->key == '@' ) )
      return pKeyVal->ctrl_key;
   else if( ( iFlags & ZH_KF_SHIFT ) != 0 && pKeyVal->shift_key )
      return pKeyVal->shift_key;
   else
      return pKeyVal->key;
}

ZH_SIZE zh_inkeyKeyString( int iKey, char * buffer, ZH_SIZE nSize )
{
   ZH_SIZE nLen = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeyKeyString(%d,%p, %" ZH_PFS "u)", iKey, ( void * ) buffer, nSize ) );

   if( ZH_INKEY_ISUNICODE( iKey ) )
   {
      nLen = zh_cdpTextPutU16( zh_vmCDP(), buffer, nSize, ZH_INKEY_VALUE( iKey ) );
   }
   else
   {
      if( ZH_INKEY_ISCHAR( iKey ) )
         iKey = ZH_INKEY_VALUE( iKey );
      else if( ZH_INKEY_ISKEY( iKey ) )
      {
         int iFlags = ZH_INKEY_FLAGS( iKey );

         iKey = ZH_INKEY_VALUE( iKey );

         if( iKey > 0 && iKey <= ( int ) ZH_SIZEOFARRAY( s_transKeyFun ) )
            iKey = s_inkeyTransChar( iKey, iFlags, &s_transKeyFun[ iKey - 1 ] );
         else if( iKey >= 32 && iKey <= 127 )
            iKey = s_inkeyTransChar( iKey, iFlags, &s_transKeyStd[ iKey - 32 ] );
      }
      if( iKey >= 32 && iKey <= 255 && iKey != 127 )
      {
         PZH_CODEPAGE cdp = zh_vmCDP();
         nLen = zh_cdpTextPutU16( cdp, buffer, nSize,
                                  zh_cdpGetU16( cdp, ( ZH_UCHAR ) iKey ) );
      }
   }
   return nLen;
}

int zh_inkeyKeyStd( int iKey )
{
   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeyKeyStd(%d)", iKey ) );

   if( ZH_INKEY_ISEXT( iKey ) )
   {
      int iFlags = ZH_INKEY_FLAGS( iKey );

      if( ZH_INKEY_ISMOUSEPOS( iKey ) )
         iKey = K_MOUSEMOVE;
      else if( ZH_INKEY_ISKEY( iKey ) )
      {
         iKey = ZH_INKEY_VALUE( iKey );

         if( iKey > 0 && iKey <= ( int ) ZH_SIZEOFARRAY( s_transKeyFun ) )
            iKey = s_inkeyTransChar( iKey, iFlags, &s_transKeyFun[ iKey - 1 ] );
         else if( iKey >= 32 && iKey <= 127 )
            iKey = s_inkeyTransChar( iKey, iFlags, &s_transKeyStd[ iKey - 32 ] );
      }
      else if( ZH_INKEY_ISCHAR( iKey ) || ZH_INKEY_ISUNICODE( iKey ) )
      {
         int iVal = ZH_INKEY_VALUE( iKey );

         if( iVal >= 32 && iVal <= 127 &&
             ( iFlags & ( ZH_KF_ALT | ZH_KF_CTRL ) ) != 0 )
            iKey = s_inkeyTransChar( iVal, iFlags, &s_transKeyStd[ iVal - 32 ] );
         else if( ZH_INKEY_ISUNICODE( iKey ) )
         {
            ZH_WCHAR wc = ( ZH_WCHAR ) iVal;
            if( wc )
            {
               ZH_UCHAR uc = zh_cdpGetUC( zh_vmCDP(), wc, 0 );
               if( uc != 0 )
                  iKey = uc;
            }
            else
               iKey = 0;
         }
         else
            iKey = iVal;
      }
      else /* ZH_INKEY_ISMOUSEKEY | ZH_INKEY_ISEVENT */
         iKey = ZH_INKEY_VALUE( iKey );
   }
   return iKey;
}

int zh_inkeyKeyMod( int iKey )
{
   int iFlags = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeyKeyMod(%d)", iKey ) );

   if( ZH_INKEY_ISEXT( iKey ) && ! ZH_INKEY_ISMOUSEPOS( iKey ) )
      iFlags = ZH_INKEY_FLAGS( iKey );

   return iFlags;
}

int zh_inkeyKeyVal( int iKey )
{
   int iValue = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeyKeyVal(%d)", iKey ) );

   if( ZH_INKEY_ISEXT( iKey ) && ! ZH_INKEY_ISMOUSEPOS( iKey ) )
      iValue = ZH_INKEY_VALUE( iKey );

   return iValue;
}

int zh_inkeyKeyExt( int iKey )
{
   int iValue = 0;

   ZH_TRACE( ZH_TR_DEBUG, ( "zh_inkeyKeyVal(%d)", iKey ) );

   if( ZH_INKEY_ISKEY( iKey ) )
   {
      iValue = ZH_INKEY_VALUE( iKey );
      if( iValue < 1 || iValue > ( int ) ZH_SIZEOFARRAY( s_transKeyFun ) )
         iValue = 0;
   }

   return iValue;
}
