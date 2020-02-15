/*
 * National Collation Support Module (template)
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
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

/*
   If accented characters need special sorting ( ZH_CDP_ACSORT_EQUAL or
   ZH_CDP_ACSORT_INTERLEAVE ) then you need to mark the accented characters
   with the symbol '~' before each of them, for example:
      a~Ç
   If there is two-character sequence, which is considered as one, it should
   be marked with '.' before and after it, for example:
      ... h.ch.i ...
   if such multibyte character has its own Unicode representation then
   this Unicode value can be set using '=' symbol, for example:
      ....h.ch=2A7C.i
   and it will be used in translations.

   The Upper case string and the Lower case string should use the same
   letters. If some characters does not have corresponding upper or lower
   letter then space ' ' can be used as dummy character, for example in
   German CPs there is no upper case 'scharfes S' letter so space is used
   as dummy character.

   otherwise 0
 */

#define ZH_CP_ID        EN
#define ZH_CP_INFO      "English CP-437"
#define ZH_CP_UNITB     ZH_UNITB_437
#define ZH_CP_ACSORT    ZH_CDP_ACSORT_NONE
#define ZH_CP_UPPER     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
#define ZH_CP_LOWER     "abcdefghijklmnopqrstuvwxyz"
#define ZH_CP_UTF8

/* include CP registration code */
#include "zh_codepage_reg.h"
