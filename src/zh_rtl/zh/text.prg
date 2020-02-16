/*
 * __TextSave()/__TextRestore() functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

THREAD STATIC t_cFile
THREAD STATIC t_lOldPrinter
THREAD STATIC t_lOldExtra
THREAD STATIC t_cOldExtraFile

PROCEDURE __TextSave( cFile )

   t_cFile := cFile

   IF t_cFile == "PRINTER"
      t_lOldPrinter := Set( _SET_PRINTER, .T. )
   ELSE
      t_lOldExtra := Set( _SET_EXTRA, .T. )
      t_cOldExtraFile := Set( _SET_EXTRAFILE, cFile )
   ENDIF

   RETURN

PROCEDURE __TextRestore()

   IF t_cFile == "PRINTER"
      Set( _SET_PRINTER, t_lOldPrinter )
   ELSE
      Set( _SET_EXTRAFILE, t_cOldExtraFile )
      Set( _SET_EXTRA, t_lOldExtra )
   ENDIF

   RETURN
