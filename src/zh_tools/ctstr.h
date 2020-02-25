/*
 * Internal function header for CT3 string functions
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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


#ifndef _CTSTR_H
#define _CTSTR_H  1

ZH_EXTERN_BEGIN

extern const char * ct_at_exact_forward( const char * pcString, ZH_SIZE sStrLen,
                                         const char * pcMatch, ZH_SIZE sMatchLen,
                                         ZH_SIZE * psMatchStrLen );
extern const char * ct_at_exact_backward( const char * pcString, ZH_SIZE sStrLen,
                                          const char * pcMatch, ZH_SIZE sMatchLen,
                                          ZH_SIZE * psMatchStrLen );
extern const char * ct_at_wildcard_forward( const char * pcString, ZH_SIZE sStrLen,
                                            const char * pcMatch, ZH_SIZE sMatchLen,
                                            char cWildCard, ZH_SIZE * psMatchStrLen );
extern const char * ct_at_wildcard_backward( const char * pcString, ZH_SIZE sStrLen,
                                             const char * pcMatch, ZH_SIZE sMatchLen,
                                             char cWildCard, ZH_SIZE * psMatchStrLen );
extern const char * ct_at_charset_forward( const char * pcString, ZH_SIZE sStrLen,
                                           const char * pcCharSet, ZH_SIZE sCharSetLen,
                                           ZH_SIZE * psMatchedCharPos );
extern const char * ct_at_charset_backward( const char * pcString, ZH_SIZE sStrLen,
                                            const char * pcCharSet, ZH_SIZE sCharSetLen,
                                            ZH_SIZE * psMatchedCharPos );

extern void ct_setref( int iNewSwitch );
extern int  ct_getref( void );
extern void ct_setatmupa( int iNewSwitch );
extern int  ct_getatmupa( void );
extern void ct_setatlike( int iNewSwitch );
extern int  ct_getatlike( void );
extern void ct_setatlikechar( char cNewChar );
extern char ct_getatlikechar( void );

#define CT_SETATLIKE_EXACT     0
#define CT_SETATLIKE_WILDCARD  1

ZH_EXTERN_END

#endif
