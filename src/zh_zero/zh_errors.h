/*
 * Header file for compiler error codes
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

#ifndef ZH_ERRORS_H_
#define ZH_ERRORS_H_

#include "zh_setup.h"

ZH_EXTERN_BEGIN

/*
 * Errors generated by Ziher compiler
 */
#define ZH_COMP_ERR_OUTSIDE                     1
#define ZH_COMP_ERR_FUNC_DUPL                   2
#define ZH_COMP_ERR_VAR_DUPL                    3
#define ZH_COMP_ERR_FOLLOWS_EXEC                4
#define ZH_COMP_ERR_OUTER_VAR                   5
#define ZH_COMP_ERR_NUMERIC_FORMAT              6
#define ZH_COMP_ERR_STRING_TERMINATOR           7
#define ZH_COMP_ERR_FUNC_RESERVED               8
#define ZH_COMP_ERR_ILLEGAL_INIT                9
#define ZH_COMP_ERR_ENDIF                       10
#define ZH_COMP_ERR_ENDDO                       11
#define ZH_COMP_ERR_ENDCASE                     12
#define ZH_COMP_ERR_NEXTFOR                     13
#define ZH_COMP_ERR_UNMATCHED_ELSE              14
#define ZH_COMP_ERR_UNMATCHED_ELSEIF            15
#define ZH_COMP_ERR_SYNTAX                      16
#define ZH_COMP_ERR_UNCLOSED_STRU               17
#define ZH_COMP_ERR_UNMATCHED_EXIT              18
#define ZH_COMP_ERR_SYNTAX2                     19
#define ZH_COMP_ERR_INCOMPLETE_STMT             20
#define ZH_COMP_ERR_CHECKING_ARGS               21
#define ZH_COMP_ERR_INVALID_LVALUE              22
#define ZH_COMP_ERR_INVALID_REFER               23
#define ZH_COMP_ERR_PARAMETERS_NOT_ALLOWED      24
#define ZH_COMP_ERR_EXIT_IN_SEQUENCE            25
#define ZH_COMP_ERR_UNTERM_ARRAY_INDEX          26
#define ZH_COMP_ERR_MEMALLOC                    27
#define ZH_COMP_ERR_MEMREALLOC                  28
#define ZH_COMP_ERR_MEMFREE                     29
#define ZH_COMP_ERR_YACC                        30
#define ZH_COMP_ERR_JUMP_TOO_LONG               31
#define ZH_COMP_ERR_CREATE_OUTPUT               32
#define ZH_COMP_ERR_CREATE_PPO                  33
#define ZH_COMP_ERR_BADOPTION                   34
#define ZH_COMP_ERR_BADPARAM                    35
#define ZH_COMP_ERR_BADFILENAME                 36
#define ZH_COMP_ERR_MAYHEM_IN_CASE              37
#define ZH_COMP_ERR_INVALID_TYPE                38
#define ZH_COMP_ERR_INVALID_ALIAS               39
#define ZH_COMP_ERR_INVALID_INDEX               40
#define ZH_COMP_ERR_INVALID_BOUND               41
#define ZH_COMP_ERR_BAD_MACRO                   42
#define ZH_COMP_ERR_INVALID_SEND                43
#define ZH_COMP_ERR_FUNC_ANNOUNCE               44
#define ZH_COMP_ERR_JUMP_NOT_FOUND              45
#define ZH_COMP_ERR_CASE                        46
#define ZH_COMP_ERR_BLOCK                       47
#define ZH_COMP_ERR_GET_COMPLEX_MACRO           48
#define ZH_COMP_ERR_INVALID_INLINE              49
#define ZH_COMP_ERR_TOOMANY_INLINE              50
#define ZH_COMP_ERR_REQUIRES_C                  51
#define ZH_COMP_ERR_OPTIMIZEDLOCAL_OUT_OF_RANGE 52
#define ZH_COMP_ERR_FORVAR_TOOMANY              53
#define ZH_COMP_ERR_FORVAR_DIFF                 54
#define ZH_COMP_ERR_NOT_LITERAL_CASE            55
#define ZH_COMP_ERR_INVALID_STR                 56
#define ZH_COMP_ERR_INVALID_DATE                57
#define ZH_COMP_ERR_INVALID_TIMESTAMP           58
#define ZH_COMP_ERR_MEMOVERFLOW                 59
#define ZH_COMP_ERR_MEMCORRUPT                  60
#define ZH_COMP_ERR_WITHOBJECT                  61
#define ZH_COMP_ERR_BUFFER_OVERFLOW             62
#define ZH_COMP_ERR_UNSUPPORTED_LANG            63
#define ZH_COMP_ERR_STRING_TOO_LONG             64
#define ZH_COMP_ERR_BLOCK_TOO_BIG               65
#define ZH_COMP_ERR_NOT_VPARAMS                 66
#define ZH_COMP_ERR_OPEN_CFG                    67
#define ZH_COMP_ERR_ALWAYS_AFTER_EXIT           68
#define ZH_COMP_ERR_FILE_WRITE                  69
#define ZH_COMP_ERR_DUPL_CASE                   70
#define ZH_COMP_ERR_ENDWITH                     71
#define ZH_COMP_ERR_ENDSWITCH                   72
#define ZH_COMP_ERR_ENDSEQ                      73
#define ZH_COMP_ERR_WITHOBJECT_MACROBLOCK       74
#define ZH_COMP_ERR_HISTORICAL_1                75
#define ZH_COMP_ERR_HISTORICAL_2                76
#define ZH_COMP_ERR_HISTORICAL_3                77
#define ZH_COMP_ERR_HISTORICAL_4                78

#define ZH_COMP_WARN_AMBIGUOUS_VAR              1
#define ZH_COMP_WARN_MEMVAR_ASSUMED             2
#define ZH_COMP_WARN_VAR_NOT_USED               3
#define ZH_COMP_WARN_BLOCKVAR_NOT_USED          4
#define ZH_COMP_WARN_NO_RETURN_VALUE            5
#define ZH_COMP_WARN_PROC_RETURN_VALUE          6
#define ZH_COMP_WARN_FUN_WITH_NO_RETURN         7
#define ZH_COMP_WARN_ASSIGN_TYPE                8
#define ZH_COMP_WARN_OPERAND_TYPE               9
#define ZH_COMP_WARN_OPERANDS_INCOMPATIBLE      10
#define ZH_COMP_WARN_ASSIGN_SUSPECT             11
#define ZH_COMP_WARN_OPERAND_SUSPECT            12
#define ZH_COMP_WARN_NOT_ARRAY                  13
#define ZH_COMP_WARN_RETURN_TYPE                14
#define ZH_COMP_WARN_RETURN_SUSPECT             15
#define ZH_COMP_WARN_PARAM_COUNT                16
#define ZH_COMP_WARN_PARAM_TYPE                 17
#define ZH_COMP_WARN_PARAM_SUSPECT              18
#define ZH_COMP_WARN_DUP_DECLARATION            19
#define ZH_COMP_WARN_DECLARATION_CONFLICT       20
#define ZH_COMP_WARN_NOT_INITIALIZED            21
#define ZH_COMP_WARN_VAL_NOT_USED               22
#define ZH_COMP_WARN_ARRAY_ASSIGN_TYPE          23
#define ZH_COMP_WARN_ARRAY_ASSIGN_SUSPECT       24
#define ZH_COMP_WARN_CLASS_NOT_FOUND            25
#define ZH_COMP_WARN_MESSAGE_NOT_FOUND          26
#define ZH_COMP_WARN_MEANINGLESS                27
#define ZH_COMP_WARN_UNREACHABLE                28
#define ZH_COMP_WARN_DUPL_ANNOUNCE              29
#define ZH_COMP_WARN_FORVAR_DUPL                30
#define ZH_COMP_WARN_ENUM_INVALID               31
#define ZH_COMP_WARN_ASSIGNED_UNUSED            32
#define ZH_COMP_WARN_NEVER_ASSIGNED             33
#define ZH_COMP_WARN_STATIC_FUNC_UNUSED         34

ZH_EXTERN_END

#endif /* ZH_ERRORS_H_ */