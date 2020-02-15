/*
 * Get function identifier
 *
 * Copyright 2010 Przemyslaw Czerpak
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

typedef struct
{
   const char * szFuncName;
   int          iMinLen;
   int          flags;
   ZH_FUNC_ID   funcID;
} _ZH_FUNCID;

/* NOTE: THIS TABLE MUST BE SORTED ALPHABETICALLY
 */
static const _ZH_FUNCID s_funcId[] =
{
   { "AADD",                    0, ZH_FN_RESERVED, ZH_F_AADD                 },
   { "ABS",                     0, ZH_FN_RESERVED, ZH_F_ABS                  },
   { "ARRAY",                   0, ZH_FN_UDF,      ZH_F_ARRAY                },
   { "ASC",                     0, ZH_FN_RESERVED, ZH_F_ASC                  },
   { "AT",                      0, ZH_FN_RESERVED, ZH_F_AT                   },
   { "BOF",                     0, ZH_FN_RESERVED, ZH_F_BOF                  },
   { "BREAK",                   4, ZH_FN_RESERVED, ZH_F_BREAK                },
   { "CDOW",                    0, ZH_FN_RESERVED, ZH_F_CDOW                 },
   { "CHR",                     0, ZH_FN_RESERVED, ZH_F_CHR                  },
   { "CMONTH",                  4, ZH_FN_RESERVED, ZH_F_CMONTH               },
   { "COL",                     0, ZH_FN_RESERVED, ZH_F_COL                  },
   { "CTOD",                    0, ZH_FN_RESERVED, ZH_F_CTOD                 },
   { "DATE",                    0, ZH_FN_RESERVED, ZH_F_DATE                 },
   { "DAY",                     0, ZH_FN_RESERVED, ZH_F_DAY                  },
   { "DELETED",                 4, ZH_FN_RESERVED, ZH_F_DELETED              },
   { "DEVPOS",                  4, ZH_FN_RESERVED, ZH_F_DEVPOS               },
   { "DOW",                     0, ZH_FN_RESERVED, ZH_F_DOW                  },
   { "DTOC",                    0, ZH_FN_RESERVED, ZH_F_DTOC                 },
   { "DTOS",                    0, ZH_FN_RESERVED, ZH_F_DTOS                 },
   { "EMPTY",                   4, ZH_FN_RESERVED, ZH_F_EMPTY                },
   { "EOF",                     0, ZH_FN_RESERVED, ZH_F_EOF                  },
   { "EVAL",                    0, ZH_FN_UDF,      ZH_F_EVAL                 },
   { "EXP",                     0, ZH_FN_RESERVED, ZH_F_EXP                  },
   { "FCOUNT",                  4, ZH_FN_RESERVED, ZH_F_FCOUNT               },
   { "FIELDNAME",               4, ZH_FN_RESERVED, ZH_F_FIELDNAME            },
   { "FILE",                    0, ZH_FN_UDF,      ZH_F_FILE                 },
   { "FLOCK",                   4, ZH_FN_RESERVED, ZH_F_FLOCK                },
   { "FOUND",                   4, ZH_FN_RESERVED, ZH_F_FOUND                },
   { "ZH_ARRAYTOPARAMS",        0, ZH_FN_UDF,      ZH_F_ARRAYTOPARAMS        },
   { "ZH_BCHAR",                0, ZH_FN_UDF,      ZH_F_BCHAR                },
   { "ZH_BCODE",                0, ZH_FN_UDF,      ZH_F_BCODE                },
   { "ZH_BITAND",               0, ZH_FN_UDF,      ZH_F_BITAND               },
   { "ZH_BITNOT",               0, ZH_FN_UDF,      ZH_F_BITNOT               },
   { "ZH_BITOR",                0, ZH_FN_UDF,      ZH_F_BITOR                },
   { "ZH_BITRESET",             0, ZH_FN_UDF,      ZH_F_BITRESET             },
   { "ZH_BITSET",               0, ZH_FN_UDF,      ZH_F_BITSET               },
   { "ZH_BITSHIFT",             0, ZH_FN_UDF,      ZH_F_BITSHIFT             },
   { "ZH_BITTEST",              0, ZH_FN_UDF,      ZH_F_BITTEST              },
   { "ZH_BITXOR",               0, ZH_FN_UDF,      ZH_F_BITXOR               },
   { "ZH_I18N_GETTEXT",         0, ZH_FN_UDF,      ZH_F_I18N_GETTEXT         },
   { "ZH_I18N_GETTEXT_NOOP",    0, ZH_FN_UDF,      ZH_F_I18N_GETTEXT_NOOP    },
   { "ZH_I18N_GETTEXT_STRICT",  0, ZH_FN_UDF,      ZH_F_I18N_GETTEXT_STRICT  },
   { "ZH_I18N_NGETTEXT",        0, ZH_FN_UDF,      ZH_F_I18N_NGETTEXT        },
   { "ZH_I18N_NGETTEXT_NOOP",   0, ZH_FN_UDF,      ZH_F_I18N_NGETTEXT_NOOP   },
   { "ZH_I18N_NGETTEXT_STRICT", 0, ZH_FN_UDF,      ZH_F_I18N_NGETTEXT_STRICT },
   { "ZH_STOD",                 0, ZH_FN_UDF,      ZH_F_STOD                 },
   { "ZH_STOT",                 0, ZH_FN_UDF,      ZH_F_STOT                 },
   { "INKEY",                   4, ZH_FN_RESERVED, ZH_F_INKEY                },
   { "INT",                     0, ZH_FN_RESERVED, ZH_F_INT                  },
   { "LASTREC",                 4, ZH_FN_RESERVED, ZH_F_LASTREC              },
   { "LEFT",                    0, ZH_FN_RESERVED, ZH_F_LEFT                 },
   { "LEN",                     0, ZH_FN_RESERVED, ZH_F_LEN                  },
   { "LOCK",                    0, ZH_FN_RESERVED, ZH_F_LOCK                 },
   { "LOG",                     0, ZH_FN_RESERVED, ZH_F_LOG                  },
   { "LOWER",                   4, ZH_FN_RESERVED, ZH_F_LOWER                },
   { "LTRIM",                   4, ZH_FN_RESERVED, ZH_F_LTRIM                },
   { "MAX",                     0, ZH_FN_RESERVED, ZH_F_MAX                  },
   { "MIN",                     0, ZH_FN_RESERVED, ZH_F_MIN                  },
   { "MONTH",                   4, ZH_FN_RESERVED, ZH_F_MONTH                },
   { "PCOL",                    0, ZH_FN_RESERVED, ZH_F_PCOL                 },
   { "PCOUNT",                  4, ZH_FN_RESERVED, ZH_F_PCOUNT               },
   { "PROW",                    0, ZH_FN_RESERVED, ZH_F_PROW                 },
   { "QSELF",                   4, ZH_FN_RESERVED, ZH_F_QSELF                },
   { "RECCOUNT",                4, ZH_FN_RESERVED, ZH_F_RECCOUNT             },
   { "RECNO",                   4, ZH_FN_RESERVED, ZH_F_RECNO                },
   { "REPLICATE",               4, ZH_FN_RESERVED, ZH_F_REPLICATE            },
   { "RLOCK",                   4, ZH_FN_RESERVED, ZH_F_RLOCK                },
   { "ROUND",                   4, ZH_FN_RESERVED, ZH_F_ROUND                },
   { "ROW",                     0, ZH_FN_RESERVED, ZH_F_ROW                  },
   { "RTRIM",                   4, ZH_FN_RESERVED, ZH_F_RTRIM                },
   { "SECONDS",                 4, ZH_FN_RESERVED, ZH_F_SECONDS              },
   { "SELECT",                  4, ZH_FN_RESERVED, ZH_F_SELECT               },
   { "SETPOS",                  4, ZH_FN_RESERVED, ZH_F_SETPOS               },
   { "SETPOSBS",                4, ZH_FN_RESERVED, ZH_F_SETPOSBS             },
   { "SPACE",                   4, ZH_FN_RESERVED, ZH_F_SPACE                },
   { "SQRT",                    0, ZH_FN_RESERVED, ZH_F_SQRT                 },
   { "STOD",                    0, ZH_FN_UDF,      ZH_F_STOD                 },
   { "STR",                     0, ZH_FN_RESERVED, ZH_F_STR                  },
   { "SUBSTR",                  4, ZH_FN_RESERVED, ZH_F_SUBSTR               },
   { "TIME",                    0, ZH_FN_RESERVED, ZH_F_TIME                 },
   { "TRANSFORM",               4, ZH_FN_RESERVED, ZH_F_TRANSFORM            },
   { "TRIM",                    0, ZH_FN_RESERVED, ZH_F_TRIM                 },
   { "TYPE",                    0, ZH_FN_RESERVED, ZH_F_TYPE                 },
   { "UPPER",                   4, ZH_FN_RESERVED, ZH_F_UPPER                },
   { "VAL",                     0, ZH_FN_RESERVED, ZH_F_VAL                  },
   { "VALTYPE",                 4, ZH_FN_RESERVED, ZH_F_VALTYPE              },
   { "WORD",                    0, ZH_FN_RESERVED, ZH_F_WORD                 },
   { "YEAR",                    0, ZH_FN_RESERVED, ZH_F_YEAR                 },
   { "_GET_",                   0, ZH_FN_UDF,      ZH_F__GET_                }
};

const char * zh_compGetFuncID( const char * szFuncName, ZH_FUNC_ID * pFunID, int * piFlags )
{
   unsigned int uiFirst = 0, uiLast = ZH_SIZEOFARRAY( s_funcId ) - 1, uiMiddle;
   int i;

   do
   {
      uiMiddle = ( uiFirst + uiLast ) >> 1;
      i = strcmp( szFuncName, s_funcId[ uiMiddle ].szFuncName );
      if( i <= 0 )
         uiLast = uiMiddle;
      else
         uiFirst = uiMiddle + 1;
   }
   while( uiFirst < uiLast );

   if( uiFirst != uiMiddle )
      i = strcmp( szFuncName, s_funcId[ uiFirst ].szFuncName );

   if( i < 0 && s_funcId[ uiFirst ].iMinLen )
   {
      int iLen = ( int ) strlen( szFuncName );

      if( iLen >= s_funcId[ uiFirst ].iMinLen )
         i = strncmp( szFuncName, s_funcId[ uiFirst ].szFuncName, iLen );
   }

   if( i == 0 )
   {
      *piFlags = s_funcId[ uiFirst ].flags;
      *pFunID = s_funcId[ uiFirst ].funcID;
      return s_funcId[ uiFirst ].szFuncName;
   }

   *piFlags = ZH_FN_UDF;
   *pFunID = ZH_F_UDF;

   /* hack for ZH_I18N_GETTEXT_[NOOP_|STRICT_]* functions */
   if( strncmp( szFuncName, "ZH_I18N_", 8 ) == 0 )
   {
      const char * szName = szFuncName + 8;
      i = *szName == 'N' ? 1 : 0;
      szName += i;
      if( strncmp( szName, "GETTEXT_", 8 ) == 0 )
      {
         szName += 8;
         if( strncmp( szName, "STRICT_", 7 ) == 0 )
            *pFunID = i ? ZH_F_I18N_NGETTEXT_STRICT : ZH_F_I18N_GETTEXT_STRICT;
         else if( strncmp( szName, "NOOP_", 5 ) == 0 )
            *pFunID = i ? ZH_F_I18N_NGETTEXT_NOOP : ZH_F_I18N_GETTEXT_NOOP;
         else
            *pFunID = i ? ZH_F_I18N_NGETTEXT :  ZH_F_I18N_GETTEXT;
      }
   }

   return szFuncName;
}
