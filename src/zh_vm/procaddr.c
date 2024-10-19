/*
 * Function to export chosen addresses of public API function
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

#include "zh_api.h"
#include "zh_vm.h"

/*

unused mandllp

PZH_FUNC zh_vmProcAddress( const char * szFuncName )
{
   typedef struct
   {
      const char * szFuncName;
      PZH_FUNC     pFuncAddr;
   }
   ZH_FUNC_REF_ADDR;

   // NOTE: this table must be well sorted by function names
   static const ZH_FUNC_REF_ADDR s_funcTable[] =
   {
      { "zh_arrayAdd",         ( PZH_FUNC ) zh_arrayAdd               },
      { "zh_arrayDel",         ( PZH_FUNC ) zh_arrayDel               },
      { "zh_arrayGet",         ( PZH_FUNC ) zh_arrayGet               },
      { "zh_arrayIns",         ( PZH_FUNC ) zh_arrayIns               },
      { "zh_arrayIsObject",    ( PZH_FUNC ) zh_arrayIsObject          },
      { "zh_arrayLast",        ( PZH_FUNC ) zh_arrayLast              },
      { "zh_arrayLen",         ( PZH_FUNC ) zh_arrayLen               },
      { "zh_arrayNew",         ( PZH_FUNC ) zh_arrayNew               },
      { "zh_arraySet",         ( PZH_FUNC ) zh_arraySet               },
      { "zh_arraySize",        ( PZH_FUNC ) zh_arraySize              },
      { "zh_extIsArray",       ( PZH_FUNC ) zh_extIsArray             },
      { "zh_extIsNil",         ( PZH_FUNC ) zh_extIsNil               },
      { "zh_param",            ( PZH_FUNC ) zh_param                  },
      { "zh_paramError",       ( PZH_FUNC ) zh_paramError             },
      { "zh_parinfa",          ( PZH_FUNC ) zh_parinfa                },
      { "zh_parinfo",          ( PZH_FUNC ) zh_parinfo                },
      { "zh_parvc",            ( PZH_FUNC ) zh_parvc                  },
      { "zh_parvclen",         ( PZH_FUNC ) zh_parvclen               },
      { "zh_parvcsiz",         ( PZH_FUNC ) zh_parvcsiz               },
      { "zh_parvds",           ( PZH_FUNC ) zh_parvds                 },
      { "zh_parvdsbuff",       ( PZH_FUNC ) zh_parvdsbuff             },
      { "zh_parvl",            ( PZH_FUNC ) zh_parvl                  },
      { "zh_parvnd",           ( PZH_FUNC ) zh_parvnd                 },
      { "zh_parvni",           ( PZH_FUNC ) zh_parvni                 },
      { "zh_parvnl",           ( PZH_FUNC ) zh_parvnl                 },
      { "zh_pcount",           ( PZH_FUNC ) zh_pcount                 },
      { "zh_ret",              ( PZH_FUNC ) zh_ret                    },
      { "zh_reta",             ( PZH_FUNC ) zh_reta                   },
      { "zh_retc",             ( PZH_FUNC ) zh_retc                   },
      { "zh_retclen",          ( PZH_FUNC ) zh_retclen                },
      { "zh_retd",             ( PZH_FUNC ) zh_retd                   },
      { "zh_retdl",            ( PZH_FUNC ) zh_retdl                  },
      { "zh_retds",            ( PZH_FUNC ) zh_retds                  },
      { "zh_retl",             ( PZH_FUNC ) zh_retl                   },
      { "zh_retnd",            ( PZH_FUNC ) zh_retnd                  },
      { "zh_retndlen",         ( PZH_FUNC ) zh_retndlen               },
      { "zh_retni",            ( PZH_FUNC ) zh_retni                  },
      { "zh_retnilen",         ( PZH_FUNC ) zh_retnilen               },
      { "zh_retnl",            ( PZH_FUNC ) zh_retnl                  },
      { "zh_retnlen",          ( PZH_FUNC ) zh_retnlen                },
      { "zh_retnllen",         ( PZH_FUNC ) zh_retnllen               },
      { "zh_storvc",           ( PZH_FUNC ) zh_storvc                 },
      { "zh_storvclen",        ( PZH_FUNC ) zh_storvclen              },
      { "zh_storvds",          ( PZH_FUNC ) zh_storvds                },
      { "zh_storvl",           ( PZH_FUNC ) zh_storvl                 },
      { "zh_storvnd",          ( PZH_FUNC ) zh_storvnd                },
      { "zh_storvni",          ( PZH_FUNC ) zh_storvni                },
      { "zh_storvnl",          ( PZH_FUNC ) zh_storvnl                },
      { "zh_vmExecute",        ( PZH_FUNC ) zh_vmExecute              },
      { "zh_vmProcessSymbols", ( PZH_FUNC ) zh_vmProcessDynLibSymbols },
      { "zh_xalloc",           ( PZH_FUNC ) zh_xalloc                 },
      { "zh_xfree",            ( PZH_FUNC ) zh_xfree                  },
      { "zh_xgrab",            ( PZH_FUNC ) zh_xgrab                  },
      { "zh_xrealloc",         ( PZH_FUNC ) zh_xrealloc               }
   };

   unsigned int uiFirst = 0, uiLast = ZH_SIZEOFARRAY( s_funcTable ), uiMiddle;
   int iCmp;

   do
   {
      uiMiddle = ( uiFirst + uiLast ) >> 1;
      iCmp = strcmp( szFuncName, s_funcTable[ uiMiddle ].szFuncName );
      if( iCmp <= 0 )
         uiLast = uiMiddle;
      else
         uiFirst = uiMiddle + 1;
   }
   while( uiFirst < uiLast );

   if( uiFirst != uiMiddle )
      iCmp = strcmp( szFuncName, s_funcTable[ uiFirst ].szFuncName );

   return iCmp == 0 ? s_funcTable[ uiFirst ].pFuncAddr : NULL;
}

*/