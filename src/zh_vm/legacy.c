/* Copyright 2015 Viktor Szakats (vszakats.net/ziher) */

#include "zh_api.h"

#undef zh_arrayScan

extern ZH_SIZE zh_arrayScan( PZH_ITEM pArray, PZH_ITEM pValue, ZH_SIZE * pnStart, ZH_SIZE * pnCount, ZH_BOOL fExact );

ZH_SIZE zh_arrayScan( PZH_ITEM pArray, PZH_ITEM pValue, ZH_SIZE * pnStart, ZH_SIZE * pnCount, ZH_BOOL fExact )
{
   return zh_arrayScanCase( pArray, pValue, pnStart, pnCount, fExact, ZH_TRUE );
}
