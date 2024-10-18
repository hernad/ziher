/*
 * Code used to register GT driver
 *
 * Copyright 2009 Przemyslaw Czerpak
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

static const ZH_GT_INIT gtInit = { ZH_GT_DRVNAME( ZH_GT_NAME ),
                                   zh_gt_FuncInit,
                                   ZH_GTSUPER,
                                   ZH_GTID_PTR };

ZH_GT_ANNOUNCE( ZH_GT_NAME )


#if ( ZH_GT_NUM == 1 )
   ZH_CALL_ON_STARTUP_EXT_BEGIN( _zh_startup_gt_Init_STD )
   printf("========================== startup gt STD ===============================================================\n");   
#elif ( ZH_GT_NUM == 2 )
   ZH_CALL_ON_STARTUP_EXT_BEGIN( _zh_startup_gt_Init_TRM )
   printf("========================== startup gt TRM ===============================================================\n");   

#elif ( ZH_GT_NUM == 5 )
   ZH_CALL_ON_STARTUP_EXT_BEGIN( _zh_startup_gt_Init_XWC )
   printf("========================== startup gt XWC ===============================================================\n");   

#elif ( ZH_GT_NUM == 0 )
   ZH_CALL_ON_STARTUP_EXT_BEGIN( _zh_startup_gt_Init_NUL )
   printf("========================== startup gt NUL ===============================================================\n");   

#endif
  zh_gtRegister( &gtInit );   
  getchar();

#if ( ZH_GT_NUM == 1 )
  ZH_CALL_ON_STARTUP_EXT_END( _zh_startup_gt_Init_STD )
#elif ( ZH_GT_NUM == 2 )
  // ext__zh_startup_gt_Init_TRM
  ZH_CALL_ON_STARTUP_EXT_END( _zh_startup_gt_Init_TRM )
#elif ( ZH_GT_NUM == 5 )
  ZH_CALL_ON_STARTUP_EXT_END( _zh_startup_gt_Init_XWC )
#elif ( ZH_GT_NUM == 0 )
  ZH_CALL_ON_STARTUP_EXT_END( _zh_startup_gt_Init_NUL )
   //ZH_CALL_ON_STARTUP_END( ZH_MACRONAME_JOIN( _zh_startup_gt_Init_, ZH_GT_NAME ) )
#endif

#if defined( ZH_DATASEG_STARTUP )
   #define ZH_DATASEG_BODY    \
      ZH_DATASEG_FUNC( ZH_MACRONAME_JOIN( _zh_startup_gt_Init_, ZH_GT_NAME ) )
   #include "..\zh_ini_seg.h"
#endif
