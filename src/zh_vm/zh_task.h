/*
 * Platform independent task system. It's used when when OS does not
 * support threads
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

#ifndef ZH_TASK_H_
#define ZH_TASK_H_

#include "zh_api.h"

ZH_EXTERN_BEGIN

extern void    zh_taskInit( void );
extern void    zh_taskExit( void );
extern void *  zh_taskCreate( void * ( * ) ( void * ), void *, long );
extern void    zh_taskDestroy( void * );
extern void    zh_taskYield( void );
extern void    zh_taskSheduler( void );
extern void    zh_taskSuspend( void );
extern void    zh_taskResume( void * );
extern void    zh_taskSleep( unsigned long );
extern void *  zh_taskMain( void );
extern void *  zh_taskSelf( void );
extern int     zh_taskID( void * );
extern void    zh_taskSetData( void * );
extern void *  zh_taskGetData( void );
extern void    zh_taskSaveData( void *, void * );
extern void *  zh_taskRestoreData( void * );
extern void *  zh_taskResult( void * );
extern int     zh_taskJoin( void *, unsigned long, void ** );
extern void    zh_taskDetach( void * );
extern void    zh_taskQuit( void * );
extern int     zh_taskLock( void **, unsigned long );
extern void    zh_taskUnlock( void ** );
extern void    zh_taskSignal( void ** cond );
extern void    zh_taskBroadcast( void ** cond );
extern int     zh_taskWait( void ** cond, void ** mutex, unsigned long ulMilliSec );
extern void    zh_taskDestroyMutex( void ** );
extern void    zh_taskDestroyCond( void ** );

#define ZH_TASK_INFINITE_WAIT       ( ( unsigned long ) -1 )

ZH_EXTERN_END

#endif /* ZH_TASK_H_ */
