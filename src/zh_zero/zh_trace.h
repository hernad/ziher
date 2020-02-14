/*
 * Header file for trace macros and functions.
 *
 * Copyright 1999 Gonzalo Diethelm <gonzalo.diethelm@iname.com>
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

#ifndef ZH_TRACE_H_
#define ZH_TRACE_H_

#include "zh_setup.h"

ZH_EXTERN_BEGIN

/*
 * Tracing levels.
 */
#define ZH_TR_ALWAYS     0
#define ZH_TR_FATAL      1
#define ZH_TR_ERROR      2
#define ZH_TR_WARNING    3
#define ZH_TR_INFO       4
#define ZH_TR_DEBUG      5
#define ZH_TR_LAST       6

#define ZH_TR_FM         10

/*
 * Default tracing level.
 */
#define ZH_TR_DEFAULT   ZH_TR_WARNING

/*
 * If we compiled without specifying a -DZH_TR_LEVEL, use the value
 * for ZH_TR_DEFAULT.
 */

#ifdef ZH_TR_LEVEL_ALWAYS
#define ZH_TR_LEVEL     ZH_TR_ALWAYS
#endif
#ifdef ZH_TR_LEVEL_FATAL
#define ZH_TR_LEVEL     ZH_TR_FATAL
#endif
#ifdef ZH_TR_LEVEL_ERROR
#define ZH_TR_LEVEL     ZH_TR_ERROR
#endif
#ifdef ZH_TR_LEVEL_WARNING
#define ZH_TR_LEVEL     ZH_TR_WARNING
#endif
#ifdef ZH_TR_LEVEL_INFO
#define ZH_TR_LEVEL     ZH_TR_INFO
#endif
#ifdef ZH_TR_LEVEL_DEBUG
#define ZH_TR_LEVEL     ZH_TR_DEBUG
#endif

#ifndef ZH_TR_LEVEL
#define ZH_TR_LEVEL     ZH_TR_DEFAULT
#endif


/*
 * This is black magic...
 * What we do here is to generate calls to ZH_ECHO_CREATE only for those
 * levels that are less or equal to the COMPILATION time ZH_TR_LEVEL.
 */

#define ZH_ECHO_CREATE( l, x )  do \
                                { \
                                   if( zh_tr_level() >= l ) \
                                   { \
                                      zh_traceset( l, __FILE__, __LINE__, NULL ); \
                                      zh_tr_trace x ; \
                                   } \
                                } while( 0 )

#if ZH_TR_LEVEL >= ZH_TR_DEBUG
#define ZH_ECHO_TRACE_ZH_TR_DEBUG(x)    ZH_ECHO_CREATE(ZH_TR_DEBUG, x)
#else
#define ZH_ECHO_TRACE_ZH_TR_DEBUG(x)    do {} while( 0 )
#endif

#if ZH_TR_LEVEL >= ZH_TR_INFO
#define ZH_ECHO_TRACE_ZH_TR_INFO(x)     ZH_ECHO_CREATE(ZH_TR_INFO, x)
#else
#define ZH_ECHO_TRACE_ZH_TR_INFO(x)     do {} while( 0 )
#endif

#if ZH_TR_LEVEL >= ZH_TR_WARNING
#define ZH_ECHO_TRACE_ZH_TR_WARNING(x)  ZH_ECHO_CREATE(ZH_TR_WARNING, x)
#else
#define ZH_ECHO_TRACE_ZH_TR_WARNING(x)  do {} while( 0 )
#endif

#if ZH_TR_LEVEL >= ZH_TR_ERROR
#define ZH_ECHO_TRACE_ZH_TR_ERROR(x)    ZH_ECHO_CREATE(ZH_TR_ERROR, x)
#else
#define ZH_ECHO_TRACE_ZH_TR_ERROR(x)    do {} while( 0 )
#endif

#if ZH_TR_LEVEL >= ZH_TR_FATAL
#define ZH_ECHO_TRACE_ZH_TR_FATAL(x)    ZH_ECHO_CREATE(ZH_TR_FATAL, x)
#else
#define ZH_ECHO_TRACE_ZH_TR_FATAL(x)    do {} while( 0 )
#endif

#if 1  /* always! */
#define ZH_ECHO_TRACE_ZH_TR_ALWAYS(x)   ZH_ECHO_CREATE(ZH_TR_ALWAYS, x)
#else
#define ZH_ECHO_TRACE_ZH_TR_ALWAYS(x)   do {} while( 0 )
#endif


#define ZH_TRACE(l, x)                ZH_ECHO_TRACE_##l(x)

/* NOTE: This will print tracing info without changing current
 * filename/linenum information - this is useful if we want to
 * trace the source of unreleased memory blocks
 */
#define ZH_ECHO_STEALTH( l, x ) do \
                                { \
                                   if( zh_tr_level() >= l ) \
                                      zh_tr_stealth x ; \
                                } while( 0 )

#if ZH_TR_LEVEL >= ZH_TR_DEBUG
#define ZH_ECHO_STEALTH_ZH_TR_DEBUG(x)    ZH_ECHO_STEALTH(ZH_TR_DEBUG, x)
#else
#define ZH_ECHO_STEALTH_ZH_TR_DEBUG(x)    do {} while( 0 )
#endif

#if ZH_TR_LEVEL >= ZH_TR_INFO
#define ZH_ECHO_STEALTH_ZH_TR_INFO(x)     ZH_ECHO_STEALTH(ZH_TR_INFO, x)
#else
#define ZH_ECHO_STEALTH_ZH_TR_INFO(x)     do {} while( 0 )
#endif

#if ZH_TR_LEVEL >= ZH_TR_WARNING
#define ZH_ECHO_STEALTH_ZH_TR_WARNING(x)  ZH_ECHO_STEALTH(ZH_TR_WARNING, x)
#else
#define ZH_ECHO_STEALTH_ZH_TR_WARNING(x)  do {} while( 0 )
#endif

#if ZH_TR_LEVEL >= ZH_TR_ERROR
#define ZH_ECHO_STEALTH_ZH_TR_ERROR(x)    ZH_ECHO_STEALTH(ZH_TR_ERROR, x)
#else
#define ZH_ECHO_STEALTH_ZH_TR_ERROR(x)    do {} while( 0 )
#endif

#if ZH_TR_LEVEL >= ZH_TR_FATAL
#define ZH_ECHO_STEALTH_ZH_TR_FATAL(x)    ZH_ECHO_STEALTH(ZH_TR_FATAL, x)
#else
#define ZH_ECHO_STEALTH_ZH_TR_FATAL(x)    do {} while( 0 )
#endif

#if 1  /* always! */
#define ZH_ECHO_STEALTH_ZH_TR_ALWAYS(x)   ZH_ECHO_STEALTH(ZH_TR_ALWAYS, x)
#else
#define ZH_ECHO_STEALTH_ZH_TR_ALWAYS(x)   do {} while( 0 )
#endif

/* NOTE: This will print tracing info without changing current
 * filename/linenum information
 */
#define ZH_TRACE_STEALTH(l, x)            ZH_ECHO_STEALTH_##l(x)

typedef struct
{
   const char * file;
   const char * proc;
   int          line;
   int          level;
}
ZH_TRACEINFO, * PZH_TRACEINFO;

extern ZH_EXPORT int           zh_tracestate( int new_state );
extern ZH_EXPORT int           zh_tracelevel( int new_level );
extern ZH_EXPORT int           zh_traceflush( int new_flush );
extern ZH_EXPORT int           zh_tracesysout( int new_sysout );
extern ZH_EXPORT const char *  zh_tracemode( const char * szNewMode );
extern ZH_EXPORT ZH_BOOL       zh_tracefile( const char * szFile );

extern ZH_EXPORT void          zh_tracelog( int level, const char * file, int line, const char * proc, const char * fmt, ... ) ZH_PRINTF_FORMAT( 5, 6 );

extern ZH_EXPORT void          zh_traceset( int level, const char * file, int line, const char * proc );
extern ZH_EXPORT PZH_TRACEINFO zh_traceinfo( void );

extern ZH_EXPORT int           zh_tr_level( void );
extern ZH_EXPORT void          zh_tr_trace( const char * fmt, ... ) ZH_PRINTF_FORMAT( 1, 2 );
extern ZH_EXPORT void          zh_tr_stealth( const char * fmt, ... ) ZH_PRINTF_FORMAT( 1, 2 );

ZH_EXTERN_END

#endif /* ZH_TRACE_H_ */
