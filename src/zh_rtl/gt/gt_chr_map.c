/*
 * Video subsystem based on ncurses screen library.
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
 * Special thanks to Marek Paliwoda <paliwoda@inetia.pl>
 * author of gtsln from which I borrowed a lot of code and ideas.
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

/* NOTE: User programs should never call this layer directly! */

/* TODO: use Ziher FS API */

#include "zh_api.h"
#include "zh_gt_core.h"
#include "zh_apifs.h"

#if defined( ZH_OS_UNIX ) || defined( ZH_OS_DOS )

#define MAX_CHAR_VAL  0xff
#define ZH_CHRMAP( a, c )  ( ( ( a ) << 16 ) | ( c ) )

static const char * s_szCharMapFileDefault = "/etc/ziher/hb-charmap.def";

static void chrmap_init( int * piTransTbl )
{
   int i;

   for( i = 0; i < 256; ++i )
      piTransTbl[ i ] = ZH_CHRMAP( i < 128 ? 1 : 0, i );

   piTransTbl[ 155 ] = ZH_CHRMAP( 1, '.' );
}

static void chrmap_dotctrl( int * piTransTbl )
{
   int i;

   for( i = 0; i < 32; ++i )
      piTransTbl[ i ] = piTransTbl[ i + 128 ] = ZH_CHRMAP( 1, '.' );
}

static void chrmap_ascictrl( int * piTransTbl )
{
   piTransTbl[ 4 ]  = ZH_CHRMAP( 1, '#' );
   piTransTbl[ 16 ] = ZH_CHRMAP( 1, '>' );
   piTransTbl[ 17 ] = ZH_CHRMAP( 1, '<' );
   piTransTbl[ 30 ] = ZH_CHRMAP( 1, '^' );
   piTransTbl[ 31 ] = ZH_CHRMAP( 1, 'v' );
   piTransTbl[ 24 ] = ZH_CHRMAP( 1, '^' );
   piTransTbl[ 25 ] = ZH_CHRMAP( 1, 'v' );
   piTransTbl[ 26 ] = ZH_CHRMAP( 1, '>' );
   piTransTbl[ 27 ] = ZH_CHRMAP( 1, '<' );
}

static void chrmap_acscbox( int * piTransTbl )
{
   piTransTbl[   4 ] = ZH_CHRMAP( 5, '`' ); /* ACS_DIAMOND */
   piTransTbl[  16 ] = ZH_CHRMAP( 5, '+' ); /* ACS_RARROW */
   piTransTbl[  17 ] = ZH_CHRMAP( 5, ',' ); /* ACS_LARROW */
   piTransTbl[  24 ] = ZH_CHRMAP( 5, '-' ); /* ACS_UARROW */
   piTransTbl[  25 ] = ZH_CHRMAP( 5, '.' ); /* ACS_DARROW */
   piTransTbl[  26 ] = ZH_CHRMAP( 5, '+' ); /* ACS_RARROW */
   piTransTbl[  27 ] = ZH_CHRMAP( 5, ',' ); /* ACS_LARROW */
   piTransTbl[  30 ] = ZH_CHRMAP( 5, '-' ); /* ACS_UARROW */
   piTransTbl[  31 ] = ZH_CHRMAP( 5, '.' ); /* ACS_DARROW */

   piTransTbl[ 176 ] = ZH_CHRMAP( 5, 'h' ); /* ACS_BOARD */
   piTransTbl[ 177 ] = ZH_CHRMAP( 5, 'a' ); /* ACS_CKBOARD */
   piTransTbl[ 178 ] = ZH_CHRMAP( 5, '0' ); /* ACS_BLOCK */
   piTransTbl[ 179 ] = ZH_CHRMAP( 5, 'x' ); /* ACS_VLINE */
   piTransTbl[ 180 ] = ZH_CHRMAP( 5, 'u' ); /* ACS_RTEE */
   piTransTbl[ 181 ] = ZH_CHRMAP( 5, 'u' ); /* ACS_RTEE */
   piTransTbl[ 182 ] = ZH_CHRMAP( 5, 'u' ); /* ACS_RTEE */
   piTransTbl[ 183 ] = ZH_CHRMAP( 5, 'k' ); /* ACS_URCORNER */
   piTransTbl[ 184 ] = ZH_CHRMAP( 5, 'k' ); /* ACS_URCORNER */
   piTransTbl[ 185 ] = ZH_CHRMAP( 5, 'u' ); /* ACS_RTEE */
   piTransTbl[ 186 ] = ZH_CHRMAP( 5, 'x' ); /* ACS_VLINE */
   piTransTbl[ 187 ] = ZH_CHRMAP( 5, 'k' ); /* ACS_URCORNER */
   piTransTbl[ 188 ] = ZH_CHRMAP( 5, 'j' ); /* ACS_LRCORNER */
   piTransTbl[ 189 ] = ZH_CHRMAP( 5, 'j' ); /* ACS_LRCORNER */
   piTransTbl[ 190 ] = ZH_CHRMAP( 5, 'j' ); /* ACS_LRCORNER */
   piTransTbl[ 191 ] = ZH_CHRMAP( 5, 'k' ); /* ACS_URCORNER */
   piTransTbl[ 192 ] = ZH_CHRMAP( 5, 'm' ); /* ACS_LLCORNER */
   piTransTbl[ 193 ] = ZH_CHRMAP( 5, 'v' ); /* ACS_BTEE */
   piTransTbl[ 194 ] = ZH_CHRMAP( 5, 'w' ); /* ACS_TTEE */
   piTransTbl[ 195 ] = ZH_CHRMAP( 5, 't' ); /* ACS_LTEE */
   piTransTbl[ 196 ] = ZH_CHRMAP( 5, 'q' ); /* ACS_HLINE */
   piTransTbl[ 197 ] = ZH_CHRMAP( 5, 'n' ); /* ACS_PLUS */
   piTransTbl[ 198 ] = ZH_CHRMAP( 5, 't' ); /* ACS_LTEE */
   piTransTbl[ 199 ] = ZH_CHRMAP( 5, 't' ); /* ACS_LTEE */
   piTransTbl[ 200 ] = ZH_CHRMAP( 5, 'm' ); /* ACS_LLCORNER */
   piTransTbl[ 201 ] = ZH_CHRMAP( 5, 'l' ); /* ACS_ULCORNER */
   piTransTbl[ 202 ] = ZH_CHRMAP( 5, 'v' ); /* ACS_BTEE */
   piTransTbl[ 203 ] = ZH_CHRMAP( 5, 'w' ); /* ACS_TTEE */
   piTransTbl[ 204 ] = ZH_CHRMAP( 5, 't' ); /* ACS_LTEE */
   piTransTbl[ 205 ] = ZH_CHRMAP( 5, 'q' ); /* ACS_HLINE */
   piTransTbl[ 206 ] = ZH_CHRMAP( 5, 'n' ); /* ACS_PLUS */
   piTransTbl[ 207 ] = ZH_CHRMAP( 5, 'v' ); /* ACS_BTEE */
   piTransTbl[ 208 ] = ZH_CHRMAP( 5, 'v' ); /* ACS_BTEE */
   piTransTbl[ 209 ] = ZH_CHRMAP( 5, 'w' ); /* ACS_TTEE */
   piTransTbl[ 210 ] = ZH_CHRMAP( 5, 'w' ); /* ACS_TTEE */
   piTransTbl[ 211 ] = ZH_CHRMAP( 5, 'm' ); /* ACS_LLCORNER */
   piTransTbl[ 212 ] = ZH_CHRMAP( 5, 'm' ); /* ACS_LLCORNER */
   piTransTbl[ 213 ] = ZH_CHRMAP( 5, 'l' ); /* ACS_ULCORNER */
   piTransTbl[ 214 ] = ZH_CHRMAP( 5, 'l' ); /* ACS_ULCORNER */
   piTransTbl[ 215 ] = ZH_CHRMAP( 5, 'n' ); /* ACS_PLUS */
   piTransTbl[ 216 ] = ZH_CHRMAP( 5, 'n' ); /* ACS_PLUS */
   piTransTbl[ 217 ] = ZH_CHRMAP( 5, 'j' ); /* ACS_LRCORNER */
   piTransTbl[ 218 ] = ZH_CHRMAP( 5, 'l' ); /* ACS_ULCORNER */

#if 0
   piTransTbl[ 219 ] = ZH_CHRMAP( 5, '`' ); /* ACS_DIAMOND */
   piTransTbl[ 220 ] = ZH_CHRMAP( 5, '`' ); /* ACS_DIAMOND */
   piTransTbl[ 221 ] = ZH_CHRMAP( 5, '`' ); /* ACS_DIAMOND */
   piTransTbl[ 222 ] = ZH_CHRMAP( 5, '`' ); /* ACS_DIAMOND */
   piTransTbl[ 223 ] = ZH_CHRMAP( 5, '`' ); /* ACS_DIAMOND */
#endif
}

static void skip_blank( char ** buf )
{
   while( **buf != '\0' && **buf == ' ' )
      ++( *buf );
}

static int get_val( char ** buf )
{
   int n = -1;

   if( ( *buf )[ 0 ] == '\'' && ( *buf )[ 1 ] != '\0' && ( *buf )[ 2 ] == '\'' )
   {
      n = ( *buf )[ 1 ] & 0xff;
      *buf += 3;
   }
   else if( ( *buf )[ 0 ] == '0' && ( ( *buf )[ 1 ] == 'x' || ( *buf )[ 1 ] == 'X' ) )
   {
      n = 0;
      *buf += 2;
      for(; ( **buf >= '0' && **buf <= '9' ) ||
            ( **buf >= 'A' && **buf <= 'F' ) ||
            ( **buf >= 'a' && **buf <= 'f' ); ++( *buf ) )
      {
         char c = **buf | 0x20;
         n = ( n << 4 ) + c - ( c > '9' ? ( 'a' - 10 ) : '0' );
      }
   }
   else if( **buf >= '0' && **buf <= '9' )
   {
      n = 0;
      for(; ( **buf >= '0' && **buf <= '9' ); ++( *buf ) )
         n = n * 10 + ( **buf - '0' );
   }
   return n > 0xff ? -1 : n;
}

static int parse_line( char * buf, int * from, int * to, char * op, int * val, int * mod )
{
   char *s;
   int ret = 0, ina = 0;

   s = buf;
   while( *s != '\0' )
   {
      switch( *s )
      {
         case '\t':
            *s = ' ';
            break;
         case '\'':
            ina ^= 1;
            if( ina )
               ++s;
            break;
         case '\n':
         case '\r':
         case '#':
            *s = '\0';
            break;
      }
      if( *s != '\0' )
         ++s;
   }

   s = buf;
   skip_blank( &s );

   if( *s == '@' )
   {
      char *s2;
      ++s;
      s2 = buf;
      while( *s != '\0' && *s != ' ' )
         *s2++ = *s++;
      *s2 = '\0';
      ret = strlen( buf ) > 0 ? 2 : -1;
   }
   else if( *s != '\0' )
   {
      ret = *from = *to = *val = *mod = -1;
      *op = '=';

      *from = get_val( &s );
      if( *from >= 0 )
      {
         if( *s == '-' )
         {
            ++s;
            *to = get_val( &s );
         }
         else
            *to = *from;
      }

      if( *to >= 0 && *s == ':' && s[ 1 ] == ' ' )
      {
         ++s;
         skip_blank( &s );
         if( *s == '*' && ( s[ 1 ] == '+' || s[ 1 ] == '-' || s[ 1 ] == '&' ||
                            s[ 1 ] == '|' || s[ 1 ] == '^' || s[ 1 ] == '=' ||
                            s[ 1 ] == ' ' ) )
         {
            *op = s[ 1 ];
            s += 2;
         }
         *val = *op == ' ' ? 0 : get_val( &s );
         if( *val >= 0 )
         {
            skip_blank( &s );
            *mod = get_val( &s );
            skip_blank( &s );
            if( *mod >= 0 && *mod <= 5 && *s == '\0' )
               ret = 1;
         }
      }
   }
   return ret;
}

static int chrmap_parse( FILE * fp, const char * pszTerm, int * nTransTbl, const char * pszFile )
{
   int line = 0, from = 0, to = 0, val = 0, mod = 0, i, n;
   char * s, op = 0;
   int isTerm = 0;
   fpos_t pos;

   fgetpos( fp, &pos );
   ( void ) fseek( fp, 0, SEEK_SET );

   while( ! feof( fp ) && isTerm < 2 )
   {
      char buf[ 256 ];
      ++line;
      if( fgets( buf, sizeof( buf ), fp ) != NULL )
      {
         n = 0;
         if( *buf == ':' )
         {
            if( isTerm == 1 )
               isTerm = 2;
            else
            {
               *buf = '|';
               s = buf;
               while( *s != '\0' && *s != ' ' && *s != '\t' &&
                      *s != '\n' && *s != '\r' )
                  ++s;
               *s = '\0';
               s = buf;
               i = ( int ) strlen( pszTerm );
               while( isTerm == 0 && ( s = strstr( s + 1, pszTerm ) ) != NULL )
               {
                  if( *( s - 1 ) == '|' &&
                      ( s[ i ] == '|' || s[ i ] == '\0' ) )
                     isTerm = 1;
               }
            }
         }
         else if( isTerm == 1 )
         {
            n = parse_line( buf, &from, &to, &op, &val, &mod );
         }

         if( n == 2 )
         {
            chrmap_parse( fp, buf, nTransTbl, pszFile );
         }
         else if( n == 1 )
         {
            #if 0
            printf( "line: %3d\tfrom=%d, to=%d, op='%c', val=%d, mod=%d\n", line, from, to, op, val, mod );
            #endif
            for( i = from; i <= to; ++i )
            {
               switch( op )
               {
                  case '|':
                     nTransTbl[ i ] = ( i | val );
                     break;
                  case '&':
                     nTransTbl[ i ] = ( i & val );
                     break;
                  case '^':
                     nTransTbl[ i ] = ( i ^ val );
                     break;
                  case '+':
                     nTransTbl[ i ] = ( i + val ) & 0xff;
                     break;
                  case '-':
                     nTransTbl[ i ] = ( i - val ) & 0xff;
                     break;
                  case '=':
                     nTransTbl[ i ] = val;
                     break;
                  case '*':
                  case ' ':
                  default:
                     nTransTbl[ i ] = i;
                     break;
               }
               nTransTbl[ i ] |= mod << 16;
            }
         }
         else if( n == -1 )
         {
            fprintf( stderr, "file: %s, parse error at line: %d\n", pszFile, line );
         }
      }
   }

   ( void ) fsetpos( fp, &pos );

   return isTerm;
}

static int zh_gt_chrmapread( const char * pszFile, const char * pszTerm, int * nTransTbl )
{
   int isTerm = -1;
   FILE * fp = zh_fopen( pszFile, "r" );

   if( fp != NULL )
   {
      char buf[ 256 ], * ptr, * pTerm;

      zh_strncpy( buf, pszTerm, sizeof( buf ) - 1 );
      isTerm = 0;
      pTerm = buf;
      while( pTerm )
      {
         if( ( ptr = strchr( pTerm, '/' ) ) != NULL )
            *ptr++ = '\0';

         if( *pTerm )
            if( chrmap_parse( fp, pTerm, nTransTbl, pszFile ) > 0 )
               isTerm = 1;

         pTerm = ptr;
      }
      fclose( fp );
   }
   return isTerm;
}

int zh_gt_chrmapinit( int * piTransTbl, const char * pszTerm, ZH_BOOL fSetACSC )
{
   char * pszFree = NULL;
   int nRet = -1;

   chrmap_init( piTransTbl );

   if( pszTerm == NULL || *pszTerm == '\0' )
      pszTerm = pszFree = zh_getenv( "ZH_TERM" );
   if( pszTerm == NULL || *pszTerm == '\0' )
   {
      if( pszFree )
         zh_xfree( pszFree );
      pszTerm = pszFree = zh_getenv( "TERM" );
   }

   if( pszTerm != NULL && *pszTerm != '\0' )
   {
      char * pszFile = zh_getenv( "ZH_CHARMAP" );

      if( pszFile != NULL && *pszFile != '\0' )
         nRet = zh_gt_chrmapread( pszFile, pszTerm, piTransTbl );
      if( nRet == -1 )
      {
         char szFile[ ZH_PATH_MAX ];
         if( pszFile )
            zh_xfree( pszFile );
         pszFile = zh_getenv( "ZH_ROOT" );
         if( pszFile != NULL && sizeof( szFile ) >
                        strlen( pszFile ) + strlen( s_szCharMapFileDefault ) )
         {
            zh_strncpy( szFile, pszFile, sizeof( szFile ) - 1 );
            zh_strncat( szFile, s_szCharMapFileDefault, sizeof( szFile ) - 1 );
            nRet = zh_gt_chrmapread( szFile, pszTerm, piTransTbl );
         }
      }
      if( pszFile )
         zh_xfree( pszFile );
      if( nRet == -1 )
         nRet = zh_gt_chrmapread( s_szCharMapFileDefault, pszTerm, piTransTbl );
   }

   if( pszFree )
      zh_xfree( pszFree );

   if( nRet == -1 )
   {
      chrmap_dotctrl( piTransTbl );
      if( fSetACSC )
         chrmap_acscbox( piTransTbl );
      else
         chrmap_ascictrl( piTransTbl );
   }

   return nRet;
}

#if 0
int main( int argc, char ** argv )
{
   int piTransTbl[ 256 ], i;

   if( zh_gt_chrmapinit( piTransTbl, NULL ) == -1 )
   {
      printf( "cannot init charmap.\n" );
      exit( 1 );
   }

   for( i = 0; i < 256; ++i )
      printf( "%3d -> %3d : %d\n", i, piTransTbl[ i ] & 0xff, piTransTbl[ i ] >> 16 );

   return 0;
}
#endif

#endif /* ZH_OS_UNIX || ZH_OS_DOS */
