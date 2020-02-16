/*
 * Version information and build time switches.
 *
 * Copyright 2008-present Przemyslaw Czerpak
 *
 * This file is generated automatically by Ziher preprocessor
 * and is covered by the same license as Ziher PP
 */

#define ZH_VER_ORIGIN_URL        "git@github.com:hernad/ziher"
#define ZH_VER_COMMIT_ID         "45f9dd7503aff53d2510b88ce2bf56fd5e4a2461"
#define ZH_VER_COMMIT_ID_SHORT   "45f9dd75"
#define ZH_VER_COMMIT_YEAR       "2020"
#define ZH_VER_COMMIT_REV        2001051035
#define ZH_VER_COMMIT_INFO       "2020-01-05 11:35:55 +0100"
#define ZH_PLATFORM              "linux"
#define ZH_COMPILER              "gcc"


/* Source repository ID string */
const char * zh_verCommitID( void )
{
   return ZH_VER_COMMIT_ID;
}

/* Source repository ID string (short) */
const char * zh_verCommitIDShort( void )
{
   return ZH_VER_COMMIT_ID_SHORT;
}

/* Source repository revision number */
int zh_verCommitRev( void )
{
   return ZH_VER_COMMIT_REV;
}

/* Last commit string */
const char * zh_verCommitInfo( void )
{
   return ZH_VER_COMMIT_INFO;
}

/* build time C compiler flags in ZH_USER_CFLAGS envvar */
const char * zh_verFlagsC( void )
{
#ifdef ZH_VER_ZH_USER_CFLAGS
   return ZH_VER_ZH_USER_CFLAGS;
#else
   return "";
#endif
}

/* build time linker flags in ZH_USER_LDFLAGS envvar */
const char * zh_verFlagsL( void )
{
#ifdef ZH_VER_ZH_USER_LDFLAGS
   return ZH_VER_ZH_USER_LDFLAGS;
#else
   return "";
#endif
}

/* build time Ziher compiler flags in ZH_USER_PRGFLAGS envvar */
const char * zh_verFlagsPRG( void )
{
#ifdef ZH_VER_ZH_USER_PRGFLAGS
   return ZH_VER_ZH_USER_PRGFLAGS;
#else
   return "";
#endif
}
