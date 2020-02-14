/* NOTE: This file is also used by Ziher .prg code. */

#ifndef ZH_VER_H_
#define ZH_VER_H_

#ifdef __ZIHER__
   #undef __ZIHER__
#endif

#define ZH_VER_MAJOR    4        /* Major version number */
#define ZH_VER_MINOR    8        /* Minor version number */
#define ZH_VER_RELEASE  0        /* Release number */
#define ZH_VER_STATUS   "hernad"    /* Build status (all lowercase) */
#define __ZIHER__     0x040800 /* Three bytes: Major + Minor + Release. This is recommented for 3rd party .c and .prg level code. */

#endif /* ZH_VER_H_ */
