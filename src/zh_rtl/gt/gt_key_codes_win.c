/*
 * zh_gt_dos_keyCodeTranslate()
 *       function used by MS-DOS, Windows and OS/2 ports of few GTs which
 *       use getkey()/getch()/_read_kbd()/KbdCharIn() or similar function
 *       for keyboard input
 *
 * Copyright 2006, 2015 Przemyslaw Czerpak
 * based on zh_gt_ReadKey() from GTDOS code by:
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

#include "zh_api.h"
#include "zh_gt_core.h"

#if defined( ZH_OS_WIN )

int zh_gt_dos_keyCodeTranslate( int iKey, int iFlags, PZH_CODEPAGE cdp )
{
   int iKeyPad = iFlags & ZH_KF_KEYPAD;

   iFlags &= ( ZH_KF_SHIFT | ZH_KF_CTRL | ZH_KF_ALT );

   /* Perform key translations */
   switch( iKey )
   {
      case 0:
      case -1:    /* No key available */
         return 0;

      case 8:     /* BackSpace or CTRL + H */
         iKey = iFlags & ZH_KF_CTRL ? 'H' : ZH_KX_BS;
         break;

      case 9:     /* Tab or CTRL + I */
         iKey = iFlags & ZH_KF_CTRL ? 'I' : ZH_KX_TAB;
         break;

      case 10:    /* CTRL + ENTER or CTRL + J - we cannot guess :( */
         iKey = ZH_KX_ENTER;
         iFlags |= ZH_KF_CTRL;
         break;

      case 13:    /* ENTER or CTRL + M */
         iKey = iFlags & ZH_KF_CTRL ? 'M' : ZH_KX_ENTER;
         break;

      case 127:   /* CTRL + BackSpace */
         iKey = ZH_KX_BS;
         iFlags |= ZH_KF_CTRL;
         break;

      case 259:   /* CTRL + "@" */
         iKey = '@';
         iFlags |= ZH_KF_CTRL;
         break;

      case 270:   /* ALT + BackSpace */
         iKey = ZH_KX_BS;
         iFlags |= ZH_KF_ALT;
         break;

      case 271:   /* SHIFT + TAB */
         iKey = ZH_KX_TAB;
         iFlags |= ZH_KF_SHIFT;
         break;

      case 404:   /* CTRL + TAB */
         iKey = ZH_KX_TAB;
         iFlags |= ZH_KF_CTRL;
         break;

      case 272:   /* Alt + Q */
         iKey = 'Q';
         iFlags |= ZH_KF_ALT;
         break;
      case 273:   /* Alt + W */
         iKey = 'W';
         iFlags |= ZH_KF_ALT;
         break;
      case 274:   /* Alt + E */
         iKey = 'E';
         iFlags |= ZH_KF_ALT;
         break;
      case 275:   /* Alt + R */
         iKey = 'R';
         iFlags |= ZH_KF_ALT;
         break;
      case 276:   /* Alt + T */
         iKey = 'T';
         iFlags |= ZH_KF_ALT;
         break;
      case 277:   /* Alt + Y */
         iKey = 'Y';
         iFlags |= ZH_KF_ALT;
         break;
      case 278:   /* Alt + U */
         iKey = 'U';
         iFlags |= ZH_KF_ALT;
         break;
      case 279:   /* Alt + I */
         iKey = 'I';
         iFlags |= ZH_KF_ALT;
         break;
      case 280:   /* Alt + O */
         iKey = 'O';
         iFlags |= ZH_KF_ALT;
         break;
      case 281:   /* Alt + P */
         iKey = 'P';
         iFlags |= ZH_KF_ALT;
         break;
      case 282:   /* Alt + [ */
         iKey = '[';
         iFlags |= ZH_KF_ALT;
         break;
      case 283:   /* Alt + ] */
         iKey = ']';
         iFlags |= ZH_KF_ALT;
         break;
      case 284:   /* ALT + ENTER */
         iKey = ZH_KX_ENTER;
         iFlags |= ZH_KF_ALT;
         break;

      case 286:   /* Alt + A */
         iKey = 'A';
         iFlags |= ZH_KF_ALT;
         break;
      case 287:   /* Alt + S */
         iKey = 'S';
         iFlags |= ZH_KF_ALT;
         break;
      case 288:   /* Alt + D */
         iKey = 'D';
         iFlags |= ZH_KF_ALT;
         break;
      case 289:   /* Alt + F */
         iKey = 'F';
         iFlags |= ZH_KF_ALT;
         break;
      case 290:   /* Alt + G */
         iKey = 'G';
         iFlags |= ZH_KF_ALT;
         break;
      case 291:   /* Alt + H */
         iKey = 'H';
         iFlags |= ZH_KF_ALT;
         break;
      case 292:   /* Alt + J */
         iKey = 'J';
         iFlags |= ZH_KF_ALT;
         break;
      case 293:   /* Alt + K */
         iKey = 'K';
         iFlags |= ZH_KF_ALT;
         break;
      case 294:   /* Alt + L */
         iKey = 'L';
         iFlags |= ZH_KF_ALT;
         break;
      case 295:   /* Alt + ; */
         iKey = ';';
         iFlags |= ZH_KF_ALT;
         break;
      case 296:   /* Alt + ' */
         iKey = '\'';
         iFlags |= ZH_KF_ALT;
         break;
      case 297:   /* Alt + ` */
         iKey = '`';
         iFlags |= ZH_KF_ALT;
         break;
      case 299:   /* Alt + \ */
         iKey = '\\';
         iFlags |= ZH_KF_ALT;
         break;

      case 300:   /* Alt + Z */
         iKey = 'Z';
         iFlags |= ZH_KF_ALT;
         break;
      case 301:   /* Alt + X */
         iKey = 'X';
         iFlags |= ZH_KF_ALT;
         break;
      case 302:   /* Alt + C */
         iKey = 'C';
         iFlags |= ZH_KF_ALT;
         break;
      case 303:   /* Alt + V */
         iKey = 'V';
         iFlags |= ZH_KF_ALT;
         break;
      case 304:   /* Alt + B */
         iKey = 'B';
         iFlags |= ZH_KF_ALT;
         break;
      case 305:   /* Alt + N */
         iKey = 'N';
         iFlags |= ZH_KF_ALT;
         break;
      case 306:   /* Alt + M */
         iKey = 'M';
         iFlags |= ZH_KF_ALT;
         break;
      case 307:   /* Alt + , */
         iKey = ',';
         iFlags |= ZH_KF_ALT;
         break;
      case 308:   /* Alt + . */
         iKey = '.';
         iFlags |= ZH_KF_ALT;
         break;
      case 309:   /* Alt + / */
         iKey = '/';
         iFlags |= ZH_KF_ALT;
         break;

      case 376:   /* Alt + 1 */
      case 377:   /* Alt + 2 */
      case 378:   /* Alt + 3 */
      case 379:   /* Alt + 4 */
      case 380:   /* Alt + 5 */
      case 381:   /* Alt + 6 */
      case 382:   /* Alt + 7 */
      case 383:   /* Alt + 8 */
      case 384:   /* Alt + 9 */
         iKey = iKey - ( 376 - '1' );
         iFlags |= ZH_KF_ALT;
         break;
      case 385:   /* Alt + 0 */
         iKey = '0';
         iFlags |= ZH_KF_ALT;
         break;
      case 386:   /* Alt + - */
         iKey = '-';
         iFlags |= ZH_KF_ALT;
         break;
      case 387:   /* Alt + = */
         iKey = '=';
         iFlags |= ZH_KF_ALT;
         break;

      case 421:   /* ALT + TAB */
         iKey = ZH_KX_TAB;
         iFlags |= ZH_KF_ALT;
         break;

      case 327:   /* Home */
         iKey = ZH_KX_HOME;
         iFlags |= iKeyPad;
         break;
      case 328:   /* Up arrow */
         iKey = ZH_KX_UP;
         iFlags |= iKeyPad;
         break;
      case 329:   /* Page Up */
         iKey = ZH_KX_PGUP;
         iFlags |= iKeyPad;
         break;
      case 331:   /* Left arrow */
         iKey = ZH_KX_LEFT;
         iFlags |= iKeyPad;
         break;
      case 333:   /* Right arrow */
         iKey = ZH_KX_RIGHT;
         iFlags |= iKeyPad;
         break;
      case 335:   /* End */
         iKey = ZH_KX_END;
         iFlags |= iKeyPad;
         break;
      case 336:   /* Down arrow */
         iKey = ZH_KX_DOWN;
         iFlags |= iKeyPad;
         break;
      case 337:   /* Page Down */
         iKey = ZH_KX_PGDN;
         iFlags |= iKeyPad;
         break;
      case 338:   /* Insert */
         iKey = ZH_KX_INS;
         iFlags |= iKeyPad;
         break;
      case 339:   /* Delete */
         iKey = ZH_KX_DEL;
         iFlags |= iKeyPad;
         break;

      case 370:   /* Ctrl + Print */
         iKey = ZH_KX_PRTSCR;
         iFlags |= ZH_KF_CTRL;
         break;

      case 371:   /* Ctrl + Left arrow */
         iKey = ZH_KX_LEFT;
         iFlags |= ZH_KF_CTRL | iKeyPad;
         break;
      case 372:   /* Ctrl + Right arrow */
         iKey = ZH_KX_RIGHT;
         iFlags |= ZH_KF_CTRL | iKeyPad;
         break;
      case 373:   /* Ctrl + End */
         iKey = ZH_KX_END;
         iFlags |= ZH_KF_CTRL | iKeyPad;
         break;
      case 374:   /* Ctrl + Page Down */
         iKey = ZH_KX_PGDN;
         iFlags |= ZH_KF_CTRL | iKeyPad;
         break;
      case 375:   /* Ctrl + Home */
         iKey = ZH_KX_HOME;
         iFlags |= ZH_KF_CTRL | iKeyPad;
         break;
      case 388:   /* Ctrl + Page Up */
         iKey = ZH_KX_PGUP;
         iFlags |= ZH_KF_CTRL | iKeyPad;
         break;
      case 397:   /* Ctrl + Up arrow */
         iKey = ZH_KX_UP;
         iFlags |= ZH_KF_CTRL | iKeyPad;
         break;
      case 401:   /* Ctrl + Down arrow */
         iKey = ZH_KX_DOWN;
         iFlags |= ZH_KF_CTRL | iKeyPad;
         break;

      case 315:   /* F1 */
      case 316:   /* F2 */
      case 317:   /* F3 */
      case 318:   /* F4 */
      case 319:   /* F5 */
      case 320:   /* F6 */
      case 321:   /* F7 */
      case 322:   /* F8 */
      case 323:   /* F9 */
      case 324:   /* F10 */
         iKey = iKey - ( 315 - ZH_KX_F1 );
         break;
      case 389:   /* F11 */
         iKey = ZH_KX_F11;
         break;
      case 390:   /* F12 */
         iKey = ZH_KX_F12;
         break;

      case 340:   /* Shift + F1 */
      case 341:   /* Shift + F2 */
      case 342:   /* Shift + F3 */
      case 343:   /* Shift + F4 */
      case 344:   /* Shift + F5 */
      case 345:   /* Shift + F6 */
      case 346:   /* Shift + F7 */
      case 347:   /* Shift + F8 */
      case 348:   /* Shift + F9 */
      case 349:   /* Shift + F10 */
         iKey = iKey - ( 340 - ZH_KX_F1 );
         iFlags |= ZH_KF_SHIFT;
         break;
      case 391:   /* Shift + F11 */
         iKey = ZH_KX_F11;
         iFlags |= ZH_KF_SHIFT;
         break;
      case 392:   /* Shift + F12 */
         iKey = ZH_KX_F12;
         iFlags |= ZH_KF_SHIFT;
         break;

      case 350:   /* Ctrl + F1 */
      case 351:   /* Ctrl + F2 */
      case 352:   /* Ctrl + F3 */
      case 353:   /* Ctrl + F4 */
      case 354:   /* Ctrl + F5 */
      case 355:   /* Ctrl + F6 */
      case 356:   /* Ctrl + F7 */
      case 357:   /* Ctrl + F8 */
      case 358:   /* Ctrl + F9 */
      case 359:   /* Ctrl + F10 */
         iKey = iKey - ( 350 - ZH_KX_F1 );
         iFlags |= ZH_KF_CTRL;
         break;
      case 393:   /* Ctrl + F11 */
         iKey = ZH_KX_F11;
         iFlags |= ZH_KF_CTRL;
         break;
      case 394:   /* Ctrl + F12 */
         iKey = ZH_KX_F12;
         iFlags |= ZH_KF_CTRL;
         break;

      case 360:   /* Alt + F1 */
      case 361:   /* Alt + F2 */
      case 362:   /* Alt + F3 */
      case 363:   /* Alt + F4 */
      case 364:   /* Alt + F5 */
      case 365:   /* Alt + F6 */
      case 366:   /* Alt + F7 */
      case 367:   /* Alt + F8 */
      case 368:   /* Alt + F9 */
      case 369:   /* Alt + F10 */
         iKey = iKey - ( 360 - ZH_KX_F1 );
         iFlags |= ZH_KF_ALT;
         break;
      case 395:   /* Alt + F11 */
         iKey = ZH_KX_F11;
         iFlags |= ZH_KF_ALT;
         break;
      case 396:   /* Alt + F12 */
         iKey = ZH_KX_F12;
         iFlags |= ZH_KF_ALT;
         break;

      case 332:
         iKey = ZH_KX_CENTER;
         iFlags |= ZH_KF_KEYPAD;
         break;

      case 311:
         iKey = '*';
         iFlags |= ZH_KF_KEYPAD | ZH_KF_ALT;
         break;
      case 330:
         iKey = '-';
         iFlags |= ZH_KF_KEYPAD | ZH_KF_ALT;
         break;
      case 334:
         iKey = '+';
         iFlags |= ZH_KF_KEYPAD | ZH_KF_ALT;
         break;

      case 398:
         iKey = '-';
         iFlags |= ZH_KF_KEYPAD | ZH_KF_CTRL;
         break;
      case 399:
         iKey = ZH_KX_CENTER;
         iFlags |= ZH_KF_KEYPAD | ZH_KF_CTRL;
         break;
      case 400:
         iKey = '+';
         iFlags |= ZH_KF_KEYPAD | ZH_KF_CTRL;
         break;
      case 402:
         iKey = ZH_KX_INS;
         iFlags |= ZH_KF_CTRL | iKeyPad;
         break;
      case 403:
         iKey = ZH_KX_DEL;
         iFlags |= ZH_KF_CTRL | iKeyPad;
         break;
      case 405:
         iKey = '/';
         iFlags |= ZH_KF_KEYPAD | ZH_KF_CTRL;
         break;
      case 406:
         iKey = '*';
         iFlags |= ZH_KF_KEYPAD | ZH_KF_CTRL;
         break;

      case 407:
         iKey = ZH_KX_HOME;
         iFlags |= ZH_KF_ALT;
         break;
      case 408:
         iKey = ZH_KX_UP;
         iFlags |= ZH_KF_ALT;
         break;
      case 409:
         iKey = ZH_KX_PGUP;
         iFlags |= ZH_KF_ALT;
         break;
      case 411:
         iKey = ZH_KX_LEFT;
         iFlags |= ZH_KF_ALT;
         break;
      case 413:
         iKey = ZH_KX_RIGHT;
         iFlags |= ZH_KF_ALT;
         break;
      case 415:
         iKey = ZH_KX_END;
         iFlags |= ZH_KF_ALT;
         break;
      case 416:
         iKey = ZH_KX_DOWN;
         iFlags |= ZH_KF_ALT;
         break;
      case 417:
         iKey = ZH_KX_PGDN;
         iFlags |= ZH_KF_ALT;
         break;
      case 418:
         iKey = ZH_KX_INS;
         iFlags |= ZH_KF_ALT;
         break;
      case 419:
         iKey = ZH_KX_DEL;
         iFlags |= ZH_KF_ALT;
         break;

      case 420:
         iKey = '/';
         iFlags |= ZH_KF_KEYPAD | ZH_KF_ALT;
         break;
      case 422:
         iKey = ZH_KX_ENTER;
         iFlags |= ZH_KF_KEYPAD | ZH_KF_ALT;
         break;

      case 583:
         iKey = ZH_KX_HOME;
         break;
      case 584:
         iKey = ZH_KX_UP;
         break;
      case 585:
         iKey = ZH_KX_PGUP;
         break;
      case 587:
         iKey = ZH_KX_LEFT;
         break;
      case 589:
         iKey = ZH_KX_RIGHT;
         break;
      case 591:
         iKey = ZH_KX_END;
         break;
      case 592:
         iKey = ZH_KX_DOWN;
         break;
      case 593:
         iKey = ZH_KX_PGDN;
         break;
      case 594:
         iKey = ZH_KX_INS;
         break;
      case 595:
         iKey = ZH_KX_DEL;
         break;

      case 627:
         iKey = ZH_KX_LEFT;
         iFlags |= ZH_KF_CTRL;
         break;
      case 628:
         iKey = ZH_KX_RIGHT;
         iFlags |= ZH_KF_CTRL;
         break;
      case 629:
         iKey = ZH_KX_END;
         iFlags |= ZH_KF_CTRL;
         break;
      case 630:
         iKey = ZH_KX_PGDN;
         iFlags |= ZH_KF_CTRL;
         break;
      case 631:
         iKey = ZH_KX_HOME;
         iFlags |= ZH_KF_CTRL;
         break;
      case 644:
         iKey = ZH_KX_PGUP;
         iFlags |= ZH_KF_CTRL;
         break;
      case 653:
         iKey = ZH_KX_UP;
         iFlags |= ZH_KF_CTRL;
         break;
      case 657:
         iKey = ZH_KX_DOWN;
         iFlags |= ZH_KF_CTRL;
         break;
      case 658:
         iKey = ZH_KX_INS;
         iFlags |= ZH_KF_CTRL;
         break;
      case 659:
         iKey = ZH_KX_DEL;
         iFlags |= ZH_KF_CTRL;
         break;

      default:
         if( iKey >= 0 && iKey < 32 && ( iFlags & ZH_KF_CTRL ) != 0 )
         {
            iFlags |= ZH_KF_CTRL;
            iKey += 'A' - 1;
         }
         else if( iKey <= 255 &&
                  ( iKey >= 128 || ( iFlags & ( ZH_KF_CTRL | ZH_KF_ALT ) ) == 0 ) )
         {
            if( cdp )
            {
               int uc = zh_cdpGetWC( cdp, ( ZH_UCHAR ) ( iKey ), 0 );
               if( uc )
                  return ZH_INKEY_NEW_UNICODEF( uc, iFlags );
            }
            return ZH_INKEY_NEW_CHARF( iKey, iFlags );
         }
         else
            return iKey;
   }

   return ZH_INKEY_NEW_KEY( iKey, iFlags );
}

#endif /* ZH_OS_WIN */
