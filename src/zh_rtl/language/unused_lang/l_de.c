/* Last Translator: zhtest */

#include "zh_lang_api.h"

static ZH_LANG s_lang =
{
   {
      /* Identification */

      "de",
      "German",
      "Deutsch",
      "",
      "UTF8",
      "",

      /* Month names */

      "Januar",
      "Februar",
      "März",
      "April",
      "Mai",
      "Juni",
      "Juli",
      "August",
      "September",
      "Oktober",
      "November",
      "Dezember",

      /* Day names */

      "Sonntag",
      "Montag",
      "Dienstag",
      "Mittwoch",
      "Donnerstag",
      "Freitag",
      "Samstag",

      

      "Datenbank Dateien Anz. Sätze   Letzte Änderung Größe",
      "Möchten Sie mehr Beispiele?",
      "Anz. Seiten",
      "** Zwischensumme **",
      "* Teilsumme *",
      "*** Summe ***",
      "Einfg.",
      "      ",
      "Ungültiges Datum",
      "Bereich: ",
      " - ",
      "J/N",
      "UNGÜLTIGER AUSDRUCK",

      /* Error description names */

      "Unknown error",
      "Argument error",
      "Bound error",
      "String overflow",
      "Numeric overflow",
      "Zero divisor",
      "Numeric error",
      "Syntax error",
      "Operation too complex",
      "",
      "",
      "Memory low",
      "Undefined function",
      "No exported method",
      "Variable does not exist",
      "Alias does not exist",
      "No exported variable",
      "Illegal characters in alias",
      "Alias already in use",
      "",
      "Create error",
      "Open error",
      "Close error",
      "Read error",
      "Write error",
      "Print error",
      "",
      "",
      "",
      "",
      "Operation not supported",
      "Limit exceeded",
      "Corruption detected",
      "Data type error",
      "Data width error",
      "Workarea not in use",
      "Workarea not indexed",
      "Exclusive required",
      "Lock required",
      "Write not allowed",
      "Append lock failed",
      "Lock Failure",
      "",
      "",
      "",
      "Object destructor failure",
      "array access",
      "array assign",
      "array dimension",
      "not an array",
      "conditional",

      /* Internal error names */

      "Unrecoverable error %d: ",
      "Error recovery failure",
      "No ERRORBLOCK() for error",
      "Too many recursive error handler calls",
      "RDD invalid or failed to load",
      "Invalid method type from %s",
      "zh_xgrab can't allocate memory",
      "zh_xrealloc called with a NULL pointer",
      "zh_xrealloc called with an invalid pointer",
      "zh_xrealloc can't reallocate memory",
      "zh_xfree called with an invalid pointer",
      "zh_xfree called with a NULL pointer",
      "Can't locate the starting procedure: '%s'",
      "No starting procedure",
      "Unsupported VM opcode",
      "Symbol item expected from %s",
      "Invalid symbol type for self from %s",
      "Codeblock expected from %s",
      "Incorrect item type on the stack trying to pop from %s",
      "Stack underflow",
      "An item was going to be copied to itself from %s",
      "Invalid symbol item passed as memvar %s",
      "Memory buffer overflow",
      "zh_xgrab requested to allocate zero bytes",
      "zh_xrealloc requested to resize to zero bytes",
      "zh_xalloc requested to allocate zero bytes",

      /* Texts */

      "DD.MM.YYYY",
      "J",
      "N"
   }
};

#define ZH_LANG_ID      DE
#include "zh_lang_register_messages.h"
