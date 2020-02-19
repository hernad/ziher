/* Last Translator: zhtest */

#include "zh_lang_api.h"

static ZH_LANG s_lang =
{
   {
      /* Identification */

      "is",
      "Icelandic",
      "Íslenska",
      "",
      "UTF8",
      "",

      /* Month names */

      "Janúar",
      "Febrúar",
      "Mars",
      "Apríl",
      "Maí",
      "Júní",
      "Júlí",
      "Ágúst",
      "September",
      "Október",
      "Nóvember",
      "Desember",

      /* Day names */

      "Sunnudagur",
      "Mánudagur",
      "Þriðjudagur",
      "Miðvikudagur",
      "Fimmtudagur",
      "Föstudagur",
      "Laugardagur",

      

      "Gagnagrunnar      Skráartal    Síðast Breytt   Stærð",
      "Viltu fleiri prófanir?",
      "Blaðsíða",
      "** Millisamtala **",
      "* Milli milli samtala *",
      "*** Samtals ***",
      "Ins",
      "   ",
      "Röng dagsetning",
      "Bil: ",
      " - ",
      "J/N",
      "ÓGILD EXPRESSION",

      /* Error description names */

      "Óþekkt villa",
      "Argument error",
      "Bound error",
      "String overflow",
      "Of há tala",
      "Deilt með núll",
      "Numeric error",
      "Syntax error",
      "Operation too complex",
      "",
      "",
      "Lágt minni",
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
      "Les villa",
      "Skrif villa",
      "Prent villa",
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

#define ZH_LANG_ID      IS
#include "zh_lang_register_messages.h"
