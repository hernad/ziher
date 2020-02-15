/* Last Translator: zhtest */

#include "zh_lang_api.h"

static ZH_LANG s_lang =
{
   {
      /* Identification */

      "cs",
      "Czech",
      "Česky",
      "",
      "UTF8",
      "",

      /* Month names */

      "leden",
      "únor",
      "březen",
      "duben",
      "květen",
      "červen",
      "červenec",
      "srpen",
      "září",
      "říjen",
      "listopad",
      "prosinec",

      /* Day names */

      "neděle",
      "pondělí",
      "úterý",
      "středa",
      "čtvrtek",
      "pátek",
      "sobota",

      

      "Databáze          # Záznamů    Aktualizace     Velikost",
      "Chcete více příkladů?",
      "Strana",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Chybné datum",
      "Rozsah: ",
      " - ",
      "A/N",
      "CHYBNÝ VÝRAZ",

      /* Error description names */

      "Neznámá chyba",
      "Chyba argumentu",
      "Chyba mezí",
      "Přetečení řetězce",
      "Přetečení čísla",
      "Dělení nulou",
      "Numerická chyba",
      "Chyba syntaxe",
      "Operace příliš komplexní",
      "",
      "",
      "Nedostatek paměti",
      "Nedefinovaná funkce",
      "Neznámá metoda",
      "Proměnná neexistuje",
      "Alias neexistuje",
      "Neznámá proměnná",
      "Nepovolené znaky v aliasu",
      "Alias již použit",
      "",
      "Chyba vytvoření",
      "Chyba otevření",
      "Chyba zavření",
      "Chyba čtení",
      "Chyba zápisu",
      "Chyba tisku",
      "",
      "",
      "",
      "",
      "Operace není podporována",
      "Překročen limit",
      "Index poškozen",
      "Typ dat se neshoduje",
      "Chyba šířky dat",
      "Pracovní oblast není použita",
      "Není otevřen index",
      "Požadováno uzamknutí",
      "Zámek při přidání záznamu selhal",
      "Zámek selhal",
      "Append lock failed",
      "Lock Failure",
      "",
      "",
      "",
      "Object destructor failure",
      "přístup k poli",
      "přiřazení pole",
      "změna dimenze pole",
      "není pole",
      "podmínka",

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
      "A",
      "N"
   }
};

#define ZH_LANG_ID      CS
#include "zh_msg_reg.h"
