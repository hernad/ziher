/* Last Translator: vszakats */

#include "zh_lang_api.h"

static ZH_LANG s_lang =
{
   {
      /* Identification */

      "sk",
      "Slovak",
      "Slovensky",
      "",
      "UTF8",
      "",

      /* Month names */

      "január",
      "február",
      "marec",
      "apríl",
      "máj",
      "jún",
      "júl",
      "august",
      "september",
      "október",
      "november",
      "december",

      /* Day names */

      "nedeľa",
      "pondelok",
      "utorok",
      "streda",
      "štvrtok",
      "piatok",
      "sobota",

      

      "Databáza          # Záznamy    Aktualizácia    Veľkosť",
      "Chcete viac príkladov?",
      "Strana",
      "** Medzisúčet **",
      "* Medzimedzisúčet *",
      "*** Súčet ***",
      "Ins",
      "   ",
      "Chybný dátum",
      "Rozsah: ",
      " - ",
      "A/N",
      "CHYBNÝ VÝRAZ",

      /* Error description names */

      "Neznáma chyba",
      "Chyba argumentu",
      "Chyba medzí",
      "Preplnenie reťazca",
      "Preplnenie čísla",
      "Delenie nulou",
      "Numerická chyba",
      "Syntaktická chyba",
      "Operácia príliš komplexná",
      "",
      "",
      "Nedostatok pamäte",
      "Nedefinovaná funkcia",
      "Neznáma metóda",
      "Premenná neexistuje",
      "Oblasť neexistuje",
      "Neznáma premenná",
      "Nepovolené znaky v oblasti",
      "Oblasť je už použitá",
      "",
      "Chyba vytvorenia",
      "Chyba otvorenia",
      "Chyba zatvorenia",
      "Chyba čítania",
      "Chyba zápisu",
      "Chyba tlače",
      "",
      "",
      "",
      "",
      "Nepodporovaná operácia",
      "Prekročený limit",
      "Index poškodený",
      "Chyba typu dát",
      "Chyba dĺžky dát",
      "Nepoužitá pracovná oblasť",
      "Nezoradená pracovná oblasť",
      "Nutný výhradný prístup",
      "Uzamknutie nutné",
      "Zapis nebol povolený",
      "Zlyhanie uzamknutia pri pridávaní",
      "Zlyhanie uzamknutia",
      "",
      "",
      "",
      "Chyba object deštruktora",
      "prístup k poľu",
      "priradenie k poľu",
      "zmena dimenze poľa",
      "nie je to pole",
      "podmienka",

      /* Internal error names */

      "Neidentifikovateľná chyba %d: ",
      "Zlyhanie identifikácie chyby",
      "Žiadny ERRORBLOCK() pre túto chybu",
      "Príliš veľa rekurzívnych error handler volaní",
      "RDD je neplatný alebo zlyhalo jeho otvorenie",
      "Neplatný typ metódy od %s",
      "zh_xgrab nemôže alkovať pamäť",
      "zh_xrealloc volaný s NULL pointrom",
      "zh_xrealloc volaný s neplatným pointrom",
      "zh_xrealloc nemôže realokovať pamäť",
      "zh_xfree volaný s neplatným pointrom",
      "zh_xfree volaný s NULL pointrom",
      "Nenájdená štartovacia procedúra: '%s'",
      "Štartovacia procedúra neurčená",
      "Nepodporovaný VM opcode",
      "Symbolová položka očakávaná pre %s",
      "Neplatný typ symbolu pre self pre %s",
      "Codeblock očakávaný od %s",
      "Nesprávny typ položky v zásobníku čítaného z %s",
      "Podtečenie zásobníka",
      "Pokus o prekopírovanie položky na samú seba v %s",
      "Neplatná symbolová položka vyhodnotená ako memvar %s",
      "Pretečenie pamäťového bufra",
      "zh_xgrab požiadal o alokáciu nula bytov",
      "zh_xrealloc požiadal zmenšenie na nula bytov",
      "zh_xalloc požiadal o alokáciu nula bytov",

      /* Texts */

      "DD.MM.YYYY",
      "A",
      "N"
   }
};

#define ZH_LANG_ID      SK
#include "zh_lang_register_messages.h"
