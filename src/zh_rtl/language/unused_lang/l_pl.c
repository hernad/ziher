/* Last Translator: zhtest */

#include "zh_lang_api.h"

static ZH_LANG s_lang =
{
   {
      /* Identification */

      "pl",
      "Polish",
      "Polski",
      "",
      "UTF8",
      "",

      /* Month names */

      "Styczeń",
      "Luty",
      "Marzec",
      "Kwiecień",
      "Maj",
      "Czerwiec",
      "Lipiec",
      "Sierpień",
      "Wrzesień",
      "Październik",
      "Listopad",
      "Grudzień",

      /* Day names */

      "Niedziela",
      "Poniedziałek",
      "Wtorek",
      "Środa",
      "Czwartek",
      "Piątek",
      "Sobota",

      

      "Baza danych       # Rekordów   Uaktualniona    Rozmiar",
      "Więcej przykładów?",
      "Strona",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Wst",
      "Zas",
      "Nieprawidłowa data",
      "Zakres: ",
      " - ",
      "T/N",
      "Błędne wyrażenie",

      /* Error description names */

      "Błąd bez opisu",
      "Nieprawidłowy argument",
      "Błąd zakresu tablicy",
      "Za duży string",
      "Przepełnienie numeryczne",
      "Dzielenie przez zero",
      "Błąd numeryczny",
      "Nieprawidłowa składnia",
      "Operacja zbyt złożona",
      "",
      "",
      "Za mało pamięci",
      "Niezdefiniowana funkcja",
      "Metoda jest niedostępna",
      "Zmienna nie istnieje",
      "Alias bazy nie istnieje",
      "Zmienna jest niedostępna",
      "Nieprawidłowy alias bazy",
      "Podany alias już istnieje",
      "",
      "Błąd podczas tworzenia zbioru",
      "Błąd podczas otwarcia zbioru",
      "Błąd podczas zamknięcia zbioru",
      "Błąd podczas odczytu ze zbioru",
      "Błąd podczas zapisu do zbioru",
      "Błąd wydruku",
      "",
      "",
      "",
      "",
      "Nieprawidłowa operacja",
      "Przekroczony limit",
      "Wykryto uszkodzenie danych",
      "Niezgodny typ danych",
      "Wartość poza zakresem",
      "Baza jest nie otwarta",
      "Baza nie ma indeksu",
      "Wymagany jest wyłączny dostęp do bazy",
      "Wymagana blokada dostępu",
      "Zapis niedozwolony",
      "Brak blokady dostępu podczas dodawania rekordu",
      "Nie udało się zablokować dostępu",
      "",
      "",
      "",
      "Błąd w destruktorze obiektu",
      "Nieprawidłowa liczba argumentów",
      "pobranie elementu tablicy",
      "zmiana wartości elementu tablicy",
      "wymagana jest tablica",
      "wymagany typ: logiczny",

      /* Internal error names */

      "Nienaprawialny błąd nr %d: ",
      "Nieudana próba naprawy błędu",
      "Brak kodu obsługi ERRORBLOCK()",
      "Zbyt wiele zagnieżdżonych błędów",
      "Niezaładowany lub zły RDD",
      "Zły typ metody wołanej z %s",
      "zh_xgrab nie może zarezerwować pamięci",
      "zh_xrealloc wywołany ze wskaźnikiem NULL",
      "zh_xrealloc wywołany ze złym wskaźnikiem",
      "zh_xrealloc nie może powiększyć bloku pamięci",
      "zh_xfree wywołany ze złym wskaźnikiem",
      "zh_xfree wywołany ze wskaźnikiem NULL",
      "Brak definicji procedury startowej: '%s'",
      "Brak procedury startowej",
      "Nieprawidłowa wartość VM opcode",
      "W %s wymagany jest item typu 'Symbol'",
      "W %s podano zły item dla SELF",
      "W %s oczekiwany jest item typu 'Codeblock'",
      "Funkcja %s wymaga innego typu na stosie",
      "Stos poniżej dna",
      "Item nie może być skopiowany w %s",
      "W %s podano zły item jako memvar",
      "Zapis poza przydzielonym obszarem",
      "zh_xgrab requested to allocate zero bytes",
      "zh_xrealloc requested to resize to zero bytes",
      "zh_xalloc requested to allocate zero bytes",

      /* Texts */

      "YYYY.MM.DD",
      "T",
      "N"
   }
};

#define ZH_LANG_ID      PL
#include "zh_lang_register_messages.h"
