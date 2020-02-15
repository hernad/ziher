/* Last Translator: mlacecilia (m.lacecilia gmail.com) */

#include "zh_lang_api.h"

static ZH_LANG s_lang =
{
   {
      /* Identification */

      "it",
      "Italian",
      "Italiano",
      "",
      "UTF8",
      "",

      /* Month names */

      "Gennaio",
      "Febbraio",
      "Marzo",
      "Aprile",
      "Maggio",
      "Giugno",
      "Luglio",
      "Agosto",
      "Settembre",
      "Ottobre",
      "Novembre",
      "Dicembre",

      /* Day names */

      "Domenica",
      "Lunedì",
      "Martedì",
      "Mercoledì",
      "Giovedì",
      "Venerdì",
      "Sabato",

      

      "File di dati      # Record     Ultima Mod.     Dimens.",
      "Vuoi altri esempi?",
      "Pagina Nr.",
      "** Subtotale **",
      "* Subsubtotale *",
      "*** Totale ***",
      "Ins",
      "   ",
      "Data non valida",
      "Intervallo: ",
      " - ",
      "Y/N",
      "ESPRESSIONE NON VALIDA",

      /* Error description names */

      "Errore sconosciuto",
      "Parametro errato",
      "Limiti superati",
      "Stringa troppo lunga (overflow)",
      "Numero troppo grande (overflow)",
      "Divisione per zero",
      "Errore numerico",
      "Errore sintattico",
      "Formula troppo complessa",
      "",
      "",
      "Memoria scarsa",
      "Funzione non definita",
      "Metodo non disponibile",
      "Variabile inesistente",
      "Alias inesistente",
      "Variabile non disponibile",
      "Caratteri non ammissibili in un Alias",
      "Alias già in uso",
      "",
      "Errore nella creazione",
      "Errore in apertura",
      "Errore in chiusura",
      "Errore in lettura",
      "Errore in scrittura",
      "Errore in stampa",
      "",
      "",
      "",
      "",
      "Operazione non supportata",
      "Limite superato",
      "Riscontrata corruzione",
      "Errore nel tipo dei dati",
      "Errore nella dimensione dei dati",
      "Workarea non in use",
      "Workarea non indicizzata",
      "Richiede l'uso esclusivo",
      "Richiede un Lock",
      "Scrittura non consentita",
      "Append Lock fallito",
      "Lock fallito",
      "",
      "",
      "",
      "Distruttore dell'oggetto fallito",
      "accesso all'array",
      "assegnazione all'array",
      "dimensione dell'array",
      "non é un array",
      "condizionale",

      /* Internal error names */

      "Errore irrecuperabile %d: ",
      "Recupero dell'errore non riuscito",
      "Manca ERRORBLOCK() per l'errore",
      "Troppe chiamate ricorsive al gestore d'errore",
      "RDD non valido o caricamento non riuscito",
      "Metodo non valido per %s",
      "zh_xgrab non riesce a riservare memoria",
      "zh_xrealloc chiamato con un puntatore nullo",
      "zh_xrealloc chiamato con un puntatore non valido",
      "zh_xrealloc non riesce a modificare la dimensione della memoria riservata",
      "zh_xfree chiamato con un puntatore non valido",
      "zh_xfree chiamato con un puntatore nullo",
      "Non trovo la procedura iniziale: '%s'",
      "Manca una procedura iniziale",
      "VM opcode non supportato",
      "Symbol item atteso per %s",
      "Tipo di simbolo non valido per self da %s",
      "Codeblock atteso per %s",
      "Trovato nello stack un tipo di elemento incongruente nel tentativo di estrarre da %s",
      "Numero insufficiente di elementi nello stack",
      "Tentativo di copia di un elemento su se stesso da %s",
      "Simbolo non valido passato come memvar %s",
      "Overflow del buffer di memoria",
      "zh_xgrab ha richiesto di allocare zero bytes",
      "zh_xrealloc ha richiesto un ridimensionamento di zero bytes",
      "zh_xalloc ha richiesto di allocare zero bytes",

      /* Texts */

      "YYYY/MM/DD",
      "S",
      "N"
   }
};

#define ZH_LANG_ID      IT
#include "zh_msg_reg.h"
