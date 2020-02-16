/* Last Translator: zhtest */

#include "zh_lang_api.h"

static ZH_LANG s_lang =
{
   {
      /* Identification */

      "hr",
      "Croatian",
      "Hrvatski",
      "",
      "UTF8",
      "",

      /* Month names */

      "siječanj",
      "veljača",
      "ožujak",
      "travanj",
      "svibanj",
      "lipanj",
      "srpanj",
      "kolovoz",
      "rujan",
      "listopad",
      "studeni",
      "prosinac",

      /* Day names */
      "nedjelja",
      "ponedjeljak",
      "utorak",
      "srijeda",
      "četvrtak",
      "petak",
      "subota",


      "Datot.baze podat. # Zapisi     Zadnja prom.    Vel.",
      "Želite još primjera?",
      "Str.Br.",
      "** Podzbroj **",
      "* Podpodzbroj *",
      "*** Zbroj ***",
      "Ins",
      "   ",
      "Pogrešan podatak",
      "Raspon: ",
      " - ",
      "D/N",
      "POGREŠAN IZRAZ",

      /* Error description names */

      "Nepoznata greška",
      "Pogrešan argument",
      "Pogrešna granica",
      "Prekoračenje niza",
      "Prekoračenje broja",
      "Dijeljenje s nulom",
      "Brojčana greška",
      "Sintaksna greška",
      "Prekomplicirana operacija",
      "",
      "",
      "Nedostatak memorije",
      "Nedefinirana funkcija",
      "Nema eksportne metode",
      "Varijabla ne postoji",
      "Alias ne postoji",
      "Nema izvozne varijable",
      "Nedopušteni znak u aliasu",
      "Alias već u upotrebi",
      "",
      "Greška kreiranja",
      "Greška otvaranja",
      "Greška zatvaranja",
      "Greška čitanja",
      "Greška zapisivanja",
      "Greška ispisa",
      "",
      "",
      "",
      "",
      "Operacija nije podržana",
      "Prekoračenje granice",
      "Otkriven kvar",
      "Tip podatka pogrešan",
      "Dužina podatka pogrešna",
      "Radno područje nije u upotrebi",
      "Radno područje nije indeksirano",
      "potrebno isključiv",
      "Potrebno zaključavanje",
      "Zapisivanje nije dozvoljeno",
      "Izostalo zaključavanje kod dodavanja",
      "Greška zaključavanja",
      "",
      "",
      "",
      "Object destructor failure",
      "pristup matrici",
      "pridruživanje matrici",
      "dimenzija matrice",
      "nije matrica",
      "uvjetan",

      /* Internal error names */

      "Nepopravljiva greška %d: ",
      "Greška obnavljanje neuspješno",
      "Nema ERRORBLOCK() za grešku",
      "Previše povratnih poziva upravljača grešaka",
      "RDD neispravan ili izostalo učitavanje",
      "Neispravan tip metode iz %s",
      "zh_xgrab ne može dodijeliti memoriju",
      "zh_xrealloc pozvan s NULL pokazivačem",
      "zh_xrealloc pozvan s neispravnim pokazivačem",
      "zh_xrealloc ne može realocirati memoriju",
      "zh_xfree pozvan s neispravnim pokazivačem",
      "zh_xfree pozvan s NULL pokazivačem",
      "Nije moguće pronaći početnu proceduru: '%s'",
      "Nema početne procedure",
      "Nepodržan VM opcod",
      "Simbol element očekivan iz %s",
      "Neispravan simbol tip za sebe iz %s",
      "Codeblock očekivan iz %s",
      "Nepravilan tip elementa na stogu pokušaj stavljanja iz %s",
      "Prekoračenje stoga",
      "Element je bio kopiran u samog sebe iz %s",
      "Neispravan simbol element dodan kao memorijska varijabla %s",
      "Prekoračenje memorijskog međuspremnika",
      "zh_xgrab zahtjev za dodjelom nul bajta",
      "zh_xrealloc zahtjev za proširenjem na nul bajtove",
      "zh_xalloc zahtjev za dodjelom nul bajtova",

      /* Texts */

      "DD/MM/YYYY",
      "D",
      "N"
   }
};

#define ZH_LANG_ID      HR
#include "zh_lang_register_messages.h"
