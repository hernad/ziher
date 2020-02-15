/* Last Translator: Alain Aupeix (alain.aupeix wanadoo.fr) */

#include "zh_lang_api.h"

static ZH_LANG s_lang =
{
   {
      /* Identification */

      "fr",
      "French",
      "français",
      "",
      "UTF8",
      "",

      /* Month names */

      "Janvier",
      "Février",
      "Mars",
      "Avril",
      "Mai",
      "Juin",
      "Juillet",
      "Août",
      "Septembre",
      "Octobre",
      "Novembre",
      "Décembre",

      /* Day names */

      "Dimanche",
      "Lundi",
      "Mardi",
      "Mercredi",
      "Jeudi",
      "Vendredi",
      "Samedi",

      

      "Base de données   Nb d'enreg   Date j-m-ae     Taille",
      "Voulez-vous d'autres échantillons?",
      "No. de pages",
      "** Total intermédiaire **",
      "* Total partiel *",
      "*** Total ***",
      "Ins",
      "   ",
      "Date invalide",
      "Limites: ",
      " - ",
      "O/N",
      "EXPRESSION INVALIDE",

      /* Error description names */

      "Erreur inconnue",
      "Argument erroné",
      "Erreur bound",
      "Dépassement de chaîne",
      "Dépassement numérique",
      "Division par zéro",
      "Erreur numérique",
      "Erreur de syntaxe",
      "Opération trop complexe",
      "",
      "",
      "Mémoire basse",
      "Fonction non définie",
      "Aucune méthode exportée",
      "Variable inexistante",
      "Alias inexistant",
      "Aucune variable exportée",
      "Caractère illégal dans l'alias",
      "Alias déjà utilisé",
      "",
      "Erreur à la création",
      "Erreur à l'ouverture",
      "Erreur à la fermeture",
      "Erreur de lecture",
      "Erreur d'écriture",
      "Erreur d'impression",
      "",
      "",
      "",
      "",
      "Opération non supportée",
      "Limite dépassée",
      "Corruption détectée",
      "Erreur de type de données",
      "Erreur de taille de donnée",
      "Zone non en cours d'utilisation",
      "Zone non indexée",
      "Mode exclusif requis",
      "Verrouillage requis",
      "Ecriture non permise",
      "Echec du verrou à l'ajout",
      "Echec du verrou",
      "",
      "",
      "",
      "Echec du destructeur d'objet",
      "accès au tableau",
      "assignation du tableau",
      "dimension du tableau",
      "pas un tableau",
      "conditionnel",

      /* Internal error names */

      "Erreur irrécupérable %d: ",
      "Echec lors de la récupération de l'erreur",
      "Aucun ERRORBLOCK() pour cette erreur",
      "Trop d'appels récursifs au gestionnaire d'erreur",
      "RDD invalide ou echec au chargement",
      "Type de méthode pour %s",
      "allocation mémoire impossible par zh_xgrab",
      "zh_xrealloc appelé avec un pointeur NULL",
      "zh_xrealloc appelé avec un pointeur invalide",
      "zh_xrealloc ne peut réallouer la mémoire",
      "zh_xfree appelé avec un pointeur invalide",
      "zh_xfree appelé avec un pointeur NULL",
      "Allocation impossible à la procédure de démarrage: '%s'",
      "Pas de procédure de démarrage",
      "Code opération non supporté par VM",
      "Elément de symbôle attendu de %s",
      "Type de symbôle invalide pour self de %s",
      "Codeblock attendu de %s",
      "Tentavive de dépilement d'un type d'élément incorrect de %s",
      "Débordement de pile",
      "Un élément allait être copié dur lui-même de %s",
      "Symbol d'élément invalide passé comme memvar %s",
      "Débordement du buffer mémoire",
      "zh_xgrab appelé pour allouer zéro octets",
      "zh_xrealloc appelé pour allouer zéro octets",
      "zh_xalloc appelé pour allouer zéro octets",

      /* Texts */

      "DD-MM-YYYY",
      "O",
      "N"
   }
};

#define ZH_LANG_ID      FR
#include "zh_msg_reg.h"
