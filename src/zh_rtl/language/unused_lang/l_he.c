/* Last Translator: zhtest */

#include "zh_lang_api.h"

static ZH_LANG s_lang =
{
   {
      /* Identification */

      "he",
      "Hebrew",
      "עברית",
      "",
      "UTF8",
      "",

      /* Month names */

      "ינואר",
      "פברואר",
      "מרץ",
      "אפריל",
      "מאי",
      "יוני",
      "יולי",
      "אוגוסט",
      "ספטמבר",
      "אוקטובר",
      "נובמבר",
      "דצמבר",

      /* Day names */

      "ראשון",
      "שני",
      "שלישי",
      "רביעי",
      "חמישי",
      "שישי",
      "שבת",

      

      "גודל     עדכון אחרון   מס' רשומות       קבצי נתונים",
      "האם ברצונך דוגמאות נוספות ?",
      "עמוד מס'",
      "** סיכום ביניים **",
      "* סיכום מישנה *",
      "*** סה\"כ ***",
      "Ins",
      "   ",
      "תאריך שגוי",
      " :תחום מותר",
      " - ",
      "כ/ל",
      "INVALID EXPRESSION",

      /* Error description names */

      "שגיאה לא ידועה",
      "Argument error",
      "Bound error",
      "String overflow",
      "Numeric overflow",
      "חלוקה באפס",
      "Numeric error",
      "שגיאת תחביר",
      "Operation too complex",
      "",
      "",
      "אין מספיק זכרון",
      "פונקציה לא מוגדרת",
      "No exported method",
      "משתנה לא קיים",
      "Alias does not exist",
      "No exported variable",
      "Illegal characters in alias",
      "Alias already in use",
      "",
      "שגיאה בזמן יצירת קובץ",
      "שגיאה בזמן פתיחה",
      "שגיאה בזמן סגירה",
      "שגיאה בזמן קריאה",
      "שגיאה בזמן כתיבה",
      "שגיאת הדפסה",
      "",
      "",
      "",
      "",
      "פעולה זאת אינה נתמכת",
      "Limit exceeded",
      "אינדקס משובש או לא תקין",
      "Data type error",
      "Data width error",
      "Workarea not in use",
      "Workarea not indexed",
      "Exclusive required",
      "דרושה נעילה",
      "פעולת כתיבה אסורה",
      "Append lock failed",
      "פעולת נעילה נכשלה",
      "",
      "",
      "",
      "Object destructor failure",
      "גישה למערך",
      "array assign",
      "גודל מערך שגוי",
      "לא מערך",
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

      "DD-MM-YYYY",
      "כ",
      "ל"
   }
};

#define ZH_LANG_ID      HE
#include "zh_lang_register_messages.h"
