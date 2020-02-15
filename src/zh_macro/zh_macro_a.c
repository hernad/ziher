/* hbexpra.c is also included from ../compiler/expropta.c
 * However it produces a slightly different code if used in
 * macro compiler (there is an additional parameter passed to some functions)
 */

#define ZH_MACRO_SUPPORT

#include "zh_macro.h"
#include "zh_comp.h"

#include "zh_comp/zh_expr_a.h"
