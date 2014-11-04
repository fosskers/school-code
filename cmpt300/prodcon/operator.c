#include <stdlib.h>

#include "operator.h"
#include "dbg.h"

// --- //

Operator* operator_create(int op_num, int max_tools) {
        Operator* o = malloc(sizeof(Operator));
        check_mem(o);

        o->op_num      = op_num;
        o->tools_taken = 0;
        o->max_tools   = max_tools;
        o->metal1      = NoMetal;
        o->metal2      = NoMetal;
        o->produced    = NoAlloy;

        return o;

 error:
        return NULL;
}
