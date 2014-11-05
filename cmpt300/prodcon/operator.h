#ifndef _operator_h_
#define _operator_h_

#include <stdbool.h>

#include "metals.h"

/* Defines */
typedef struct {
        int op_num;
        int tools_taken;
        int max_tools;
        bool active;
        Metal metal1;
        Metal metal2;
        Alloy produced;
} Operator;

/* Forward Declarations */
Operator* operator_create(int op_num, int max_tools);

#endif
