#include "metals.h"

// --- //

char* metal_to_string(Metal metal) {
        char* names[4] = {
                "NoMetal", "Tin", "Copper", "Lead"
        };

        return names[metal];
}
