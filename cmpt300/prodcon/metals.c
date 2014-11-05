#include "metals.h"

// --- //

char* metal_to_string(Metal metal) {
        char* names[4] = {
                "NoMetal", "Tin", "Copper", "Lead"
        };

        return names[metal];
}

// There's probably some clever math I could do here.
Alloy make_alloy(Metal m1, Metal m2) {
        Alloy a;

        if((m1 == Tin && m2 == Copper) || (m1 == Copper && m2 == Tin)) {
                a = Bronze;
        } else if((m1 == Tin && m2 == Lead) || (m1 == Lead && m2 == Tin)) {
                a = Solder;
        } else if((m1 == Copper && m2 == Lead) || (m1 == Lead && m2 == Copper)) {
                a = Molybdochalkos;
        } else {
                a = NoAlloy;
        }

        return a;
}
