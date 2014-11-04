#ifndef _metals_h_
#define _metals_h_

#define METAL_TYPES 3

/* Defines */
typedef enum { NoMetal, Tin, Copper, Lead } Metal;

/*
 * Tin + Copper  = Bronze
 * Tin + Lead    = Solder
 * Copper + Lead = Molybdochalkos
 */
typedef enum { NoAlloy, Bronze, Solder, Molybdochalkos } Alloy;

// For passing multiple arguments to a threaded function.
typedef struct {
        Metal metal;
        int max_tools;  // # of tools user chose.
} MetalWrap;

/* Forward Declarations */
char* metal_to_string(Metal);

#endif
