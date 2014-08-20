#include "DebugLibrary.h"
#include <cstdio>
#include "../../includes/Rts.h"

//#include "ffi.h"

using namespace std;

void print_info_table(const void* hval) {
    StgInfoTable *table = (StgInfoTable*) hval;
    fprintf(stderr, "Info pointer value: %p\n", table);
    fprintf(stderr, "Closure type: %d\n", table->type);
    fflush(stderr);
}

void print_ap_stack(const void* hval) {
    char **ptr = ((char**) hval);
    StgAP_STACK *stack = (StgAP_STACK*) *ptr;
    fprintf(stderr, "Pointer value: %p\n", hval);
    fflush(stderr);
    print_info_table(stack->header.info);
}
