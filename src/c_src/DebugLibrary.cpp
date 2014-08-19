#include "DebugLibrary.h"
#include <cstdio>
#include "../../includes/Rts.h"

using namespace std;

void print_hvalue(void* hval) {
    StgAP_STACK *stack = (StgAP_STACK*) hval;
    char *ptr = (char*) hval;
    fprintf(stderr, "Pointer value: %p\nStgAP_STACK.size = %lu\n", stack, stack->size);
    fflush(stderr);
}