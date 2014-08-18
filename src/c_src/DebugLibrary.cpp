#include "DebugLibrary.h"
#include <cstdio>

using namespace std;

int test_lib(int x) {
    printf("%d\n", x);
    return -x;
}