#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "FibC_stub.h"
extern void __stginit_Safe(void);
#endif
#include <stdio.h>

int main(int argc, char *argv[])
{
    int i;
    hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_Safe);
#endif

    i = fibonacci_hs(42);
    printf("Fibonacci: %d\n", i);

    hs_exit();
    return 0;
}
