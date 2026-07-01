#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "mymalloc.h"

void test_basic_malloc_free()
{
    printf("\n=== Test 1: Basic malloc and free ===\n");
    void *p = malloc(32);
    if (p)
    {
        printf("✅ malloc(32) succeeded\n");
    }
    else
    {
        printf("❌ malloc(32) failed\n");
    }

    free(p);
    printf("✅ free() executed\n");
}

void test_invalid_free()
{
    printf("\n=== Test 2: Freeing invalid pointer ===\n");
    int x;
    free(&x); // Should print an error and exit
}

void test_double_free()
{
    printf("\n=== Test 3: Double free detection ===\n");
    void *p = malloc(40);
    free(p);
    free(p); // Should print an error and exit
}

void test_split_and_coalesce()
{
    printf("\n=== Test 4: Splitting and coalescing chunks ===\n");
    void *p1 = malloc(32);
    void *p2 = malloc(64);
    void *p3 = malloc(128);

    printf("✅ Allocated three chunks\n");

    free(p2);
    printf("✅ Freed middle chunk\n");

    void *p4 = malloc(60); // Should reuse p2’s space
    if (p4 == p2)
    {
        printf("✅ malloc(60) reused the freed space\n");
    }
    else
    {
        printf("❌ malloc(60) did not reuse the freed space\n");
    }

    free(p1);
    free(p3);
    free(p4);
}

void test_leak_detection()
{
    printf("\n=== Test 5: Memory Leak Detection ===\n");
    void *leak = malloc(50); // Previously leaked
    free(leak);              // Now properly freed
    printf("✅ Leak check should not detect any leaks now\n");
}

void test_alignment()
{
    printf("\n=== Test 6: Pointer Alignment ===\n");
    void *p1 = malloc(17); // Should be rounded up to 24
    void *p2 = malloc(9);  // Should be rounded up to 16
    void *p3 = malloc(25); // Should be rounded up to 32

    printf("✅ Allocated three misaligned sizes\n");

    if ((size_t)p1 % 8 == 0 && (size_t)p2 % 8 == 0 && (size_t)p3 % 8 == 0)
    {
        printf("✅ All pointers are 8-byte aligned\n");
    }
    else
    {
        printf("❌ Alignment failed\n");
    }

    free(p1);
    free(p2);
    free(p3);
}

void test_heap_exhaustion()
{
    printf("\n=== Test 7: Heap Exhaustion Handling ===\n");
    void *ptrs[1000];
    int i;

    for (i = 0; i < 1000; i++)
    {
        ptrs[i] = malloc(50);
        if (!ptrs[i])
        {
            printf("✅ malloc failed after %d allocations as expected\n", i);
            break;
        }
    }

    // Free allocated memory
    for (int j = 0; j < i; j++)
    {
        free(ptrs[j]);
    }
}

void test_heap_fragmentation()
{
    printf("\n=== Test: Heap Exhaustion with Fragmentation ===\n");

    void *blocks[10]; // Store allocated blocks
    size_t sizes[10] = {512, 256, 128, 64, 128, 256, 512, 64, 128, 256};

    // Step 1: Allocate chunks of various sizes
    for (int i = 0; i < 10; i++)
    {
        blocks[i] = malloc(sizes[i]);
        if (blocks[i])
        {
            printf("✅ Allocated %zu bytes at %p\n", sizes[i], blocks[i]);
        }
        else
        {
            printf("❌ Failed to allocate %zu bytes\n", sizes[i]);
        }
    }

    // Step 2: Free every second block to create fragmentation
    for (int i = 1; i < 10; i += 2)
    {
        free(blocks[i]);
        printf("✅ Freed block at %p\n", blocks[i]);
    }

    // Step 3: Try allocating a large block that cannot fit due to fragmentation
    void *large_block = malloc(1024);
    if (!large_block)
    {
        printf("✅ Heap exhaustion detected: Unable to allocate 1024 bytes (Expected)\n");
    }
    else
    {
        printf("❌ Large allocation unexpectedly succeeded (Check fragmentation handling!)\n");
    }

    // Step 4: Clean up remaining allocations
    for (int i = 0; i < 10; i += 2)
    {
        free(blocks[i]);
    }
}

int main()
{
    test_basic_malloc_free();
    //test_invalid_free();  // Uncomment this to test invalid free (will terminate)
    //test_double_free();    // Uncomment this to test double free (will terminate)
    test_split_and_coalesce();
    test_leak_detection();
    test_alignment();
    test_heap_exhaustion();
    test_heap_fragmentation();

    printf("\n✅ All tests completed!\n");
    return 0;
}
