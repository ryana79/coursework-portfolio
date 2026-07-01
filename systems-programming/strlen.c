#include <stdio.h>
#include <stdlib.h>

int main()
{
    char *p = "Hello"; // Pointer to string literal "Hello"
    int i = 0;         // Counter initialized to 0
    while (*p++)       // Loop while *p is non-zero (not null terminator)
        i++;           // Increment i for each non-null character

    printf("i is %d\n", i); // Print final value of i

    return EXIT_SUCCESS;
}

int *p = some_int_array_of_length(n); // Pointer to the first element of an array
int *q = p + n;                       // Pointer to the memory address right after the last element of the array
int i = 0;                            // Initialize `i` to 0
while (p != q)                        // Loop while `p` has not reached `q`
    i += *p++;                        // Add the value at `p` to `i`, then move `p` to the next element



void bsort(void *a, size_t nelems, size_t elemlength, int (*compare)(void *, void *))
{
    for(int i = 0; i < nelems; i++){
        for(int j = 0; j < nelems; j++){
            void *p = a + elemlength * (j - 1);
            void *q = a + elemlength * j;

            if (compare(p, q) > 0){
                //swap
                *t = *p;
                *p = *q;
                *q = *t;
            }
        }
    }

    free(t);
    
}