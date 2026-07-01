#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "mymalloc.h"

#define MEMLENGTH 4096
#define MIN_CHUNK_SIZE 16 // Minimum size for any chunk (header + payload)

// Defines the heap (ensuring proper 8-byte alignment)
static union
{
    char bytes[MEMLENGTH];
    double force_align; // Ensures proper alignment
} heap;

// Structure for chunk metadata
// 'size' represents the total size of the chunk (header + payload)
typedef struct chunk
{
    size_t size;
    int free; // 1 if free, 0 if allocated
} chunk_t;

static int initialized = 0;

// Rounds up size to the nearest multiple of 8
size_t align_size(size_t size)
{
    return (size + 7) & ~7;
}

// Initialize heap with a single free chunk spanning the entire heap
void initialize_heap()
{
    chunk_t *first_chunk = (chunk_t *)heap.bytes;
    first_chunk->size = MEMLENGTH; // Total size includes header and payload
    first_chunk->free = 1;
    initialized = 1;
}

// Traverses the heap to find the best-fit free chunk for a given payload size
// total required size = payload size + header size.
chunk_t *find_best_fit_chunk(size_t payload_size)
{
    size_t total_required = payload_size + sizeof(chunk_t);
    chunk_t *curr = (chunk_t *)heap.bytes;
    chunk_t *best_fit = NULL;

    while ((char *)curr < heap.bytes + MEMLENGTH)
    {
        if ((char *)curr + sizeof(chunk_t) > heap.bytes + MEMLENGTH)
            break; 

        if (curr->free && curr->size >= total_required)
        {
            // Ensure there's enough space for a valid leftover chunk
            size_t remaining_space = curr->size - total_required;
            if (!best_fit || curr->size < best_fit->size)
            {
                if (remaining_space == 0 || remaining_space >= MIN_CHUNK_SIZE) // Prevent unusable fragments
                {
                    best_fit = curr;
                }
            }
        }
        curr = (chunk_t *)((char *)curr + curr->size);
    }
    return best_fit;
}

// Split a free chunk if there is enough space to create a new free chunk
// 'total_required' is the total size needed for allocation (payload + header)
void split_chunk(chunk_t *chunk, size_t total_required)
{
    // If the remaining space is big enough to hold a new chunk of at least MIN_CHUNK_SIZE,
    // then perform the split.
    if (chunk->size - total_required >= MIN_CHUNK_SIZE)
    {
        chunk_t *new_chunk = (chunk_t *)((char *)chunk + total_required);
        new_chunk->size = chunk->size - total_required;
        new_chunk->free = 1;
        chunk->size = total_required;
    }
    chunk->free = 0; // Mark the allocated chunk as not free
}

// Coalesce adjacent free chunks to reduce fragmentation
void coalesce()
{
    chunk_t *curr = (chunk_t *)heap.bytes;

    while ((char *)curr < heap.bytes + MEMLENGTH)
    {
        chunk_t *next = (chunk_t *)((char *)curr + curr->size);

        if ((char *)next >= heap.bytes + MEMLENGTH)
            break;

        if (curr->free && next->free)
        {
            curr->size += next->size; // Merge next into current
            continue;                 // Restart merging from current chunk
        }
        curr = next; // Move forward
    }
}

// For debugging: print the status of the heap
void print_heap_status()
{
    chunk_t *curr = (chunk_t *)heap.bytes;
    printf("Heap Status:\n");
    while ((char *)curr < heap.bytes + MEMLENGTH)
    {
        printf("[Chunk at %p] Total Size: %zu, Free: %d\n",
               (void *)curr, curr->size, curr->free);
        curr = (chunk_t *)((char *)curr + curr->size);
    }
    printf("\n");
}

void *mymalloc(size_t size, char *file, int line)
{
    if (size == 0)
    {
        fprintf(stderr, "malloc: Invalid size 0 (%s:%d)\n", file, line);
        return NULL;
    }

    if (!initialized)
    {
        initialize_heap();
    }

    size_t asize = align_size(size);                 // Align payload size
    size_t total_required = asize + sizeof(chunk_t); // Total chunk size

    // Coalesce free chunks before searching
    coalesce();

    chunk_t *chunk = find_best_fit_chunk(asize);
    if (!chunk)
    {
        fprintf(stderr, "malloc: Unable to allocate %zu bytes (%s:%d)\n", asize, file, line);
        return NULL;
    }

    // Ensure that we are not exceeding the available heap memory
    if ((size_t)((char *)heap.bytes + (size_t)MEMLENGTH - (char *)chunk) < total_required)
    {
        fprintf(stderr, "malloc: Not enough space for %zu bytes (%s:%d)\n", asize, file, line);
        return NULL;
    }

    // Split the chunk if possible
    split_chunk(chunk, total_required);

    // Return pointer to the payload (excluding the chunk header)
    return (void *)((char *)chunk + sizeof(chunk_t));
}

void myfree(void *ptr, char *file, int line)
{
    if (!initialized)
    {
        initialize_heap();
    }

    if (!ptr)
    {
        fprintf(stderr, "free: Inappropriate pointer (%s:%d)\n", file, line);
        exit(2);
    }

    // Get the pointer to the chunk header
    chunk_t *chunk = (chunk_t *)((char *)ptr - sizeof(chunk_t));

    // Validate that the chunk lies within the heap bounds
    if ((char *)chunk < heap.bytes || (char *)chunk >= heap.bytes + MEMLENGTH)
    {
        fprintf(stderr, "free: Inappropriate pointer (%s:%d)\n", file, line);
        exit(2);
    }

    // Check if the pointer is aligned to 8 bytes
    if (((uintptr_t)chunk - (uintptr_t)heap.bytes) % 8 != 0)
    {
        fprintf(stderr, "free: Pointer is misaligned (%s:%d)\n", file, line);
        exit(2);
    }

    // Verify that ptr corresponds to the start of a valid chunk by traversing the heap
    chunk_t *curr = (chunk_t *)heap.bytes;
    int valid = 0;
    while ((char *)curr < heap.bytes + MEMLENGTH)
    {
        if (curr == chunk)
        {
            valid = 1;
            break;
        }
        curr = (chunk_t *)((char *)curr + curr->size);
    }

    if (!valid)
    {
        fprintf(stderr, "free: Inappropriate pointer (%s:%d)\n", file, line);
        exit(2);
    }

    // Detect double free
    if (chunk->free)
    {
        fprintf(stderr, "free: Double free detected (%s:%d)\n", file, line);
        exit(2);
    }

    // Mark the chunk as free and coalesce adjacent free blocks
    chunk->free = 1;
    coalesce();
}

// Leak detection function (called at program exit)
void leak_check()
{
    chunk_t *curr = (chunk_t *)heap.bytes;
    int leaked_objects = 0;
    size_t leaked_size = 0;

    while ((char *)curr < heap.bytes + MEMLENGTH)
    {
        if (!curr->free)
        {
            // Report the leaked payload size (total size minus header)
            fprintf(stderr, "Leaked block at %p of size %zu bytes\n",
                    (void *)curr, curr->size - sizeof(chunk_t));
            leaked_objects++;
            leaked_size += (curr->size - sizeof(chunk_t));
        }
        curr = (chunk_t *)((char *)curr + curr->size);
    }

    if (leaked_objects > 0)
    {
        fprintf(stderr, "mymalloc: %zu bytes leaked in %d objects.\n",
                leaked_size, leaked_objects);
    }
}

// Register leak detector to run at exit
__attribute__((constructor)) void setup()
{
    atexit(leak_check);
}
