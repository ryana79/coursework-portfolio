#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "mymalloc.h"

#define ALLOCATIONS 120

typedef struct node
{
    int value;
    struct node *next;
} node_t;

typedef struct tree
{
    int value;
    struct tree *left, *right;
} tree_t;

void workload1()
{
    for (int i = 0; i < ALLOCATIONS; i++)
    {
        void *ptr = malloc(1);
        if (ptr)
        {
            free(ptr);
        }
        else
        {
            fprintf(stderr, "workload1: malloc(1) returned NULL at iteration %d\n", i);
        }
    }
}

void workload2()
{
    void *ptrs[ALLOCATIONS];
    for (int i = 0; i < ALLOCATIONS; i++)
    {
        ptrs[i] = malloc(1);
        if (!ptrs[i])
        {
            fprintf(stderr, "workload2: malloc(1) returned NULL at iteration %d\n", i);
        }
    }
    for (int i = 0; i < ALLOCATIONS; i++)
    {
        if (ptrs[i])
        {
            free(ptrs[i]);
        }
    }
}

void workload3()
{
    void *ptrs[ALLOCATIONS];
    int allocated = 0;
    for (int i = 0; i < ALLOCATIONS; i++)
    {
        if ((rand() % 2 == 0) && (allocated < ALLOCATIONS))
        {
            void *p = malloc(1);
            if (p)
            {
                ptrs[allocated++] = p;
            }
            else
            {
                fprintf(stderr, "workload3: malloc(1) returned NULL at iteration %d\n", i);
            }
        }
        else if (allocated > 0)
        {
            free(ptrs[--allocated]);
        }
    }
    while (allocated > 0)
    {
        free(ptrs[--allocated]);
    }
}

// **Workload 4: Simulating a Linked List**
void workload4()
{
    node_t *head = NULL, *current = NULL;
    for (int i = 0; i < 30; i++)
    {
        node_t *new_node = malloc(sizeof(node_t));
        if (!new_node)
        {
            fprintf(stderr, "workload4: malloc(sizeof(node_t)) returned NULL for node %d\n", i);
            continue;
        }
        new_node->value = i;
        new_node->next = NULL;
        if (!head)
        {
            head = new_node;
        }
        else
        {
            current->next = new_node;
        }
        current = new_node;
    }

    // Free the linked list
    while (head)
    {
        node_t *temp = head;
        head = head->next;
        free(temp);
    }
}

// **Workload 5: Simulating a Binary Tree**
tree_t *create_tree(int depth)
{
    if (depth == 0)
        return NULL;

    tree_t *node = malloc(sizeof(tree_t));
    if (!node)
    {
        fprintf(stderr, "workload5: malloc(sizeof(tree_t)) returned NULL at depth %d\n", depth);
        return NULL;
    }
    node->value = depth;
    node->left = create_tree(depth - 1);
    node->right = create_tree(depth - 1);
    return node;
}

void free_tree(tree_t *root)
{
    if (!root)
        return;
    free_tree(root->left);
    free_tree(root->right);
    free(root);
}

void workload5()
{
    tree_t *root = create_tree(4);
    free_tree(root);
}

void run_memgrind()
{
    struct timeval start, end;

    gettimeofday(&start, NULL);
    for (int i = 0; i < 50; i++)
    {
        workload1();
        workload2();
        workload3();
        workload4();
        workload5();
    }
    gettimeofday(&end, NULL);

    double elapsed_time = (end.tv_sec - start.tv_sec) * 1000.0 +
                          (end.tv_usec - start.tv_usec) / 1000.0;
    printf("Average execution time: %.2f ms\n", elapsed_time / 50);
}

int main()
{
    run_memgrind();
    return 0;
}
