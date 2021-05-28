#include <stdlib.h>

#ifndef MINIJAVA_INT_SIZE
    #define MINIJAVA_INT_SIZE sizeof(void*)
#endif

void* __minijava_new(size_t size)
{
    void* ptr = calloc(1, size);
    if (ptr == NULL)
        abort();
    return ptr;
}

void* __minijava_newarray(size_t count)
{
    void* ptr = calloc(count + 1, MINIJAVA_INT_SIZE);
    if (ptr == NULL || count + 1 == 0)
        abort();
    return (char*)ptr + MINIJAVA_INT_SIZE;
}
