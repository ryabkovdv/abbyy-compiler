#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef MINIJAVA_INT_TYPE
    #define MINIJAVA_INT_TYPE long long
#endif

void prints(const char* s, size_t len, int end);

void __minijava_print_bool(int b, int end)
{
    if (b)
        prints("true", 4, end);
    else
        prints("false", 5, end);
}

void __minijava_print_int(unsigned MINIJAVA_INT_TYPE num, int end)
{
    char sign = 0;
    if ((MINIJAVA_INT_TYPE)num < 0) {
        num = -num;
        sign = '-';
    }

    char buf[24];
    char* it = buf + sizeof(buf) - 1;
    while (num >= 10) {
        MINIJAVA_INT_TYPE q = num / 10;
        MINIJAVA_INT_TYPE r = num % 10;
        *it-- = r + '0';
        num = q;
    }
    *it = num + '0';

    if (sign != 0)
        *--it = sign;

    prints(it, buf + sizeof(buf) - it, end);
}

void __minijava_print_object(void* obj, int end)
{
    if (obj == NULL) {
        prints("<nil>", 5, end);
        return;
    }

    void* vtable = *(void**)obj;
    char* name = *((void**)vtable - 1);

    /* <MyClass at 0x7fff1234> */
    prints("<", 1, 0);
    prints(name, strlen(name), 0);
    prints(" at ", 4, 0);
    prints("0x1234", 0, 0);
    prints(">", 1, end);
}

void __minijava_print_array(MINIJAVA_INT_TYPE* array, int end)
{
    if (array == NULL) {
        prints("[nil]", 5, end);
        return;
    }

    prints("[", 1, 0);

    MINIJAVA_INT_TYPE count = array[-1];
    for (MINIJAVA_INT_TYPE i = 0; i < count - 1; i++)
        __minijava_print_int(array[i], ' ');

    if (count != 0)
        __minijava_print_int(array[count - 1], 0);

    prints("]", 1, end);
}
