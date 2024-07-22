#include <stdio.h>
#include <stdlib.h>

int factorial(int n) {
    if (n == 0)
        return 1;
    else
        return n * factorial(n - 1);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <number>\n", argv[0]);
        return 1;
    }

    int n = atoi(argv[1]);
    if (n < 0) {
        printf("Please enter a non-negative integer.\n");
        return 1;
    }

    printf("Factorial of %d is %d\n", n, factorial(n));
    return 0;
}
