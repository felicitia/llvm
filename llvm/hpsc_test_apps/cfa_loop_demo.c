#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <userInput>\n", argv[0]);
        return 1;
    }

    int userInput = atoi(argv[1]); // Convert the first command line argument to integer

    clock_t start = clock();
    if (userInput % 2 == 0) {
        printf("The EVEN user input is %d\n", userInput);
        int loopCount = userInput;
        double total_loop_time = 0;

        for (int i = 0; i < loopCount; i++) {
            clock_t loop_start = clock();
            printf("EVEN loop iteration: %d\n", i);
            userInput = i + 1;
            sleep(1);
            clock_t loop_end = clock();
            double loop_time = (double)(loop_end - loop_start) / CLOCKS_PER_SEC;
            total_loop_time += loop_time;
            // printf("Time spent in iteration %d: %f seconds\n", i, loop_time);
        }
        printf("End EVEN Loop\n");
        printf("Average execution time per loop: %f seconds\n", total_loop_time / loopCount);

    } else {
        printf("The ODD user input is %d\n", userInput);
        int loopCount = userInput * 1000;
        double total_loop_time = 0;

        for (int i = 0; i < loopCount; i++) {
            clock_t loop_start = clock();
            printf("ODD loop iteration: %d\n", i);
            userInput = i + 1;
            sleep(1);
            clock_t loop_end = clock();
            double loop_time = (double)(loop_end - loop_start) / CLOCKS_PER_SEC;
            total_loop_time += loop_time;
            // printf("Time spent in iteration %d: %f seconds\n", i, loop_time);
        }
        printf("End ODD Loop\n");
        printf("Average execution time per loop: %f seconds\n", total_loop_time / loopCount);
    }

    userInput = -userInput;
    printf("Negated userInput is %d\n", userInput);
    clock_t end = clock();
    double total_time = (double)(end - start) / CLOCKS_PER_SEC;
    printf("Total program running time: %f seconds\nBye bye! :)\n", total_time);

    return 0;
}
