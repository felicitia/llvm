#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
  int condition = 1;

  srand(time(0)); // Seed the random number generator

  int randomNumber = rand() % 11; // Generate a random number between 0 and 10

  if (randomNumber % 2 == 0) {
    printf("Bye World, the random number is %d\n", randomNumber);
    printf("Start Loop\n");
    for (int i = 0; i < randomNumber; i++) {
      printf("Bye %d\n", i);
    }
    printf("End Loop\n");
  } else {
    printf("Hello World, the random number is %d\n", randomNumber);
    printf("Start Loop\n");
    for (int i = 0; i < randomNumber; i++) {
      printf("Hello %d\n", i);
    }
    printf("End Loop\n");
  }

  printf("Bye bye! :)\n");

  return 0;
}