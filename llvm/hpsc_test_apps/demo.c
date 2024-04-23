#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {

  srand(time(0)); // Seed the random number generator

  int num = rand() % 11; // Generate a random number between 0 and 10

  if (num % 2 == 0) {
    num = 200;
  } else {
    num = 100;
  }
  num = -num;
  printf("num is %d\n", num);
  return 0;
}