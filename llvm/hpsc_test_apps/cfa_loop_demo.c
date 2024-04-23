#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {

  srand(time(0)); // Seed the random number generator

  int randomNumber = rand() % 11; // Generate a random number between 0 and 10

  if (randomNumber % 2 == 0) {
    printf("The EVEN random number is %d\n", randomNumber);
    printf("Start EVEN Loop\n");
    time_t startTime = time(NULL);
    time_t endTime = startTime + 600; // 600 seconds = 10 minutes

    while (time(NULL) < endTime)
    {
      printf("EVEN number timestamp: %ld\n", time(NULL)); // Print current timestamp
    }
    printf("End EVEN Loop\n");

  } else {

    printf("The ODD random number is %d\n", randomNumber);
    printf("Start ODD Loop\n");
    time_t startTime = time(NULL);
    time_t endTime = startTime + 600; // 600 seconds = 10 minutes

    while (time(NULL) < endTime)
    {
      printf("ODD number timestamp: %ld\n", time(NULL)); // Print current timestamp
    }
    printf("End ODD Loop\n");
  }

  printf("End of program...Bye bye! :)\n");

  return 0;
}