#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h> 

int main(int argc, char *argv[]) {

  if (argc < 2) {
        printf("Usage: %s <randomNumber>\n", argv[0]);
        return 1;
  }

  int randomNumber = atoi(argv[1]); // Convert the first command line argument to integer

  if (randomNumber % 2 == 0) {
    printf("The EVEN random number is %d\n", randomNumber);
    printf("Start EVEN Loop\n");
    time_t startTime = time(NULL);
    time_t endTime = startTime + 600; // 600 seconds = 10 minutes

    while (time(NULL) < endTime)
    {
      printf("EVEN number timestamp: %ld\n", time(NULL)); 
      randomNumber = randomNumber + 2;
      sleep(1);
    }
    printf("End EVEN Loop\n");

  } else {

    printf("The ODD random number is %d\n", randomNumber);
    printf("Start ODD Loop\n");
    time_t startTime = time(NULL);
    time_t endTime = startTime + 3; 

    while (time(NULL) < endTime)
    {
      printf("ODD number timestamp: %ld\n", time(NULL));
      randomNumber = randomNumber + 2;
      sleep(1);
    }
    printf("End ODD Loop\n");
  }

  randomNumber = -randomNumber;
  printf("Negated randomNumber is %d\n", randomNumber);
  printf("End of program...Bye bye! :)\n");
  
  return 0;
}