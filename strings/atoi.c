/*
  David Lettier (C) 2016.
  http://www.lettier.com/

  Convert an integer in string
  form to its numerical equivalent.
*/

#include <stdio.h>
#include <string.h>

long long int lettier_pow(int i, int c) {
  if (c <= 0) {
    return 1;
  } else {
    return i * lettier_pow(i, c - 1);
  }
}

long long int lettier_atoi(char* s) {
  int length = strlen(s);

  /*
    Given: "-12345"
    Do: -1 * 1 * 10^4 + 2 * 10^3 + 3 * 10^2 + 4 * 10^1 + 5 * 10^0
  */

  if (length == 0) {
    return 0; // Base case.
  } else {
    int sign  = 1;
    int start = 0;

    long long int result = 0;

    if (s[0] == '-') {
      length -=  1;
      start   =  1;

      sign    = -1;
    }

    result = (s[start] - 48) * lettier_pow(10, length - 1);

    memmove(s, s + start + 1, length);

    /*
      Given: "-12345"
      Do:
        -1 * (1 * 10^4 +
          1 * (2 * 10^3 +
            1 * (3 * 10^2 +
              1 * (4 * 10^1 +
                1 * (5 * 10^0 +
                  0
                )
              )
            )
          )
        )
    */

    return sign * (result + lettier_atoi(s));
  }
}

int main(int argc, char* argv[]) {
  if (argc == 2) {
    char command[64];
    strncpy(command, argv[1], 64);

    if (strcmp(command, "test") == 0) {
      for (long long int i = 50000; i >= -50000; --i) {
        char test[7];
        snprintf(test, 7, "%d", i);

        printf("Testing: %d", i);

        long long int result = lettier_atoi(test);
        if (result == i) {
          printf("\n=> pass\n");
        } else {
          printf("\n=> fail %lld != %lld\n", i, result);
          break;
        }
      }
    } else {
      printf("In:  %s\n", argv[1]);
      printf("Out: %lld\n", lettier_atoi(argv[1]));
    }
  } else {
    printf("Must supply an integer.\n");
  }

  return 0;
}
