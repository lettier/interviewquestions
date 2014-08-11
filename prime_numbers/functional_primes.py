'''
  David Lettier
  (C) 2015
  http://www.lettier.com/
'''

# Given n, write a function that uses map
# and filter to print all primes from 2 up
# to n.

import numpy


def primes(n):
  return ' '.join(
      list(
          map(
              lambda e: str(e),
              map(
                  lambda d: d[1],
                  filter(
                      lambda c: c[0],
                      map(
                          lambda b: (
                              len(b[1]) == 2 or len(b[1]) == 1,
                              b[0]
                          ),
                          enumerate(
                              map(
                                  lambda x: list(
                                      filter(
                                          lambda z: z == 0,
                                          list(
                                              map(
                                                  lambda y: x % y,
                                                  numpy.arange(2, x + 1)
                                              )
                                          )
                                      )
                                  ),
                                  numpy.arange(0, n + 1)
                              )
                          )
                      )
                  )
              )
          )
      )
  )

print(primes(1001))
print(primes(0))
print(primes(1))
print(primes(13))
print(primes(-13))
