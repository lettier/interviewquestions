'''
  David Lettier (C) 2015.
  http://www.lettier.com/
'''

import random

'''
  Reverse a string in place.
'''


def swap(l, i):
  t = l[i]
  length = len(l)
  l[i] = l[length - 1 - i]
  l[length - 1 - i] = t


def reverse(string):
  string = list(string)
  half = int(len(string) / 2)
  for i in range(half):
    swap(string, i)
  return ''.join(string)


if __name__ == '__main__':
  alpha = list('abcdefghijklmnopqrstuvwxyz')
  for i in range(100):
    string = ''
    x = random.randint(1, 50)
    for i in range(x):
      string += random.choice(alpha)
    test = string
    solution = reverse(string)
    print('Test: %s, Solution: %s' % (test, solution))
    assert reverse(string) == test[::-1]
