'''
  David Lettier (C) 2015.
  http://www.lettier.com/
'''

import random

'''
  Convert a float in string
  form to its numerical equivalent.
'''


def atoi(string):
  if len(string) == 0:
    return string
  integer = 0
  for j, c in enumerate(string[::-1]):
    i = ord(c) - ord('0')
    integer += i * (10**j)
  return integer


def atof(string):
  if len(string) == 0:
    return string
  split = string.split('.')
  if len(split) == 1:
    return atoi(split[0])
  if len(split) == 2:
    if len(split[1]) != 0:
      decimal = 0.0
      for j, c in enumerate(split[1]):
        i = ord(c) - ord('0')
        decimal += i * (10**(-(j + 1)))
      return atoi(split[0]) + decimal
    elif len(split[1]) == 0:
      return atoi(split[0])
  else:
    return string


if __name__ == '__main__':
  nums = list('0123456789')
  for i in range(100):
    x = random.randint(1, 11)
    string = ''
    for j in range(x):
      string += random.choice(nums)
    string += '.'
    for j in range(x):
      string += random.choice(nums)
    test = string
    solution = atof(string)
    print('Test: %s, Solution: %s' % (test, solution))
    assert round(float(test), 11) == round(solution, 11)
