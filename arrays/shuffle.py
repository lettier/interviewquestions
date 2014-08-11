'''
  David Lettier (c) 2015.
  http://www.lettier.com
'''

import math
import random
import operator

'''
  Write a function that inputs a string.
  This function shuffles the string such that
  no two identical characters are adjacent.
  If the string cannot satisfy the constraint,
  the function returns false.
  If it can satisfy the constraint, it returns
  the shuffled string.
'''


def shuffle(string):
  if len(string) < 2:
    return string
  counts = {}
  limit = len(string) - (math.floor(len(string) / 2.0))
  for c in string:
    if c not in counts:
      counts[c] = 0
    counts[c] += 1
    if counts[c] > limit:
      return False
  sorted_counts = sorted(
      counts.items(),
      key=operator.itemgetter(1),
      reverse=True
  )
  sorted_counts = [list(x) for x in sorted_counts]
  result = ''
  num_sorted_counts = len(sorted_counts)
  zeros = 0
  cont = True
  while cont:
    for sorted_count in sorted_counts:
      if sorted_count[1] == 0:
        continue
      result += sorted_count[0]
      sorted_count[1] -= 1
      if sorted_count[1] == 0:
        zeros += 1
        if zeros == num_sorted_counts:
          cont = False
          break
        break
  return result


def checker(string, solution):
  if len(string) == 0:
    if len(solution) == 0:
      return True
    return False
  if solution is False:
    limit = len(string) - (math.floor(len(string) / 2.0))
    hashh = {}
    for y in string:
      if y not in hashh:
        hashh[y] = 0
      hashh[y] += 1
    vals = [v for v in hashh.values()]
    if max(vals) > limit:
      return True
    else:
      print('You said false but a solution did exist.')
      return False
  else:
    if len(solution) != len(string):
      print('Size does not match: %s %s' % (len(string), len(solution)))
      return False
    for i, y in enumerate(solution):
      if (i + 1) < len(solution):
        if solution[i + 1] == y:
          print('Two adjacent characters found: %s %s' % (y, solution[i + 1]))
          return False
    return True


if __name__ == '__main__':
  tests = [
      '',
      'ab',
      'aab',
      'aaa',
      'aaab',
      'aabb',
      'aaaabbb',
      'aaaaabb',
      '111222333444555666777888999000a',
      '1234567890'
  ]
  for test in tests:
    print('Test: %s, Solution: %s\n' % (test, shuffle(test)))
    assert checker(test, shuffle(test)) == True,\
        '%s %s' % (test, shuffle(test))
  for i in range(50):
    test = ''
    rand_int = random.randint(0, 50)
    for i in range(rand_int):
      test += chr(random.randint(0, 25) + ord('a'))
    print('Test: %s, Solution: %s\n' % (test, shuffle(test)))
    assert checker(test, shuffle(test)) == True,\
        '%s %s' % (test, shuffle(test))
