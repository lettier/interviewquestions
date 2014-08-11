'''
  David Lettier (c) 2015.
  http://www.lettier.com
'''

'''
  Write a recursive function.
'''


def define_recursion(n, i=1, string='Define recursion.'):
  index = -(n * i + (i * i))
  if n <= 0:
    return string[:index] + ' See recursion.' + string[index:]
  return define_recursion(
      n - 1,
      i + 1,
      string[:index] + ' Define recursion.' + string[index:]
  )

if __name__ == '__main__':
  print(define_recursion(10))
