# (C) David Lettier 2016.
# http://www.lettier.com/

# Given a key and a string
# encrypt the text using Caesar's Cipher.
# You can ignore any non-alpha character.

ALPHA = 'abcdefghijklmnopqrstuvwxyz'.split('')

def shift(key, char)
  return char unless ALPHA.include?(char.downcase)
  ALPHA[(ALPHA.index(char.downcase) + key) % ALPHA.size]
end

def encrypt(key, text)
  return '' if text.nil?
  text.split('').map do |x|
    char = shift(key, x)
    next char.upcase if x == x.upcase
    char
  end.join('')
end

[
  [0, 'This is a test.', 'This is a test.'],
  [1, nil, ''],
  [-1, '', ''],
  [-2, 'AbCdE fG', 'YzAbC dE']
].each do |problem|
  key = problem.first
  text = problem[1]
  expected = problem.last
  result = encrypt(key, text)
  puts result
  raise "#{problem} is wrong." unless expected == result
end
