# David Lettier (c) 2015.
# http://www.lettier.com

# Implement square root recursively.

def sqrt(value, **params) # rubocop:disable all
  return 0 if value <= 0
  params[:left] = 0 if params[:left].nil?
  params[:right] = value if params[:right].nil?
  params[:i] = 1 if params[:i].nil?
  return nil if params[:left] >= params[:right]
  search = (params[:right] + params[:left]) / 2.0
  test = search**2
  if test > value
    params[:right] = search
  elsif test < value
    params[:left] = search
  elsif test == value
    return search
  end
  return search if params[:i] == 7663  # Do not blow the stack.
  params[:i] += 1
  sqrt(value, params)
end

1000.times do |i|
  fail "Wrong value for #{i}: #{Math.sqrt(i)} #{sqrt(i)}" \
    unless Math.sqrt(i).round(11) == sqrt(i).round(11)
  puts "#{i}: #{Math.sqrt(i).round(11)} #{sqrt(i).round(11)}"
end
