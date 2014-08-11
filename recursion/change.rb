# David Lettier (c) 2016.
# http://www.lettier.com

# Given a total and a set of values,
# how many combinations of repeated
# elements from the set of values sum
# to the total?

def combination_count(total, values = [])
  return nil unless values.is_a?(Array)
  values = values.sort_by(&:-@) # Desc.
  combination_count_recursion(total, values.uniq)
end

def combination_count_recursion(total, values)
  result = base_case(total, values)
  return result unless result.nil?
  # There are two sets of solutions
  # that make up the total number of ways
  # of adding up repeated elements
  # in values summing up to the total.
  # 1) All the solutions containing at least one use of the
  #    first element in values.
  # 2) All the solutions with out the first element in values.
  # Say our total is 8 and our values are [2,1].
  # ways of adding up to 8 =
  #   ways of adding up to 8 using only 1's +
  #   ways of adding up to 6 (after using one 2) plus some other
  #   combinations of 2's and 1's
  # 2 | 2 2 2
  # 2 | 2 2 1 1
  # 2 | 2 1 1 1 1
  # 2 | 1 1 1 1 1 1
  # +
  # 1 1 1 1 1 1 1 1
  # ---------------
  # 5 total ways
  #
  # Going up one step in recursion:
  # ways of adding up to 6 =
  #   ways of adding up to 6 using only 1's +
  #   ways of adding up to 4 (after using one 2) plus some other
  #   combinations of 2's and 1's
  # 2 | 2 2
  # 2 | 2 1 1
  # 2 | 1 1 1 1
  # +
  # 1 1 1 1 1 1
  # -----------
  # 4 total ways
  #
  # And on and on...
  combination_count_recursion(
    total,
    values[1..-1]
  ) + combination_count_recursion(
    total - values.first,
    values
  )
end

def base_case(total, values)
  return 0 if values.empty? # Empty set so no way of making a combination.
  return 0 if total < 0 # No positive combination can sum up to less than zero.
  return 1 if total == 0 # Only one way to sum up to 0: use no values.
  nil # Keep recurring.
end

[
  [100, 3, 7, 2],
  [8, 1, 2],
  [50, 4, 7, 10],
  [1001, 1001, 1],
  [102, 103]
].each do |problem|
  total = problem.first
  values = problem[1..-1]
  puts "There are #{combination_count(total, values)} " \
    "ways of adding up to #{total} using #{values}."
end
