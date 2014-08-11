# David Lettier (c) 2016.
# http://www.lettier.com

# Demonstrate Ruby's method lookup path.

module A
  def run
    puts 'six'
    super
  end
end

module B
  def run
    puts 'five'
    super
  end
end

class C
  def run
    puts 'seven'
  end
end

module D
  class D < C
    # Reverse order.
    include A
    include B

    def run
      puts 'four'
      super
    end
  end

  def run
    puts 'two'
    super
  end
end

module E
  def run
    puts 'three'
    super
  end
end

d = D::D.new
d.define_singleton_method(:run) do
  puts 'one'
  super()
end
# Reverse order.
d.extend(E)
d.extend(D)

d.run

# one
# two
# three
# four
# five
# six
# seven
