module Math
  def self.log_n(n, number)
    Math.log(number) / Math.log(n)
  end
end

module Trinary
  def self.encode_natural(n)
    return "0" if n == 0
    return "1" if n == 1
    return "220" if n == 2

    length = Math.log_n(3, n + 3).floor + 1
    base = (3**(length))
    mod = (n + 3 - (3**(length-1)))
    i = base + mod

    i.to_s(3).sub(/^1/, '22' * (length-1))
  end
end
