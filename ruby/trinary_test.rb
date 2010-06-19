require 'test/unit'
require 'trinary'

class TrinaryTest < Test::Unit::TestCase
  def expectation_map
    {
      "0"       => 0,
      "1"       => 1,
      "220"     => 2,
      "2210"    => 3,
      "2211"    => 4,
      "2212"    => 5,
      "2222000" => 6,
      "2222001" => 7,
      "2222002" => 8,
      "2222010" => 9,
      "2222020" => 12
    }
  end

  def test_encode_natural
    (0..30).each{|n|puts "%s\t%s" % [n, Trinary.encode_natural(n)]}

    expectation_map.sort_by{|k,v| v}.each do |expected, n|
      assert_equal( expected, Trinary.encode_natural(n), "encoding #{n} failed" )
    end
  end
end
