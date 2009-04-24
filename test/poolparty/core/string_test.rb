require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestString < Test::Unit::TestCase
  context "to_hash" do
    setup do
      @string =<<-EOS
        a = "a"
        b = "b"
        c = "c"
      EOS
    end

    should "turn it into a hash with the values from =" do
      @string.to_hash[:a].should == "a"
      @string.to_hash[:b].should == "b"
      @string.to_hash[:c].should == "c"
    end
  end
  context "macify" do
    setup do
      @mac = "00:0c:29:44:f1:0f"
    end

    should "should turn the mac into the response on the command-line" do
      @mac.macify.should == "0:c:29:44:f1:f"
    end
  end
  
end