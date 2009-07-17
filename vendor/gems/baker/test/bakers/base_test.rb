require "#{File.dirname(__FILE__)}/../test_helper"

class BaseTest < Test::Unit::TestCase
  context "extract_options" do
    setup do
      @hsh = {:key => "matthews band"}
      @base = Baker::Base.new @hsh
    end

    should "extract the options that are sent to it and turn a string into a hash of the format: {:key => 'string'}" do      
      assert_equal @hsh, @base.extract_options(:key => "matthews band")
    end
    should "extract a string to the key" do
      assert_equal @hsh, @base.extract_options("matthews band")
    end
    should "set the key from the options given as a hash" do
      assert_equal "matthews band", @base.key
    end
    should "set the key when given as a string" do
      assert_equal "matthews band", Baker::Base.new("matthews band").key
    end
  end
  
end