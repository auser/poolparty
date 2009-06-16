require "#{File.dirname(__FILE__)}/../../test_helper"

class PoolTest < Test::Unit::TestCase
  context "load_from_file" do
    setup do
      reset!
      PoolParty::Pool::Pool.load_from_file "#{::File.dirname(__FILE__)}/../../fixtures/fake_clouds.rb"
    end
    
    should "should load all the pools" do
      assert_not_nil pools[:boxed]
      assert_not_nil clouds[:app]
    end
    
    should "load the files" do
      assert_equal 2, clouds[:app].files.size
      assert_equal "/etc/junk", clouds[:app].files[0].name
      assert_equal "/etc/junkie", clouds[:app].files[1].name
    end
  end
  
end