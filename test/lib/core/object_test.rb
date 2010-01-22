require "#{File.dirname(__FILE__)}/../../test_helper"

class ObjectTest < Test::Unit::TestCase
  context "object" do
    setup do
      reset!
    end
    context "global methods" do
      setup do
        @o = Object.new
      end
            
      should "have the pools method" do
        assert @o.respond_to?(:pool)
      end
      
      should "make a pool when calling pool" do
        assert_nil @@pool
        @@pool = pool :fun_pool do
        end
        assert_not_nil @@pool
      end
      
    end
    
    
  end
  
end