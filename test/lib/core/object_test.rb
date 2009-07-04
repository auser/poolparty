require "#{File.dirname(__FILE__)}/../../test_helper"

class ObjectTest < Test::Unit::TestCase
  context "object" do
    setup do
      clear!
    end
    context "global methods" do
      setup do
        @o = Object.new
      end
      
      should "have the clouds method" do      
        assert @o.respond_to?(:clouds)
        assert_equal Hash, @o.clouds.class
      end
      
      should "have the pools method" do
        assert @o.respond_to?(:pools)
        assert_equal Hash, @o.pools.class
      end
      
      should "make a pool when calling pool" do
        assert_nil @o.pools[:fun_pool]
        @pool = @o.pool :fun_pool do
        end
        assert_not_nil @o.pools["fun_pool"]
        assert_equal @pool, @o.pools["fun_pool"]
      end
      
      should "throw an error if the method pool for creation is given without a block" do
        PoolParty::PoolPartyError.create("PoolError")
        assert_raise PoolError do
          @o.pool :banks
        end
      end
      
      context "do_once" do
        should "have run_procs" do
          assert_equal Array, @o.run_procs.class
          assert_equal [], @o.run_procs
        end
        
        should "insert the block into the runprocs" do
          @proc = Proc.new {}
          @o.do_once &@proc
          assert_equal [@proc.to_s], @o.run_procs
        end
      end
      
      
    end
    
    
  end
  
end