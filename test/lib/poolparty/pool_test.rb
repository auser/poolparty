require 'test_helper'

class PoolTest < Test::Unit::TestCase  
  def setup
    stub_keypair_searchable_paths
  end
  
  def test_set_up_pool_object
    reset!
    pool "hi" do
    end
    assert_equal @@pool.name, "hi"
  end
end