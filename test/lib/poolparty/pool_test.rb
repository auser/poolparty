require "#{File.dirname(__FILE__)}/../../test_helper"

stub_keypair_searchable_paths

class PoolTest < Test::Unit::TestCase  
  def test_set_up_pool_object
    reset!
    pool "hi" do
    end
    assert_equal @@pool.name, "hi"
  end
end