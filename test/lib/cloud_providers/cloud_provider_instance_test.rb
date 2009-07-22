require "#{File.dirname(__FILE__)}/../../test_helper"

class CloudProviderInstanceTest < Test::Unit::TestCase
  
  def setup
    @inst = CloudProviders::CloudProviderInstance.new( 
              :keypair_name => 'test_key',
              :status=>'pending'
            )
  end
  
  def test_default_options
    assert_nil @inst.public_ip
    assert_nil @inst.internal_ip
    assert_nil @inst.launch_time
    assert_nil @inst.name
    assert_equal 'test_key', @inst.keypair_name
  end
  
  def test_keypair
    @inst.keypair(fixtures_dir/'keys/test_key')
    assert_equal 'test_key', @inst.keypair_name
    assert_kind_of PoolParty::Keypair, @inst.keypair
  end
  
  def test_pending_status
    assert 'pending', @inst.status
    assert @inst.pending?
    assert !@inst.running?
    assert !@inst.terminated?
    assert !@inst.terminating?
  end
  
  def test_valid
    assert !@inst.valid?  #no ip or name
    @inst.internal_ip = '10.0.0.1'
    @inst.name = 'fruby'
    assert @inst.valid?
  end
  
  def test_cloud_provider
    assert_raise StandardError do
      @inst.cloud_provider
    end
  end
  
end
