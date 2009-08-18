require "#{File.dirname(__FILE__)}/../../test_helper"

class CloudProviderInstanceTest < Test::Unit::TestCase
  
  def setup
    @inst = CloudProviders::CloudProviderInstance.new(
              :keypair_name => 'test_key',
              :status       => 'pending'
            )
  end
  
  def test_default_options
    assert_nil @inst.public_ip
    assert_nil @inst.internal_ip
    assert_nil @inst.launch_time
    assert_nil @inst.name
    assert_equal 'test_key', @inst.keypair_name
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

  def test_blank_public_ip_is_invalid
    @inst.public_ip = ''
    assert ! @inst.wait_for_public_ip(1)
  end

  def test_zero_public_ip_is_invalid
    @inst.public_ip = '0.0.0.0'
    assert ! @inst.wait_for_public_ip(1)
  end

  def test_nil_public_ip_is_invalid
    @inst.public_ip = nil
    assert ! @inst.wait_for_public_ip(1)
  end

  def test_wait_for_public_ip
    @inst.public_ip = '10.0.0.1'
    assert @inst.wait_for_public_ip(1)
  end
  
  # Not quite an amazing test, but better than nothing to share
  # that the port is open.
  def test_wait_for_port
    @inst.class.send(:define_method, :is_port_open?) {false}
    assert !@inst.wait_for_port(22, :timeout => 20, :retry_times => 1)
    @inst.class.send(:define_method, :is_port_open?) {true}
    assert @inst.wait_for_port(80)
  end
  
  def test_terminate!
    assert_respond_to @inst, :terminate!
  end
  
end
