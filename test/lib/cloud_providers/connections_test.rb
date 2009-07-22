require "#{File.dirname(__FILE__)}/../../test_helper"
require File.dirname(__FILE__)+"/cloud_provider_instance_test.rb"

class CloudProviders::CloudProviderInstance
  def cloud_provider(opts={}, &block)
    {:keypair=>Keypair.new(fixtures_dir/"keys/test_key")}
  end
end

class ConnectionsTest < Test::Unit::TestCase
  
  def inst
    @inst ||= CloudProviders::CloudProviderInstance.new( 
                :keypair_name => 'test_key',
                :status=>'pending'
              )
  end
  
  def test_ssh_options
    assert_match /-o StrictHostKeyChecking=no -i (.*)keys\/test_key -l poolparty/, inst.ssh_options
  end
  
  def test_simplest_run_remote
  end
  
  def test_rsync_to
    
  end
  
end