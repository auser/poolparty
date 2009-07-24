require "#{File.dirname(__FILE__)}/../../test_helper"
require File.dirname(__FILE__)+"/cloud_provider_instance_test.rb"

stub_keypair_searchable_paths

module  CloudProviders
  module Connections
    def system_run(cmds, opts={})
      return cmds
    end
  end
  
  class CloudProviderInstance
    def cloud_provider(opts={}, &block)
      {:keypair=>Keypair.new(fixtures_dir/"keys/test_key")}
    end
  end
  
end

class ConnectionsTest < Test::Unit::TestCase
  
  def inst
    @inst ||= CloudProviders::CloudProviderInstance.new( 
                :keypair_name => 'test_key',
                :status       => 'pending',
                :dns_name     => 'bigboy'
              )
  end
  
  def test_default_user
    assert_equal 'poolparty', inst.user
    assert_equal 'fred', inst.user('fred')
    assert_equal 'fred', inst.user
  end
  
  def test_host
    assert_equal 'bigboy', inst.host
    assert_equal inst.dns_name, inst.host
    assert_equal 'bigsite', inst.host('bigsite')
    assert_equal 'bigsite', inst.host 
  end
  
  def test_ssh
    assert_match /ssh poolparty@bigboy .* -i (.*)test_key 'uptime'$/, inst.ssh('uptime')
  end
  
  def test_run
    assert_match /ssh poolparty@bigboy .* -i (.*)test_key 'uptime'$/, inst.run('uptime')
    assert_match /ssh poolparty@bigboy (.*)--sshoptions foo (.*)'uptime'$/, inst.ssh('uptime', {'--sshoptions'=>'foo'})
  end
  
  def test_rsync
    assert_match /rsync/, inst.rsync(:source=>fixtures_dir/'templates')
  end
  
  def test_ssh_options
    assert_match /-o StrictHostKeyChecking=no -i (.*)keys\/test_key/, inst.ssh_options
  end
  
  def test_simplest_run_remote
  end
  
  def test_rsync_to    
  end
  
  def test_scp
    assert_match /scp (.*)\/templates\/script poolparty@bigboy/, inst.scp(:source=>fixtures_dir/'templates/script')
  end
  
end