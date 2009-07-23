require "#{File.dirname(__FILE__)}/../../test_helper"
require File.dirname(__FILE__)+"/cloud_provider_instance_test.rb"

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
    assert 'poolparty', inst.user
    assert 'fred', inst.user('fred')
  end
  
  def test_host
    assert 'bigboy', inst.host
    assert inst.dns_name, inst.host
    assert 'bigsite', inst.host('bigsite')
  end
  
  def test_ssh
    assert_match /ssh bigboy .* -i (.*)test_key -l poolparty 'uptime'$/, inst.ssh('uptime')
  end
  
  def test_run
    assert_match /ssh bigboy .* -i (.*)test_key -l poolparty 'uptime'$/, inst.run('uptime')
    assert_match /ssh bigboy --sshoptions foo .*'uptime'$/, inst.ssh('uptime', {'--sshoptions'=>'foo'})
  end
  
  def test_rsync
    assert_match /rsync/, inst.rsync(:source=>fixtures_dir/'templates')
  end
  
  def test_ssh_options
    assert_match /-o StrictHostKeyChecking=no -i (.*)keys\/test_key -l poolparty/, inst.ssh_options
  end
  
  def test_simplest_run_remote
  end
  
  def test_rsync_to
    
  end
  
end