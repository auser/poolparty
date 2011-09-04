class Test::Unit::TestCase
  # Helpers
  def FIXTURES_PATH
    "#{::File.dirname(__FILE__)}/fixtures"
  end

  def test_dir
    "#{File.dirname(__FILE__)}/test_dir"
  end

  def clear!
    $pools = $clouds = nil
  end

  def modify_env_with_hash(h={})
    orig_env = Kernel.const_get(:ENV)
  
    h.each do |k,v|
      orig_env.delete(k)
      orig_env[k] = v
      orig_env[k].freeze
    end

    if RUBY_VERSION.scan(/1.8/).pop
      # Kernel.send :remove_const, 'ENV' if Kernel.const_defined?('ENV')
      Kernel.stubs(:ENV).returns(orig_env)
    elsif RUBY_VERSION.scan(/1.9/).pop
      Object.stubs(:ENV).returns(orig_env)
      # Object.send :remove_const, 'ENV' if Object.const_defined?('ENV')
    else
      raise "can't determine what version of ruby you are running."
    end
    
    # Kernel.const_set(:ENV, orig_env)
  end

  def capture_stdout(&block)
     old_stdout = $stdout
     old_stderr = $stderr
     out = StringIO.new
     $stdout = out
     old_stderr = StringIO.new
     begin
        block.call if block
     ensure
        $stdout = old_stdout
        $stderr = old_stderr
     end
     out.string
  end

  def stub_keypair_searchable_paths
    Keypair.searchable_paths << File.join(FIXTURES_PATH, "keys")
  end
  
  def read_fixture(path)
    open(File.join(FIXTURES_PATH, "ec2", path)).read
  end

  def stub_ec2_calls(&block)
    stub_keypair_searchable_paths
    
    modify_env_with_hash(
      "EC2_ACCESS_KEY" => "fake_access_key",
      "EC2_SECRET_KEY" => "fake_secret_key",
      "EC2_PRIVATE_KEY" => ::File.dirname(__FILE__) + "/fixtures/keys/test_key",
      "EC2_CERT"        => ::File.dirname(__FILE__) + "/fixtures/keys/test_key",
      "EC2_USER_ID"     => '1234567890'
      )
  
    # FakeWeb.allow_net_connect=false
    stub_request(:get, /.*Action=DescribeInstances.*/).
      to_return(:status => 200, :body => read_fixture('ec2-describe-instances_response_body.xml'))

    stub_request(:get, /.*Action=RunInstances.*/).
      to_return(:status => 200, :body => read_fixture('ec2-run-instances_response_body.xml'))


    stub_request(:get, /.*Action=TerminateInstances.*/).
      to_return(:status => 200, :body => read_fixture('ec2-terminate-instances_response_body.xml'))
      
    stub_request(:post, /elasticloadbalancing\.amazonaws\.com/).
      to_return(:status => 200, :body => read_fixture('elb-describe-load-balancers.xml'))
    
    stub_request(:post, /\//).
      to_return(:status => 200, :body => read_fixture('ec2-describe-instances_response_body.xml'))

    # FakeWeb.register_uri(:get, /.*Action=TerminateInstances.*/, :status => ["200", "OK"],
    #                      :body => open(FIXTURES_PATH/"ec2/ec2-terminate-instances_response_body.xml").read)
    #                      
    # FakeWeb.register_uri(:post, /elasticloadbalancing\.amazonaws\.com/, :status => ["200", "OK"],
    #                      :body => open(FIXTURES_PATH/"ec2/elb-describe-load-balancers.xml").read)
    #                      
    # FakeWeb.register_uri(:post, /\//, :status => ["200", "OK"],
    #                     :body => open(FIXTURES_PATH/"ec2/ec2-describe-instances_response_body.xml").read)
  
    instance_eval &block if block
  end
end