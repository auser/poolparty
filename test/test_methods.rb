# Helpers
def fixtures_dir
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
  # Kernel.send :remove_const, :ENV if Kernel.const_defined?(:ENV)
  Kernel.const_set(:ENV, orig_env)
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
  Keypair.searchable_paths << fixtures_dir/"keys"  
end

def stub_ec2_calls(&block)
  stub_keypair_searchable_paths
  
  # FakeWeb.allow_net_connect=false
  stub_request(:get, /.*Action=DescribeInstances.*/).to_return(:status => 200, :body => open(fixtures_dir/"ec2/ec2-describe-instances_response_body.xml").read)

  # FakeWeb.register_uri(:get, /.*Action=DescribeInstances.*/, :status => ["200", "OK"],
  #                      :body => open(fixtures_dir/"ec2/ec2-describe-instances_response_body.xml").read)
  # 
  # FakeWeb.register_uri(:get, /.*Action=RunInstances.*/, :status => ["200", "OK"],
  #                      :body => open(fixtures_dir/"ec2/ec2-run-instances_response_body.xml").read)
  # 
  # FakeWeb.register_uri(:get, /.*Action=TerminateInstances.*/, :status => ["200", "OK"],
  #                      :body => open(fixtures_dir/"ec2/ec2-terminate-instances_response_body.xml").read)
  #                      
  # FakeWeb.register_uri(:post, /elasticloadbalancing\.amazonaws\.com/, :status => ["200", "OK"],
  #                      :body => open(fixtures_dir/"ec2/elb-describe-load-balancers.xml").read)
  #                      
  # FakeWeb.register_uri(:post, /\//, :status => ["200", "OK"],
  #                     :body => open(fixtures_dir/"ec2/ec2-describe-instances_response_body.xml").read)
  
  instance_eval &block if block
end