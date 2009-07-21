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
  end
  
  Kernel.send :remove_const, :ENV if Kernel.const_defined?(:ENV)
  Kernel.const_set(:ENV, orig_env)
end

def capture_stdout(&block)
   old_stdout = $stdout
   out = StringIO.new
   $stdout = out
   begin
      block.call if block
   ensure
      $stdout = old_stdout
   end
   out.string
end

def include_fixture_resources
  Dir["#{::File.dirname(__FILE__)}/fixtures/resources/*.rb"].each do |res|
    require res
  end
end

def include_chef_only_resources
  DependencyResolvers::Chef.send :require_chef_only_resources
end