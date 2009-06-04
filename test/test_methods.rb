def modify_env_with_hash(h={})
  orig_env = Kernel.const_get(:ENV)
  
  h.each do |k,v|
    orig_env.delete(k)
    orig_env[k] = v
  end
  
  Kernel.send :remove_const, :ENV if Kernel.const_defined?(:ENV)
  Kernel.const_set(:ENV, orig_env)
end