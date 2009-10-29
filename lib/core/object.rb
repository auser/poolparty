class Object
    
  def pool(name=nil, &block)
    @@pool ||= PoolParty::Pool.new(name, &block)
  end
  
  def reset!
    @@pool = nil
  end
    
end