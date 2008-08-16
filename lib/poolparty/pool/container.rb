=begin rdoc
  The Container
  
  Container holds the various features for the final compilations
  for each pool.
    
  There is a Container for every pool
=end
module PoolParty
  
  class Container
    attr_accessor_with_default :packages, :services, :files, :lines do
      Hash.new
    end
    attr_accessor_with_default :classes, :templates, :custom_functions, :custom_calls do
      Array.new
    end
    
  end  
end  