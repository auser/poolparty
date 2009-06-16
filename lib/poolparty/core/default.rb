=begin rdoc
  Default class
  
  Hangs on and defines defaults for PoolParty
=end

module PoolParty
  class Default
    include Dslify
    
    default_options(
      :user => "root"
    )
    
    def method_missing(m,*a,&block)
      p [dsl_options]
      if self.class.dsl_options.has_key?(m)
        self.class.dsl_options[m]
      else
        super
      end
    end
    
  end
end