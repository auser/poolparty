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
    
    # Method missing
    def self.method_missing(m,*a,&block)
      dsl_options.include?(m) ? dsl_options[m] : super
    end
    
  end
end