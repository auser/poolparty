=begin rdoc
  Base class for all PoolParty objects
  
  Inherits from Default so that all the default methods can be retrieved
=end
module PoolParty
  class Base < Default
    
    # So methods inside the default_options do not have to 
    # have default options and can pull from their parents
    # but the option is still pulled for the printed default_options
    def self.additional_options(*o)
      dsl_options.merge!(o.inject({}) {|s,i| s.merge(i => nil)})
    end
    
  end
end