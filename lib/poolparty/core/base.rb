=begin rdoc
  Base class for all PoolParty objects
=end
module PoolParty
  class Base
    include Callbacks
    include Dslify
    
    # So methods inside the default_options do not have to 
    # have default options and can pull from their parents
    # but the option is still pulled for the printed default_options
    def self.additional_options(*o)
      dsl_options.merge!(o.inject({}) {|s,i| s.merge(i => nil)})
    end
    
    def method_missing(m,*a,&block)
      if Default.respond_to?(m)
        Default.send(m,*a,&block)
      else
        super
      end
    end
    
  end
end