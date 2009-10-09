=begin rdoc
  Base class for all PoolParty objects
=end
module PoolParty
  
  class Base
    include Dslify
    attr_reader :name
    def initialize(name, o={}, &block)
      @name = name
      @init_opts = o
      set_vars_from_options(o)
      instance_eval &block if block
      after_initialized
    end
    def after_initialized
    end
    def run
      warn "#{self.class} does not implement run. Something is wrong"
    end
    def method_missing(m,*a,&block)
      if parent.respond_to?(m)
        parent.send(m,*a,&block)
      else
        super
      end
    end
    
    private
  end

end