module PoolParty
  def working_conditional
    @working_conditional ||= []
  end
  
  def case_of o, &block
    c = Conditional.new({:name => "case_of_#{o}", :attribute => o}, &block)
    add_service c
    c
  end
  
  class Conditional < PoolParty::Service
    default_options({})
    
    def initialize(opts={}, &block)
      super(opts, &block)
      dsl_options(opts)
      options.freeze
    end
    
    def when_is o, &block
      add(o, &block)
    end  
    def otherwise &block
      add(nil, &block)
    end
    
    def add(o, &block)
      service = PoolParty::Service.new(&block)
      obj = (o ? o : :default).to_sym
      when_statements.merge!({obj => service})
    end
    
    def when_statements
      @when_statement ||= {}
    end
    def to_properties_hash
      {
        :options => {:variable => (dsl_options.attribute)},
        :resources => {},
        :services => {:control_statements => when_statements}
      }
    end
  end
  
end