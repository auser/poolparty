module PoolParty
  class Plugin < Base
    
    include Callbacks
    
    def self.available
      @available_plugins ||= []
    end
    
    def initialize(opts={}, extra_opts={}, prnt=nil, &block)
      run_in_context {  callback(:before_load, opts) }
      
      block = Proc.new {enable} unless block      
      super(opts, &block)
      
      run_in_context { callback(:loaded, opts) }
      
      after_create
    end
  
        
    # Overwrite this method
    def before_load(o={}, &block)
    end
    def after_loaded(o={}, &block)
    end
    
    def calls(r)
      has_exec "echo 'calling #{r.name}'", :calls => r
    end
    
    def enable
    end
    def is_plugin?
      true
    end
    def cloud
      context_stack.find do |i|
        i.class == PoolParty::Cloud::Cloud
      end
    end
    
    def bootstrap_gems *gems
      gems.each do |g|
        Provision::BootStrapper.gem_list << g unless Provision::BootStrapper.gem_list.include?(g)
      end
    end
    
    def bootstrap_commands cmds
      Provision::BootStrapper.class_commands << cmds
    end
    
    def configure_commands cmds
      Provision::DrConfigure.class_commands << cmds
    end
    
    def self.inherited(subclass)
      lowercase_sym = subclass.to_s.top_level_class.to_sym
      available.push(lowercase_sym) unless available.include?(lowercase_sym)
    end      
  end
end