require "#{::File.dirname(__FILE__)}/service.rb"
require "#{::File.dirname(__FILE__)}/../provision/boot_strapper.rb"

module PoolParty
  module Plugin
    
    def self.available
      @available_plugins ||= []
    end
        
    class Plugin < PoolParty::Service
      include CloudResourcer
      include PoolParty::DependencyResolverCloudExtensions
      include PoolParty::Callbacks
      
      dsl_methods :name
      
      def initialize(opts={}, extra_opts={}, prnt=nil, &block)
        setup_callbacks
        
        run_in_context {  before_load(opts, &block) }
        
        block = Proc.new {enable} unless block
        
        super(opts, &block)
        
        run_in_context { loaded opts, &block }
        
        after_create
      end
      
      # Overwrite this method
      def before_load(o={}, &block)
      end
      def loaded(o={}, &block)
      end
      # Callbacks available to plugins
      def after_create
      end
      
      def calls(r)
        has_exec "ls", :calls => r
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
        
        klass = subclass.to_s.split('::').pop.to_s
        add_has_and_does_not_have_methods_for(klass.snake_case)
        add_resource_lookup_method(klass.snake_case)
        
        # Add the plugin definition to the cloud as an instance method
        # For example apache do; apache option and method; end
        meth = <<-EOM
          def #{klass.snake_case}(opts={}, &block)
            i = plugin_store.select {|i| i if i.class.to_s =~ /#{klass}/ }.first if plugin_store
            if i
              i
            else
              inst = ::PoolParty::Plugin::#{klass}.new(opts, parent, &block)               
              plugin_store << inst if plugin_store
              inst
            end
          end
        EOM
        
        ::PoolParty::PoolPartyBaseClass.module_eval meth
        
        # Store the plugins so they will be availble in an array at Plugin.available
        ::PoolParty::Plugin.available << subclass
        super
        
      end
      
    end
    
  end
end