require "#{::File.dirname(__FILE__)}/service.rb"
require "#{::File.dirname(__FILE__)}/../provision/boot_strapper.rb"

module PoolParty
  module Plugin
        
    class Plugin < PoolParty::Service
      include CloudResourcer
      include PoolParty::DependencyResolverCloudExtensions
      include PoolParty::Callbacks
      
      default_options(
        :name => nil
      )
      
      def initialize(opts={}, extra_opts={}, prnt=nil, &block)        
        @base_name = get_name_from_options_and_extra_options(opts, extra_opts)
        @opts = (opts.is_a?(Hash) ? extra_opts.merge(opts) : extra_opts).merge(:name => @base_name)
        
        setup_callbacks
        
        puts "#{@opts.inspect} before run_in_context before_load" if self.class.to_s.top_level_class =~ /git/
        
        run_in_context do
          before_load(@opts, &block)
        end
        
        block = Proc.new {enable} unless block        
        
        super(@opts, &block)
        
        puts "#{@opts.inspect} after run_in_context before loaded" if self.class.to_s.top_level_class =~ /git/
        
        run_in_context do
          loaded @opts, &block
        end
                
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
      
      def enable
      end
      def is_plugin?
        true
      end
      def cloud
        @parent
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
        method_name = subclass.to_s.top_level_class.gsub(/pool_party_/, '').gsub(/_class/, '').downcase.to_sym
        add_has_and_does_not_have_methods_for(method_name)
      end
      
    end
    
  end
end