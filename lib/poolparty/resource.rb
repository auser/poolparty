module PoolParty
  class Resource < Base
    
    default_options(
      :name => "resource"
    )
    
    def initialize(opts={}, extra_opts={}, &block)
      # self.class.add_has_and_does_not_have_methods_for(has_method_name)
      super
    end
    
    # has_name
    # The has_ and does_not_have methods names
    # are considered, unless otherwise denoted to be 
    # the top level class name
    # for instance
    #   class Tengo < Resource
    #   end
    # the has_ method will be
    #   has_tengo
    def has_method_name
      self.class.to_s.top_level_class
    end
    
    # Adds two methods to the module
    # Adds the method type:
    #   has_
    # and 
    #   does_not_have_
    # for the type passed
    # for instance
    # add_has_and_does_not_have_methods_for(:file)
    # gives you the methods has_file and does_not_have_file
    # TODO: Refactor nicely to include other types that don't accept ensure
    def self.add_has_and_does_not_have_methods_for(typ=:file)
      method_name = "__#{typ}"
      ev=<<-EOE
        def has_#{typ}(opts={}, extra={}, &block)
          puts "Create a new #{typ}"
          # #{method_name}({:ensures => :present}.merge(handle_option_values(opts).merge(extra)), &block)
        end
        def does_not_have_#{typ}(opts={}, extra={}, &block)
          # #{method_name}({:ensures => :absent}.merge(handle_option_values(opts).merge(extra)), &block)
        end
      EOE
      module_eval ev, ($pool_specfile || "")
    end
    
  end
end

Dir["#{File.dirname(__FILE__)}/../resources/*.rb"].each {|lib| require lib }