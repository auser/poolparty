=begin rdoc
  This overwriting of the Capistrano method missing allows our capistrano
  tasks to have access to the methods on the provisioner and options
  set on the clouds. This saves us the responsibility of setting variables
  in the dynamic cap file.
=end
#TODO# Clean up
module Capistrano
  class Configuration
    attr_accessor :cloud, :provisioner

    def method_missing_without_variables(sym, *args, &block)
      if parent.respond_to?(sym)
        parent.send(sym, *args, &block)
      elsif provisioner.respond_to?(sym)
        provisioner.send(sym, *args, &block)
      elsif cloud.respond_to?(sym)
        cloud.send(sym, *args, &block)
      elsif PoolParty::Default.options.has_key?(sym)
        PoolParty::Default.options[sym]
      elsif PoolParty::Default.respond_to?(sym)
        PoolParty::Default.send(sym, *args, &block)
      else
        super
      end
    end
      
    
    module Namespaces            
      class Namespace        
        
        def provisioner
          parent.provisioner
        end
        
        def cloud(name=nil)
          puts "name: #{name}"
          name ? PoolParty::Cloud.clouds[name] : parent.cloud
        end
        
        def parent
          cloud 
        end
        
        def method_missing(sym, *args, &block)
          # if parent
          #   parent.send(sym, *args, &block) rescue super
          # else
          #   super
          # end
          if parent.respond_to?(sym)
            parent.send(sym, *args, &block)
          elsif provisioner.respond_to?(sym)
            provisioner.send(sym, *args, &block)
          elsif cloud.respond_to?(sym)
            cloud.send(sym, *args, &block)
          elsif PoolParty::Default.options.has_key?(sym)
            PoolParty::Default.options[sym]
          elsif PoolParty::Default.respond_to?(sym)
            PoolParty::Default.send(sym, *args, &block)
          else
            super
          end

        end
      end
    end    
  end
end