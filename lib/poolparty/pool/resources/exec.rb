module PoolParty    
  module Resources
    
    def exec(opts={}, &block)
      resources[:exec] ||= PoolParty::Resources::Exec.new(opts, &block)
    end
    
    class Exec < Resource
      
      default_options({
        :path => "/usr/bin:/bin:/usr/local/bin"
      })
      
      def to_s
        raise Exception.new("No command for Exec resource") unless name || cmd
        super
      end
      
    end
    
  end
end