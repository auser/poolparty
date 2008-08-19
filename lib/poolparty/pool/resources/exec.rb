module PoolParty    
  module Resources
    
    def exec(opts={}, &block)
      returning PoolParty::Resources::Exec.new(opts, &block) do |r|
        resources << r
      end
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