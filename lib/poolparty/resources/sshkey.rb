module PoolParty    
  module Resources
    
    class Sshkey < Resource
      
      def initialize(opts={}, extra_opts={}, &block)
        super(opts, extra_opts, &block)
        @key = Key.new(keypath? ? keypath : nil)
        options[:key] = @key.content
      end
      
      def name(i=nil)
        if i
          options[:name] = i
        else
          options[:name] ? options[:name] : ::File.basename(@key.full_filepath)
        end
      end
      
      def enctype(i=nil)
        i ? options[:type] = i : options[:type]
      end
      
    end
    
  end
end