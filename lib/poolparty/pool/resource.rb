=begin rdoc
  The Container

  Container holds the various features for the final compilations
  for each pool.
  
  Most of the Resources will not need to create their own
=end
module PoolParty    
  module Resources
    
    class Resource
      include MethodMissingSugar
      
      def initialize(&block)
        self.instance_eval &block if block_given?
      end
      # Generic to_s
      # Most Resources won't need to extend this
      def to_s
        returning Array.new do |output|
          output << "#{self.class.to_s.split("::")[-1].downcase} {"
            instances.each do |opts|
              output << "\t#{::File.basename(opts[:name])}:"
              output << opts.flush_out_options({:prev => "\t\t", :post => ";"})
            end
          output << "}"
        end.join("\n")
      end
      # Each container has instances      
      def instances
        @instances ||= []
      end
      def <<(*args)
        args.each {|arg| instances << options.merge(arg) }
      end
      alias_method :push, :<<
      
      def options
        {}
      end
    end
    
  end
end