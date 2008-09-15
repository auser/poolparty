module PoolParty
  module Binary
    
    class << self
      
      def list_binaries
        available_binaries.join(", ")
      end
      def available_binaries
        Dir["#{binary_directory}/pool-*"].map {|a| File.basename(a.gsub(/pool-/, '')) }.sort
      end
      def binary_directory
        "#{File.dirname(__FILE__)}/../../../bin"
      end
      
      
    end
    
  end
end