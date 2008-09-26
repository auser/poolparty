require "ftools"
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
        "#{::File.dirname(__FILE__)}/../../../bin"
      end
      def get_existing_spec_location
        [   
            ENV["POOL_SPEC"], 
            "pool.spec", 
            "#{Base.remote_storage_directory}/pool.spec", 
            "#{Base.storage_directory}/pool.spec"
        ].reject {|a| a.nil?}.reject do |f|
          f unless ::File.file?(f)
        end.first
      end
      
    end
    
  end
end