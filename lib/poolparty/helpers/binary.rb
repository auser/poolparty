require "ftools"
module PoolParty
  module Binary
    
    class << self
      
      def list_binaries_for(ty="pool")
        available_binaries_for(ty).join(", ")
      end
      def available_binaries_for(ty="pool")
        Dir["#{binary_directory}/#{ty}-*"].map {|a| File.basename(a.gsub(/#{ty}-/, '')) }.sort
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