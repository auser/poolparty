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
            "#{Base.base_config_directory}/#{Base.default_specfile_name}",
            "#{Base.remote_storage_path}/#{Base.default_specfile_name}", 
            ENV["POOL_SPEC"],
            "#{Base.default_specfile_name}",
            "#{Base.storage_directory}/#{Base.default_specfile_name}",            
        ].reject {|a| a.nil?}.reject do |f|
          f unless ::File.file?(f)
        end.first
      end
      
    end
    
  end
end