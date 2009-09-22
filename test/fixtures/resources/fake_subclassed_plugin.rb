module PoolParty
  module Resources
    
    class FakeSubclassedPlugin < Resource
      
      def self.has_method_name
        "subclassed"
      end
      
      def after_loaded
        has_file "/etc/my_configs/special_config" do
          requires get_directory("/etc/my_configs")
        end
      end
      
    end
    
  end
end