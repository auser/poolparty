module PoolParty
  module Resources
    
    class FakePlugin < Resource
      
      def self.has_method_name
        "fake_plugin"
      end
      
      def name
        "fake_plugin"
      end
      
      def after_loaded
        has_file "/etc/my_configs/special_config" do
          requires get_directory("/etc/my_configs")
        end
      end
      
    end
    
  end
end