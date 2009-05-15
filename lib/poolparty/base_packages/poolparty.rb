module PoolParty
  class Base
    plugin :poolparty_base_packages do
      
      def enable        
        has_cron "/usr/bin/server-manage-election" do
          minute "*"
          command "/usr/bin/server-manage-election"
        end
      end
      
    end
  end
end