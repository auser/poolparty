module PoolParty
  class Base
    plugin :poolparty_base_packages do
      
      def enable        
        has_cron "/usr/bin/server-manage-election" do
          minute "0,5,10,15,20,25,30,35,40,45,50,55"
          command "/usr/bin/server-manage-election"
        end
      end
      
    end
  end
end