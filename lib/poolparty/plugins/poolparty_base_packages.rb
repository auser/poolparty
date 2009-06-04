module PoolParty
  module Plugin
    class PoolPartyBasePackages < Plugin
      
      def enable        
        has_cron "/usr/bin/server-manage-election" do
          minute "*"
          command "/usr/bin/server-manage-election"
        end
      end
      
    end
  end
end