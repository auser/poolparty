=begin rdoc
  This module is included by the remote module and defines the remoting methods
  that the clouds can use to rsync or run remote commands
=end
module PoolParty
  module Remoter
    module ClassMethods
    end
    
    module InstanceMethods
      def ssh_string
        (["ssh"] << ssh_array).join(" ")
      end
      def ssh_array
        ["-o StrictHostKeyChecking=no", "-l '#{Base.user}'", "-i '#{keypair_path}'"]
      end
      def rsync_command
        "rsync --delete -azP -e '#{ssh_string}' "
      end
    end
    
    def self.included(receiver)
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods
    end
  end
end