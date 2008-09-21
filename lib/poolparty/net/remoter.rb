=begin rdoc
  This module is included by the remote module and defines the remoting methods
  that the clouds can use to rsync or run remote commands
=end
module PoolParty
  module Remoter    
    module InstanceMethods
      def rsync_storage_files_to_command(remote_instance)
        if remote_instance
          "#{rsync_command} #{Base.storage_directory} #{remote_instance.ip}:#{Base.remote_storage_path}"
        end
      end
      # Generic commandable strings
      def ssh_string
        (["ssh"] << ssh_array).join(" ")
      end
      def ssh_array
        ["-o StrictHostKeyChecking=no", "-l '#{Base.user}'", "-i '#{keypair_path}'"]
      end
      def rsync_command
        "rsync --delete -azP --exclude cache --exclude temp -e '#{ssh_string}'"
      end
    end
    
    module ClassMethods
    end
    
    def self.included(receiver)
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods
    end
  end
end