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
        "rsync --delete -azP --exclude cache -e '#{ssh_string}'"
      end
    end
    
    module ClassMethods
      def list_from_local
        list_file = get_working_listing_file
        if list_file
          returning Array.new do |instances|
            open(list_file).read.split("\n").each do |line|
              ip,name = line.split("\t")
              instances << RemoteInstance.new({:ip => ip, :name => name}).to_s
            end
          end.join("\n")
        else
          return "Cannot find list file"
        end
      end
      def list_from_remote        
      end
      def get_working_listing_file
        local_instances_list_file_locations.select {|f| File.file?(f) }.first
      end
      def local_instances_list_file_locations
        [
          "#{Base.base_config_directory}/instances.list",
          "~/.instances.list",
          "~/instances.list",
          "instances.list"
        ]
      end
    end
    
    def self.included(receiver)
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods
      receiver.send :include, Remote
    end
  end
end