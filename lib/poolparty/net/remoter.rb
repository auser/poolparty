=begin rdoc
  This module is included by the remote module and defines the remoting methods
  that the clouds can use to rsync or run remote commands
=end
module PoolParty
  module Remote
    module Remoter
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

      def list_from_local
        list_file = get_working_listing_file
        if list_file
          out = returning Array.new do |instances|
            open(list_file).read.split("\n").each do |line|
              ip,name,responding,load = line.split(" ")
              instances << RemoteInstance.new({:ip => ip, :name => name, :responding => responding, :load => load}).to_s
            end
          end.join("\n")
        else
          out = "Cannot find list file"
        end
        return out
      end
      def list_from_remote
        out_array = returning Array.new do |instances|
          list_of_instances.each do |h|
            instances << PoolParty::Remote::RemoteInstance.new(h).to_s
          end
        end
        write_to_file(local_instances_list_file_locations.first, out_array.join("\n"))
        out_array.join("\n")
      end
      def get_working_listing_file
        local_instances_list_file_locations.reject {|f| f unless File.file?(f) }.first
      end
      def local_instances_list_file_locations
        [
          "#{Base.base_config_directory}/instances.list",
          "~/.instances.list",
          "~/instances.list",
          "#{Base.storage_directory}/instances.list",
          "instances.list"
        ]
      end

      def self.included(receiver)
        receiver.extend self      
      end
    end
  end
end