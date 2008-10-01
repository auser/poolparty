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
      # Array of ssh options
      # Includes StrictHostKeyChecking to no
      # Ssh with the user in Base
      # And including the keypair_path
      def ssh_array
        ["-o StrictHostKeyChecking=no", "-l '#{Base.user}'", "-i '#{keypair_path}'"]
      end
      def rsync_command
        "rsync --delete -azP --exclude cache -e '#{ssh_string}'"
      end
      # Open the cached local copy of the instances list and 
      # create a new RemoteInstance from each line
      def list_from_local
        list_file = get_working_listing_file
        if list_file
          out = returning Array.new do |instances|
            open(list_file).read.split("\n").each do |line|
              instances << RemoteInstance.new(line)
            end
          end
        else
          out = "Cannot find list file"
        end
        return out
      end
      # List the instances that are known from the remoter_base
      # Create a RemoteInstance for each of the instances from the hash
      # returned by the list of instances, write them to the cached file
      # and then return the array of instances
      def list_from_remote(options={})
        out_array = get_remote_nodes
        write_to_file(local_instances_list_file_locations.first, out_array.map{|a| a.to_s}.join("\n")) if options[:cache]
        out_array
      end
      def list_of_node_names(options={})
        list_from_local.collect {|ri| ri.name }
      end
      def list_of_node_ips(options={})
        list_from_local.collect {|ri| ri.ip }
      end
      def get_remote_nodes
        returning Array.new do |instances|
          list_of_instances(respond_to?(:keypair) ? keypair : nil).each do |h|
            instances << PoolParty::Remote::RemoteInstance.new(h)
          end
        end
      end
      # Get the instance first instance file that exists on the system from the expected places
      # denoted in the local_instances_list_file_locations
      def get_working_listing_file
        local_instances_list_file_locations.reject {|f| f unless File.file?(f) }.first
      end
      # Expected places for the instances.list to be located at on the machine
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