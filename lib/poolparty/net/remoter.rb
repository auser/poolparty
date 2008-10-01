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
      # Get the names of the nodes. Mainly used for puppet templating
      def list_of_node_names(options={})
        list_from_local.collect {|ri| ri.name }
      end
      # An array of node ips. Mainly used for puppet templating
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
      
      # More functional methods
      # Are the minimum number of instances running?
      def minimum_number_of_instances_are_running?
        list_of_running_instances.size > minimum_instances
      end
      # Can we shutdown an instance?
      def can_shutdown_an_instance?
        minimum_number_of_instances_are_running?
      end
      # Request to launch a number of instances
      def request_launch_new_instances(num=1)
        out = []
        num.times {out << launch_new_instance!}
        out
      end
      # Can we start a new instance?
      def can_start_a_new_instance?
        maximum_number_of_instances_are_not_running?
      end
      # Are the maximum number of instances running?
      def maximum_number_of_instances_are_not_running?
        list_of_running_instances.size < Application.maximum_instances
      end
      # Launch new instance while waiting for the number of pending instances
      #  to be zero before actually launching. This ensures that we only
      #  launch one instance at a time
      def request_launch_one_instance_at_a_time
        reset!
        while !list_of_pending_instances.size.zero?
          wait "5.seconds"
          reset!
        end
        return launch_new_instance!
      end

      def self.included(receiver)
        receiver.extend self
      end
    end
  end
end