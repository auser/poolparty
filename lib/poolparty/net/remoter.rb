=begin rdoc
  This module is included by the remote module and defines the remoting methods
  that the clouds can use to rsync or run remote commands
=end
module PoolParty
  module Remote
    module Remoter
      def rsync_storage_files_to_command(remote_instance)
        if remote_instance
          "#{rsync_command} #{Base.storage_directory}/* #{remote_instance.ip}:#{Base.remote_storage_path}"
        end
      end
      def run_command_on_command(cmd="ls -l", remote_instance=nil)
        "#{ssh_string} #{remote_instance.ip} '#{cmd}'"
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
        ["-o StrictHostKeyChecking=no", "-l '#{Base.user}'", '-i "'+full_keypair_path+'"']
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
          out = list_from_remote(:cache => true)
        end
        return out
      end
      # List the instances that are known from the remoter_base
      # Create a RemoteInstance for each of the instances from the hash
      # returned by the list of instances, write them to the cached file
      # and then return the array of instances
      def list_from_remote(options={})
        out_array = get_remote_nodes
        write_to_file(local_instances_list_file_locations.first, out_array.map{|a| a.to_s }.join("\n")) if options[:cache]
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
          "#{Base.storage_directory}/#{name}-instances.list",
          "#{Base.base_config_directory}/#{name}-instances.list",
          "~/.#{name}-instances.list",
          "~/#{name}-instances.list",          
          "#{name}-instances.list"
        ]
      end
      
      # More functional methods
      # Are the minimum number of instances running?
      def minimum_number_of_instances_are_running?
        list_of_running_instances.size >= minimum_instances
      end
      # Can we shutdown an instance?
      def can_shutdown_an_instance?
        list_of_running_instances.size > minimum_instances
      end
      # Request to launch a number of instances
      def request_launch_new_instances(num=1)
        out = []
        num.times {out << launch_new_instance!}
        out
      end
      # Let's terminate an instance that is not the master instance
      def request_termination_of_non_master_instance
        inst = list_of_running_instances.reject {|a| a.master? }.last
        terminate_instance!(inst.name)
      end
      # Can we start a new instance?
      def can_start_a_new_instance?
        maximum_number_of_instances_are_not_running?
      end
      # Are the maximum number of instances running?
      def maximum_number_of_instances_are_not_running?
        list_of_running_instances.size < maximum_instances
      end
      # Launch new instance while waiting for the number of pending instances
      #  to be zero before actually launching. This ensures that we only
      #  launch one instance at a time
      def request_launch_one_instance_at_a_time
        reset!
        if list_of_pending_instances.size.zero?
          launch_new_instance!
        else
          puts "list_of_pending_instances"
          wait "5.seconds"
          request_launch_one_instance_at_a_time
        end
      end
      # This will launch the minimum_instances if the minimum number of instances are not running
      # If the minimum number of instances are not running and if we can start a new instance
      def launch_minimum_number_of_instances
        if can_start_a_new_instance?
          while !minimum_number_of_instances_are_running?
            request_launch_one_instance_at_a_time
            wait "5.seconds"
          end
        end
      end
      # Stub method for the time being to handle expansion of the cloud
      def should_expand_cloud?(force=false)
        force || false
      end
      # Stub method for the time being to handle the contraction of the cloud
      def should_contract_cloud?(force=false)
        force || false
      end
      # Expand the cloud
      # If we can start a new instance and the load requires us to expand
      # the cloud, then we should request_launch_new_instances
      def expand_cloud_if_necessary(force=false)
        if can_start_a_new_instance?
          request_launch_new_instances(1) if should_expand_cloud?(force)
        end
      end
      # Contract the cloud
      # If we can shutdown an instnace and the load allows us to contract
      # the cloud, then we should request_termination_of_non_master_instance
      def contract_cloud_if_necessary(force=false)
        if can_shutdown_an_instance?          
          request_termination_of_non_master_instance if should_contract_cloud?(force)
        end
      end
      
      # Rsync command to the instance
      def rsync_storage_files_to(instance=nil)
        if instance && instance.respond_to?(:ip) && !instance.ip.nil?
          Kernel.system "#{rsync_storage_files_to_command(instance)}"
        end
      end
      
      def run_command_on(cmd, instance=nil)
        if instance && instance.respond_to?(:ip) && !instance.ip.nil?
          Kernel.system "#{run_command_on_command(cmd, instance)}"
        end
      end

      def self.included(receiver)
        receiver.extend self
      end
    end
  end
end