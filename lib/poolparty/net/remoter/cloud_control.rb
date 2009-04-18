require "ping"

module PoolParty
  module Remote
    
    # TODO: Deprecate
    # # A convenience method for waiting until there are no more
    # # pending instances and then running the block
    # def when_no_pending_instances(&block)
    #   reset!
    #   if list_of_pending_instances && list_of_pending_instances.size == 0
    #     vputs "" # Clear the terminal with a newline
    #     block.call if block
    #   else
    #     vprint "."
    #     wait "5.seconds"
    #     when_no_pending_instances(&block)
    #   end
    # end
    # 
    # # Stub method for the time being to handle expansion of the cloud
    # def can_expand_cloud?(force=false)
    #   (are_too_few_instances_running? || are_expansion_rules_valid? ) || force || false
    # end
    # def are_expansion_rules_valid?
    #   valid_rules?(:expand_when)
    # end
    # # Stub method for the time being to handle the contraction of the cloud
    # def can_contract_cloud?(force=false)
    #   return true if force
    #   ((are_any_nodes_exceeding_minimum_runtime? and are_too_many_instances_running?) || are_contraction_rules_valid?) || false
    # end
    # def are_contraction_rules_valid?
    #   valid_rules?(:contract_when)
    # end
    # # Expand the cloud
    # # If we can start a new instance and the load requires us to expand
    # # the cloud, then we should request_launch_new_instances
    # # Wait for the instance to boot up and when it does come back
    # # online, then provision it as a slave, this way, it is ready for action from the
    # # get go
    # def expand_cloud_if_necessary(force=false)
    #   if can_start_a_new_instance? && can_expand_cloud?(force)
    #     vputs "Expanding the cloud based on load"
    #     @num = 1
    #     @num.times do |i|
    #       list_of_pending_instances.size == 0 ? request_launch_one_instance_at_a_time : wait("5.seconds")          
    #       reset!
    #       vputs "request_launch_new_instances: #{@num}"
    #       provision_slaves_from_n(@num)
    #       after_launched
    #     end
    #   end
    # end
    # # Contract the cloud
    # # If we can shutdown an instnace and the load allows us to contract
    # # the cloud, then we should request_termination_of_non_master_instance
    # def contract_cloud_if_necessary(force=false)
    #   if can_shutdown_an_instance? && can_contract_cloud?(force)
    #     vputs "Shrinking the cloud by 1"
    #     before_shutdown
    #     request_termination_of_non_master_instance
    #   end
    # end
    # 
    # # List calculation methods
    # # 
    # # Are the minimum number of instances running?
    # def minimum_number_of_instances_are_running?
    #   instances_by_status("running").size >= minimum_instances.to_i
    # end
    # # Are the minimum number of instances NOT running?
    # def minimum_number_of_instances_are_not_running?
    #   !(minimum_number_of_instances_are_running?)
    # end
    # # Can we shutdown an instance?
    # def can_shutdown_an_instance?
    #   instances_by_status("running").size > minimum_instances.to_i
    # end
    # # Are too few instances running?
    # def are_too_few_instances_running?
    #   instances_by_status("running").size < minimum_instances.to_i
    # end
    # # Are there more instances than allowed?
    # def are_too_many_instances_running?
    #   instances_by_status("running").size > maximum_instances.to_i
    # end
    # 
    # ########
    # # TODO: deprecate methods below here (only if they are deprecate-able)
    # ########
    # 
    # # Request to launch a number of instances
    # def request_launch_new_instances(num=1)
    #   out = []
    #   num.times {out << launch_new_instance!(options) }
    #   out
    # end
    # def request_launch_master_instance
    #   @inst = launch_new_instance!
    #   wait "5.seconds"
    #   when_no_pending_instances do
    #     vputs "Master has launched"
    #     reset!
    #     after_launch_master(@inst)
    #   end
    # end
    # 
    # 
    # def after_launch_master(inst=nil)
    #   vputs "After launch master in remoter"
    # end
    # # Let's terminate an instance that is not the master instance
    # def request_termination_of_non_master_instance
    #   inst = nonmaster_nonterminated_instances.last
    #   terminate_instance!(inst.instance_id) if inst
    # end
    # # Can we start a new instance?
    # def can_start_a_new_instance?
    #   maximum_number_of_instances_are_not_running? && list_of_pending_instances.size == 0
    # end
    # # Are the maximum number of instances not running?
    # def maximum_number_of_instances_are_not_running?
    #   instances_by_status("running").size < maximum_instances.to_i
    # end
    # # Are the maximum number of instances running?
    # def maximum_number_of_instances_are_running?
    #   instances_by_status("running").size >= maximum_instances.to_i
    # end
    # # Launch new instance while waiting for the number of pending instances
    # #  to be zero before actually launching. This ensures that we only
    # #  launch one instance at a time
    # def request_launch_one_instance_at_a_time
    #   when_no_pending_instances { launch_new_instance! }
    # end
    # 
    # # A convenience method for waiting until all the instances have an ip
    # # assigned to them. This is useful when shifting the ip addresses
    # # around on the instances
    # def when_all_assigned_ips(&block)
    #   reset!
    #   if list_of_nonterminated_instances.select {|a| a.ip == 'not.assigned' }.empty?          
    #     block.call if block
    #   else
    #     vprint "."
    #     wait "5.seconds"
    #     when_all_assigned_ips(&block)
    #   end
    # end
    # def running_instance_ips
    #   remote_instances_list.select {|inst| 
    #     inst.running? and inst.ip!='not.assigned'
    #   }.collect{|n| n.ip}
    # end
    # 
    # # This will launch the minimum_instances if the minimum number of instances are not running
    # # If the minimum number of instances are not running and if we can start a new instance
    # def launch_minimum_number_of_instances        
    #   if can_start_a_new_instance? && !minimum_number_of_instances_are_running?         
    #     list_of_pending_instances.size == 0 ? request_launch_one_instance_at_a_time : wait("5.seconds")
    #     reset!
    #     launch_minimum_number_of_instances
    #     provision_slaves_from_n(minimum_instances.to_i)
    #     after_launched
    #   end
    # end
    # 
    # def provision_slaves_from_n(num=1)
    #   vputs "In provision_slaves_from_n: #{num}"
    #   reset!
    #   when_no_pending_instances do
    #     vputs "Waiting for 10 seconds"
    #     wait "10.seconds" # Give some time for ssh to startup          
    #     @num_instances = instances_by_status("running").size
    #     vputs "(@num_instances - (num))..(@num_instances): #{(@num_instances - (num))..(@num_instances)}"
    #     last_instances = nonmaster_nonterminated_instances[(@num_instances - (num))..(@num_instances)]
    #     last_instances.each do |inst|
    #       vputs "Provision slave: #{inst}"
    #       verbose ? provisioner_for(inst).install(testing) : hide_output { provisioner_for(inst).install(testing)}
    #     end
    #     # PoolParty::Provisioner.reconfigure_master(self)
    #   end
    # end

    def list_of_nodes_exceeding_minimum_runtime
      instances_by_status("running").reject{|i| i.elapsed_runtime < minimum_runtime}
    end
    
    def are_any_nodes_exceeding_minimum_runtime?
      !list_of_nodes_exceeding_minimum_runtime.blank?
    end
    # Is there a node that is running with the name master
    def is_master_running?
      !instances_by_status("running").select {|a| a.name == "master"}.first.nil?
    end    
    
  end
end