namespace(:ec2) do        
  task :init do
    Application.options
  end
  # Start a new instance in the cloud
  desc "Add and start an instance to the pool"
  task :start_new_instance => [:init] do
    puts PoolParty::Remoting.new.launch_new_instance!
  end
  # Stop all the instances via command-line
  desc "Stop all running instances"
  task :stop_running_instances => [:init] do
    Thread.new {`ec2-describe-instances | grep INSTANCE | grep running | awk '{print $2}' | xargs ec2-terminate-instances`}
  end
  # Reboot the instances via commandline
  desc "Restart all running instances"
  task :restart_running_instances => [:init] do
    Thread.new {`ec2-describe-instances | grep INSTANCE | grep running | awk '{print $2}' | xargs ec2-reboot-instances`}
  end
end