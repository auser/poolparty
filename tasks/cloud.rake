# Cloud tasks
namespace(:cloud) do
  # Setup
  task :init do
    setup_application
    raise Exception.new("You must specify your access_key and secret_access_key") unless Base.access_key && Base.secret_access_key
  end
  # Install the stack on all of the nodes
  desc "Prepare all servers"
  task :prepare => :init do
    PoolParty::Master.new.nodes.each do |node|
      node.install
    end
  end
  # Start the cloud
  desc "Start the cloud"
  task :start => :init do
    PoolParty::Master.new.start_cloud!
  end
  # Reload the cloud with the new updated data
  desc "Reload all instances with updated data"
  task :reload => :init do
    PoolParty::Master.new.nodes.each do |node|
      node.configure
      node.restart_with_monit
    end
  end
  # List the cloud
  desc "List cloud"
  task :list => :init do
    puts PoolParty::Master.new.list
  end
  # Shutdown the cloud
  desc "Shutdown the entire cloud"
  task :shutdown => :init do
    PoolParty::Master.new.request_termination_of_all_instances
  end
  # Watch the cloud and scale it if necessary
  desc "Watch the cloud and maintain it"
  task :scale => :init do
    begin
      PoolParty::Master.new.scale_cloud!
    rescue Exception => e
      puts "There was an error scaling the cloud: #{e}"
    end
    
  end
  # Maintain the cloud in a background process
  desc "Maintain the cloud (run on the master)"
  task :maintain => :init do
    begin
      PoolParty::Master.new.start_monitor!
    rescue Exception => e
      puts "There was an error starting the monitor: #{e}"
    end
  end
end