namespace(:instance) do
  # Find the instance we want to deal with
  # interface can be: num=0, i=0, inst=0, 0
  # defaults to the master instance (0)
  task :init do
    num = (ENV['num'] || ENV["i"] || ENV["inst"] || ARGV.shift || 0).to_i
    raise Exception.new("Please set the number of the instance (i.e. num=1, i=1, or as an argument)") unless num
    @node = PoolParty::Master.new.get_node(num)
  end
  # Ssh into the node
  desc "Remotely login to the remote instance"
  task :ssh => [:init] do
    @node.ssh
  end
  desc "Send a file to the remote instance"
  task :exec => :init do
    @node.ssh ENV['cmd']
  end
  # Send a file to the remote instance
  # as designated by src='' and dest=''
  desc "Send a file to the remote instance"
  task :scp => :init do
    @node.scp ENV['src'], ENV['dest']
  end
  # Execute a command on the remote instance as designated
  # by cmd=''
  desc "Execute cmd on a remote instance"
  task :exec => [:init] do
    cmd = ENV['cmd'] || "ls -l"
    puts @node.ssh(cmd.runnable)
  end
  # Restart all the services monitored by monit
  desc "Restart all the services"
  task :reload => [:init] do
    @node.restart_with_monit
  end
  # Start all the services monitored by monit
  desc "Start all services"
  task :load => [:init] do
    @node.start_with_monit
  end
  # Stop the services monitored by monit
  desc "Stop all services"
  task :stop => [:init] do
    @node.stop_with_monit
  end
  # Install the required services on this node
  desc "Install stack on this node"
  task :install => :init do          
    @node.install
  end
  # Turnoff this instance
  desc "Teardown instance"
  task :shutdown => :init do
    `ec2-terminate-instances #{@node.instance_id}`
  end
  # Configure this node and start the services
  desc "Configure the stack on this node"
  task :configure => :init do
    @node.configure
    @node.restart_with_monit
  end
end
