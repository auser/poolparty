# Rakefile for workflow for the testing server
namespace :metavirt do
  desc "workflow for testing server on metavirt for poolparty"
  task :ci do
    # git checkout the examples directory
    # for each example
    #   open the cloud
    #   change the remoter base to metavirt
    #   start the cloud
    #   run the verifiers
    #   record success
    successful_clouds = []
    failed_clouds = []
    
    if ::File.directory?("poolparty-examples")
      `cd poolparty-examples && git pull origin master`
    else
      Kernel.system "git clone git://github.com/jashmenn/poolparty-examples.git"
    end
    $:.unshift("#{File.dirname(__FILE__)}/../lib/poolparty")
    $:.unshift("#{File.dirname(__FILE__)}/../../poolparty-extensions/lib")
    require "poolparty"
    
    Dir["poolparty-examples/*"].each do |dir|
      next if ::File.file?(dir)
      puts ""
      puts "******** testing #{dir} ********"
      
      require "#{dir}/clouds.rb"
      # PoolParty::Pool::Pool.load_from_file(dir/"clouds.rb")
      clouds.each do |cloud_name, cloud|
        begin
          begin
            consume_cloud_for_testing(cloud)
          rescue Exception => e
            failed(cloud_name, e, "Cloud consuming failure")
          end
          begin
            start_the_cloud(cloud)
          rescue Exception => e
            failed(cloud_name, e, "Cloud starting failed")
          end
          begin
            verify_the_cloud(cloud)
          rescue Exception => e
            failed(cloud_name, e, "Cloud verification failed")
          end          
          begin
            terminate_the_cloud(cloud)
          rescue Exception => e
            failed(cloud_name, e, "Cloud termination failed")
          end
        rescue Exception => e
          failed(cloud_name, e, "Error loading cloud")
        end
                
      end
      PoolParty::Pool::Pool.reset!
    end
    
    if failed_clouds
      puts "\n********************* #{failed_clouds.size} FAILED TESTS *********************"
      failed_clouds.each {|cloud_name, e, msg| $stderr.puts "#{cloud_name}: #{msg}\n#{e.inspect}" }
      puts "**********************************************************"
    end
  end
  
  private
  
  def failed(cloud_name, error, msg)
    failed_clouds << [cloud_name, msg, e]
    next
  end
  
  # Consume the cloud for testing
  # take the cloud and replace it's remoter_base
  # with metavirt for testing
  def consume_cloud_for_testing(cloud)
    orig_remoter_base = cloud.remoter_base
    cloud.instance_eval "@remote_base = nil"
    
    cloud.using :metavirt do
      server_config({:host=>"127.0.0.1", :port=>3000})
      authorized_keys 'ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCTppECfx7Tb0zoviRfqFaePyAek6+ZktKkHiTHu/jkhG1s4q1oHEe89no21xLxuReyJrDlNe8rLxxZzoYCaAWRdhcqMR3BNqb2w2jpF4pH+bFj0557KrwWP6HSNpRRkyYhxLqZbuH/2t3TzkPevZbcfSYa09jIzqnmTruh9l1s+n5E3cNr/RDdDn7tv3Ok7mKN7GEjkK7F83Pt9xviHevg22xqzm99nS+hg6Kl/fQUTO6pOmC5x+9V47RJz1+9WdhGJ7M83zijX9rMnAwrR5LFoL6aZyyU0G71SpoIL5e8XD/jt1WNKFJOfG8YMLb3i03UABm/Q5Q30+R7UoRxSWRX'
      
      using :libvirt do
        image_id 'mvi_ef77fdf0'
      end
    end
    
    puts "#{cloud.name}\t -> running on #{orig_remoter_base} (running on #{cloud.remoter_base} for testing)"
    cloud
  end
  
  # Start the cloud in testing mode
  def start_the_cloud(cld)
    puts "Starting cloud #{cld.name} (#{cld.keypair})"
    puts "#{cld.nodes(:status => "running").size} running instances (#{cld.minimum_instances} - #{cld.maximum_instances})"
    
    cld.launch_instance!(:cloud_name => cld.name) do |node|
      
      ::PoolParty::Provision::BootStrapper.new(node.ip, :cloud => cld)
      ::PoolParty::Provision::DrConfigure.new(node.ip,  :cloud => cld)
      
      puts <<-EOM
        Your cloud has started. Your ip is #{node.ip}
      EOM
    end
  end
  
  # Runt he cloud's verifiers
  def verify_the_cloud(cld)
    if cld.verifiers.size > 0
      cld.passing? && true rescue false
    else
      $stderr.puts("#{cld.name} doesn't have any verifiers")
      false
    end    
  end
  
  # Turn off the nodes that are running!
  def terminate_the_cloud(cld)
    cld.nodes(:status => "running").each do |inst|
      if o
        puts "Shutting down #{inst.instance_id}"
        cld.terminate_instance!(:instance_id => inst.instance_id)
      else
        if are_you_sure?(msg)
          puts "Shutting down #{inst.instance_id}"
          cld.terminate_instance!(:instance_id => inst.instance_id)
        end
      end
    end
  end
  
end