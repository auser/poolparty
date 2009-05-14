require "#{::File.dirname(__FILE__)}/../../modules/pinger"
require 'rubygems'
require 'net/ssh'

module PoolParty
  module Remote
    
    include ::PoolParty::Pinger
    
    def target_host(dns_or_ip=nil)
      dns_or_ip ? @target_host=dns_or_ip : @target_host
    end
  
    # Simply shell out and call ssh, simple, reliable and fewest dependencies, but slow
    def simplest_run_remote(host=target_host, command=[], extra_ssh_ops={})
      command = command.compact.join(' && ') if command.is_a? Array
      cmd = "ssh #{host} #{ssh_options(extra_ssh_ops)} '#{command}'"
      puts "\n--------------------------------\nrunning_remote:\n #{cmd}\n"
      puts %x{#{cmd}}
    end
    
    def run_remote( hst, cmds )
      netssh hst, cmds
    end
    
    def ssh_into(inst, extra_ssh_ops={} )
      ip =  ((inst.respond_to?(:has_key) && inst.has_key?(:ip)) || inst.respond_to?(:ip)) ? inst.ip : inst
      Kernel.system("ssh #{ssh_options(extra_ssh_ops)} #{ip}")
    end
    
    def ssh_options(opts={})
      o = {"-i" => keypair_path,
           "-l" => user,
           "-o" =>"StrictHostKeyChecking=no"
           }.merge(opts)
      o.collect{ |k,v| "#{k} #{v}"}.join(' ')
    end
  
    def rsync( source_path, destination_path, rsync_opts=['-v -a'] )
      dputs "rsync -e 'ssh #{ssh_options}' #{rsync_opts.join(' ')} #{source_path}  root@#{target_host}:#{destination_path}"
      out = %x{ rsync -e 'ssh #{ssh_options}' #{rsync_opts.join(' ')} #{source_path}  root@#{target_host}:#{destination_path} }
      puts out if debugging?
    end
   
    def run_local(commands)
      commands.each do |cmd|
        puts `#{cmd}`
      end
    end

    def commands
      @commands ||= Array.new
    end

    # TODO: make extendable multithreaded version
    def execute!(cmds=commands)
      netssh(
        [cmds.compact.join(' && ')],
        :host=>target_host, :user=>'root')
      # commands.each {|c| run_remote(c, target_host) }
    end
    
    def netssh(cmds=[], opts={})
      user = opts.delete(:user) || user #rescue 'root'
      host = opts.delete(:host) || target_host
      ssh_options_hash = {:keys => [keypair_path],
                          :auth_methods => 'publickey',
                          :paranoid => false
                           }.merge(opts)
      
      # Start the connection
      Net::SSH.start(host, user, ssh_options_hash) do |ssh|  
        cmds.each do |command|
          ssh.exec!(command) do |ch, stream, data|
            if stream == :stdout
             print data
            else
              $stderr.print "#{host} stderr => #{data}"
            end
          end
        end
      end
    end
    
    
##########################################################################################################   
# TODO: Delete deprecated commands below here
    
    def rsync_storage_files_to_command(remote_instance)
      #TODO: rsync_to_command("#{Default.storage_directory}/", Default.remote_storage_path, remote_storage_path) if remote_instance
      "#{rsync_command} #{Default.storage_directory}/ #{remote_instance.ip}:#{Default.remote_storage_path}" if remote_instance
    end
    # rsync a file to a node.  By default to the master node.
    def rsync_to_command(source, target=source, remote_instance=master)
      "#{rsync_command} #{source} #{remote_instance.ip}:#{target}"
    end
    def run_command_on_command(cmd="ls -l", remote_instance=nil)
      vputs "Running #{cmd} on #{remote_instance.name == %x[hostname].chomp ? "self (master)" : "#{remote_instance.name}"}"
      (remote_instance.nil? || remote_instance.name == %x[hostname].chomp) ? %x[#{cmd}] : "#{ssh_command(remote_instance)} '#{cmd}'"
    end
    def ssh_command(remote_instance)
      "#{ssh_string} #{remote_instance.ip}"
    end
    # Generic commandable strings
    def ssh_string
      (["ssh"] << ssh_array).join(" ")
    end
    # Array of ssh options
    # Includes StrictHostKeyChecking to no
    # Ssh with the user in Base
    # And including the keypair_path
    # "-l '#{Default.user}'", 
    def ssh_array
      ["-o StrictHostKeyChecking=no", "-l #{Default.user}", '-i "'+full_keypair_path+'"']
    end
    def scp_array
      ["-o StrictHostKeyChecking=no", '-i "'+full_keypair_path+'"']
    end
    def rsync_command
      "rsync -azP --exclude cache -e '#{ssh_string} -l #{Default.user}'"
    end
    def remote_ssh_array
      ["-o StrictHostKeyChecking=no", "-l '#{Default.user}'", '-i "'+remote_keypair_path+'"']
    end
    def remote_ssh_string
      (["ssh"] << remote_ssh_array).join(" ")
    end
    def remote_rsync_command
      "rsync -azP --exclude cache -e '#{remote_ssh_string}'"
    end
          
    def scp_to_command(source, dest=target, remote_instance=master)
      #TODO: check if source is Directory and add -r if it is
      "scp #{source} #{remote_instance.ip}:#{dest} #{scp_array.join(' ')}"
    end
    
    # Rsync a file or directory to a node.  Rsync to master by default
    def rsync_to(source, target=source, num=0)
      str = "#{rsync_to_command(source, target, get_instance_by_number( num ))}"
      dputs "Running: #{str}"
      verbose ?  Kernel.system(str) : hide_output {Kernel.system str}
    end
    
    # Rsync command to the instance
    def rsync_storage_files_to(instance=nil)
      hide_output {Kernel.system "#{rsync_storage_files_to_command(instance)}" if instance}
    end
    # Take the rsync command and execute it on the system
    # if there is an instance given
    def run_command_on(cmd, instance=nil)        
      Kernel.system "#{run_command_on_command(cmd, instance)}"
    end
    
    # Ssh into the instance given
    # def ssh_into(instance)
    #   cmd = "#{ssh_command(instance)}"
    #   vputs "Running #{cmd}"
    #   Kernel.system cmd if instance
    # end
    # Find the instance by the number given
    # and then ssh into the instance
    def ssh_into_instance_number(num=0)
      ssh_into( get_instance_by_number( num ) )
    end
    
    # Run command on the instance by the number
    def run_command_on_instance_number(cmd="ls -l", num=0)
      run_command_on(cmd, get_instance_by_number( num ) )
    end
    
  end
end