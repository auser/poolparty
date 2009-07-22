module  CloudProviders
  module Connections
    
    def user(n=nil)
      @user ||= (n.nil? ? 'root' : n)
    end
    
    # hostname or ip to use when running remote commands
    def host(n=nil)
      @host ||= (n.nil? ? public_ip : n)
    end
    
    # Simply shell out and call ssh, simple, reliable and fewest dependencies, but slow
    def simplest_run_remote( command=[], extra_ssh_ops={})
      command = command.compact.join(' && ') if command.is_a? Array
      cmd = "ssh #{host} #{ssh_options(extra_ssh_ops)} '#{command}'"
      puts "\n--------------------------------\nrunning_remote:\n #{cmd}\n"
      puts %x{#{cmd}}
    end
    
    def ssh_options(opts={})
      o = {"-i" => keypair.full_filepath,
           "-l" => user,
           "-o" =>"StrictHostKeyChecking=no"
           }.merge(opts)
      o.collect{ |k,v| "#{k} #{v}"}.join(' ')
    end
    
    def rsync( source_path, destination_path, rsync_opts=['-v -a'] )
      dputs "rsync -e 'ssh #{ssh_options}' #{rsync_opts.join(' ')} #{source_path}  root@#{host}:#{destination_path}"
      out = %x{ rsync -e 'ssh #{ssh_options}' #{rsync_opts.join(' ')} #{source_path}  root@#{host}:#{destination_path} }
      puts out if debugging?
    end
    
  end
end