module  CloudProviders
  module Connections
    
    def user(n=nil)
      if n.nil? 
        @user ||= 'root'
      else
        @user = n
      end
    end
    
    # hostname or ip to use when running remote commands
    def host(n=nil)
      if n.nil? 
        @host ||= dns_name
      else
        @host = n
      end
    end
    
    def run(commands, opts={})
      ssh(commands, opts)
    end
    
    # Simply shell out and call ssh, simple, reliable and fewest dependencies, but slow
    def ssh( commands=[], extra_ssh_ops={})
      commands = commands.compact.join(' && ') if commands.is_a?(Array)
      command_string = commands.empty? ? nil : "'#{commands}'"
      cmd_string = "ssh #{user}@#{host} #{ssh_options(extra_ssh_ops)} #{command_string}"
      system_run(cmd_string)
    end
    
    # Take a hash of options and join them into a string, combined with default options.
    # Default options are -o StrictHostKeyChecking=no -i keypair.full_filepath -l user
    # {'-i'=>'keyfile, '-l' => 'fred' } would become
    # "-i keyfile -o StrictHostKeyChecking=no -i keypair.to_s -l fred"
    def ssh_options(opts={})
      o = {"-i" => keypair.full_filepath,
           "-o" =>"StrictHostKeyChecking=no"
           }.merge(opts)
      o.collect{ |k,v| "#{k} #{v}"}.join(' ')
    end
    
    def rsync( opts={} )
      raise StandardError.new("You must pass a :source=>uri option to rsync") unless opts[:source]
      destination_path = opts[:destination] || opts[:source]
      rsync_opts = opts[:rsync_opts] || '-va'
      cmd_string =  "rsync -e 'ssh #{ssh_options}' #{rsync_opts} #{opts[:source]}  #{user}@#{host}:#{destination_path}"
      out = system_run(cmd_string)
      dputs(out)
      out
    end
    
    def scp(opts={})
      source = opts[:source]
      destination_path = opts[:destination] || opts[:source]
      raise StandardError.new("You must pass a local_file to scp") unless source
      scp_opts = opts[:scp_opts] || ""
      cmd_string = "scp #{source} #{user}@#{host}:#{destination_path} #{ssh_options(scp_opts)}"
      out = system_run(cmd_string)
      dputs(out)
      out
    end
    
    private
    # Execute command locally.
    # This method is mainly broken out to ease testing in the other methods
    def system_run(cmd)
      Kernel.system("#{cmd}")
    end
    
  end
end