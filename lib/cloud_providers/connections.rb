module  CloudProviders
  module Connections
    
    def user(n=nil)
      @user ||= (n.nil? ? 'poolparty' : n)
    end
    
    # hostname or ip to use when running remote commands
    def host(n=nil)
      @host ||= (n.nil? ? dns_name : n)
    end
    
    def run(commands, opts={})
      ssh(commands, opts)
    end
    
    # Simply shell out and call ssh, simple, reliable and fewest dependencies, but slow
    def ssh( commands=[], extra_ssh_ops={})
      commands = commands.compact.join(' && ') if commands.is_a?(Array)
      cmd_string = "ssh #{host} #{ssh_options(extra_ssh_ops)} '#{commands}'"
      system_run(cmd_string)
    end
    
    # Take a hash of options and join them into a string, combined with default options.
    # Default options are -o StrictHostKeyChecking=no -i keypair.full_filepath -l user
    # {'-i'=>'keyfile, '-l' => 'fred' } would become
    # "-i keyfile -o StrictHostKeyChecking=no -i keypair.to_s -l fred"
    def ssh_options(opts={})
      o = {"-i" => keypair.full_filepath,
           "-l" => user,
           "-o" =>"StrictHostKeyChecking=no"
           }.merge(opts)
      o.collect{ |k,v| "#{k} #{v}"}.join(' ')
    end
    
    def rsync( opts={} )
      raise StandardError.new("You must pass a :source=>uri option to rsync") unless opts[:source]
      destination_path = opts[:destination] || opts[:source]
      rsync_opts = opts[:rsync_opts] || '-va'
      cmd_string =  "rsync -e 'ssh #{ssh_options}' #{rsync_opts} #{opts[:source]}  root@#{host}:#{destination_path}"
      out = system_run(cmd_string)
      puts out if debugging?
      out
    end
    
    private
    # Execute command locally.
    # This method is mainly broken out to ease testing in the other methods
    def system_run(cmd)
      `#{cmd}`
    end
    
  end
end