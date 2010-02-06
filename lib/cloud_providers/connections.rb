require "open3"

module CloudProviders
  module Connections
        
    # hostname or ip to use when running remote commands
    def host(n=nil)
      if n.nil? 
        @host ||= dns_name
      else
        @host = n
      end
    end
    
    def ping_port(host, port=22, retry_times=400)
      connected = false
      retry_times.times do |i|
        begin
          break if connected = TCPSocket.new(host, port).is_a?(TCPSocket)
        rescue Exception => e
          sleep(2)
        end
      end
      false
    end
    
    def run(commands, o={})
      ssh(commands)
    end
  
    def shell_escape(str)
      String(str).gsub(/(?=["'\\$])/n, '\\').
        gsub(/\n/, "'\n'").
        sub(/^$/, "''")
    end

    def ssh( commands=[], extra_ssh_ops={})
      # Get the environment hash out of
      # the extra_ssh_ops and then delete
      # the element
      env = extra_ssh_ops[:env] || {}
      extra_ssh_ops.delete :env

      # Decide to use sudo or not
      do_sudo = true
      if extra_ssh_ops.has_key? :do_sudo
        do_sudo = extra_ssh_ops[:do_sudo] 
        extra_ssh_ops.delete :do_sudo
      end

      envstring = env.collect {|k,v| "#{k}=#{v}"}.join ' && '
      envstring += " && " unless envstring.size == 0
      ssh_string = "ssh #{user}@#{host} #{ssh_options(extra_ssh_ops)}"

      if commands.empty?
        #TODO: replace this with a IO.popen call with read_nonblocking to show progress, and accept input
        Kernel.system(ssh_string)
      else
        r = nil
        commands.each do |command|

          cmd = "#{envstring}#{command}" 
          if do_sudo
            sudocmd = %Q% sudo sh -c "#{shell_escape cmd}" % 
          else
            sudocmd = cmd
          end

          r = system_run ssh_string + %Q% "#{shell_escape sudocmd}"% 
        end
        r
      end
    end
    
    # Take a hash of options and join them into a string, combined with default options.
    # Default options are -o StrictHostKeyChecking=no -i keypair.full_filepath -l user
    # {'-i'=>'keyfile, '-l' => 'fred' } would become
    # "-i keyfile -o StrictHostKeyChecking=no -i keypair.to_s -l fred"
    def ssh_options(opts={})
      return @ssh_options if @ssh_options && opts.empty?
      ssh_opts = {"-i" => keypair.full_filepath,
           "-o" =>"StrictHostKeyChecking=no",
           }.merge(opts)
      @ssh_options = ssh_opts.collect{ |k,v| "#{k} #{v}"}.join(' ')
    end
    
    def rsync( opts={} )
      raise StandardError.new("You must pass a :source=>uri option to rsync") unless opts[:source]
      destination_path = opts[:destination] || opts[:source]
      rsync_opts = opts[:rsync_opts] || '-va'
      rsync_opts += %q% --rsync-path="sudo rsync" %
      cmd_string =  "rsync -L  -e 'ssh #{ssh_options}' #{rsync_opts} #{opts[:source]}  #{user}@#{host}:#{destination_path}"
      out = system_run(cmd_string)
      out
    end
    
    def scp(opts={})
      source = opts[:source]
      destination_path = opts[:destination] || opts[:source]
      raise StandardError.new("You must pass a local_file to scp") unless source
      scp_opts = opts[:scp_opts] || ""
      cmd_string = "scp #{ssh_options(scp_opts)} #{source} #{user}@#{host}:#{destination_path}"
      out = system_run(cmd_string)
      out
    end
    
    private
    
    # Execute command locally.
    # This method is mainly broken out to ease testing in the other methods
    # It opens the 3 IO outputs (stdin, stdout, stderr) and print the output out
    # as the command runs, unless the quiet option is passed in
    def system_run(cmd, o={})
      opts = {:quiet => false, :sysread => 1024}.merge(o)
      buf = ""
      puts("Running command: #{cmd}")
      Open3.popen3(cmd) do |stdout, stdin, stderr|
        begin
          while (chunk = stdin.readpartial(opts[:sysread]))
            buf << chunk
            unless chunk.nil? || chunk.empty?
              $stdout.write(chunk) #if debugging? || verbose?
            end
          end
          err = stderr.readlines
          $stderr.write_nonblock(err)
        rescue SystemCallError => error
          err = stderr.readlines
          $stderr.write_nonblock(err)
        rescue EOFError => error
          err = stderr.readlines
          $stderr.write_nonblock(err)
          # used to do nothing
        end
      end
      unless $?.success?
        warn "Failed sshing. Check ~/.poolparty/ssh.log for details"
      end
      buf
    end
    
  end
end
