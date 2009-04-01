module PoolParty
      
  module Daemonizeable
    
    attr_accessor :pid_file
    
    def self.included(base)
      base.extend ClassMethods
    end
    
    def pid
      @pid ||= File.file?(pid_file) ? open(pid_file).read.to_i : nil
    end
    
    def pid_file
      @pid_file ||= PoolParty::Default.agent_pid_file
    end
    
    # Returns +true+ the process identied by +pid+ is running.
    def running?
      return false unless pid
      Process.getpgid(pid) != -1
    rescue Errno::ESRCH
      false
    end
    
    # Turns the current script into a daemon process that detaches from the console.
    def daemonize(&block)
      raise unless pid_file

      remove_stale_pid_file
      
      pwd = Dir.pwd # Current directory is changed during daemonization, so store it
      
      vputs "Daemonizing..."
      trap("CHLD") {Process.wait(-1, Process::WNOHANG)}        
      @pid = fork do
        Signal.trap('HUP', 'IGNORE') # Don't die upon logout
        File.open("/dev/null", "r+") do |devnull|
          $stdout.reopen(devnull)
          $stderr.reopen(devnull)
          $stdin.reopen(devnull) unless @use_stdin
        end
        block.call if block
      end
      
      Dir.chdir(pwd)
      
      write_pid_file
      
      trap('HUP') { restart }
      at_exit do
        remove_pid_file
      end
      Process.detach(pid)
    end
    
    # Register a proc to be called to restart the server.
    def on_restart(&block)
      @on_restart = block
    end
    
    # Restart the server.
    def restart
      raise ArgumentError, "Can't restart, no 'on_restart' proc specified" unless @on_restart
      stop
      remove_pid_file
      @on_restart.call
      exit!
    end
    
    module ClassMethods
      # Send a QUIT signal the process which PID is stored in +pid_file+.
      # If the process is still running after +timeout+, KILL signal is
      # sent.
      def kill(pid_file, timeout=60)
        if pid = send_signal('QUIT', pid_file)
          Timeout.timeout(timeout) do
            sleep 0.1 while Process.running?(pid)
          end
        end
      rescue Timeout::Error
        print "Timeout! "
        send_signal('KILL', pid_file)
      rescue Interrupt
        send_signal('KILL', pid_file)
      ensure
        puts pid_file
        File.delete(pid_file) if File.exists?(pid_file)
      end
      
      # Restart the server by sending HUP signal.
      def restart(pid_file)
        send_signal('HUP', pid_file)
      end
      
      # Send a +signal+ to the process which PID is stored in +pid_file+.
      def send_signal(signal, pid_file)
        if File.exist?(pid_file) && pid = open(pid_file).read
          pid = pid.to_i
          print "Sending #{signal} signal to process #{pid} ... "
          Process.kill(signal, pid)
          puts
          pid
        else
          puts "Can't stop process, no PID found in #{pid_file}"
          nil
        end
      rescue Errno::ESRCH # No such process
        puts "process not found!"
        nil
      end
    end
    
    protected
      def remove_pid_file
        File.delete(@pid_file) if @pid_file && File.exists?(@pid_file)
      end
    
      def write_pid_file
        FileUtils.mkdir_p File.dirname(@pid_file)
        open(@pid_file,"w") { |f| f.write(@pid) }
        File.chmod(0644, @pid_file)
      end
      
      # If PID file is stale, remove it.
      def remove_stale_pid_file
        if File.exist?(@pid_file)
          if pid && running?
            puts "Pid file exists" and raise
          else
            remove_pid_file
          end
        end
      end
  end
  class Daemonize
    include Daemonizeable
  end
end