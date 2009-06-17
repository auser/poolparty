require "open-uri"

module PoolParty
  class MonitorDaemon
    
    attr_reader :should_daemonize, :pid_file, :log_file_path, :sleep_time
    
    def self.run(o={})
      new(o).run
    end
    
    def initialize(o={})
      @should_daemonize = o.delete(:daemonize)
      @pid_file = o.delete(:daemonize) || "/tmp/poolparty_monitor.pid"
      temp_log_file_path = o.delete(:log_file_path) || "poolparty_monitor.log"
      @log_file_path = temp_log_file_path
      
      unless ::File.file?(temp_log_file_path)
        ::FileUtils.mkdir_p ::File.dirname(temp_log_file_path) unless ::File.directory?(::File.dirname(temp_log_file_path))
        ::File.open(temp_log_file_path, 'a+')
        temp_log_file_path
      end
      @sleep_time = o.delete(:sleep_time) || 5
    end
    
    def pass_the_baton
      %w(Memory neighborhood elections).each do |monitor|
        out = open("http://localhost:8642/#{monitor}").read
        log "#{monitor} / #{out.inspect}"        
      end
      sleep sleep_time
    end
    
    def run      
      if should_daemonize
        @should_daemonize = false
        daemonize
      else
        log "Starting MonitorDaemon"
        loop {pass_the_baton}        
      end
    end
    
    def daemonize(o={})
      raise unless pid_file
      
      pwd = Dir.pwd # Current directory is changed during daemonization, so store it
      
      remove_stale_pid_file
      pid = fork do
        Signal.trap('HUP') do
          restart
        end
        Signal.trap('INT') do
          stop!
        end
        Signal.trap("CHLD") do 
          Process.waitpid(pid, Process::WNOHANG)
        end
        File.open("/dev/null", "r+") do |devnull|
          $stdout.reopen(devnull)
          $stderr.reopen(devnull)
          $stdin.reopen(devnull) unless @use_stdin
        end
        run
      end
      
      Dir.chdir(pwd)
      
      write_pid_file(pid)

      Process.detach(pid)
    end
    
    def log(msg)
      log_file.flush
      log_file << "[INFO] - #{Time.now} -- #{msg}\n"
    end
    
    private
    
    def log_file
      if @logfile
        @logfile
      else
        begin
          ::FileUtils.mkdir_p ::File.dirname(log_file_path) unless ::File.directory?(::File.dirname(log_file_path))
          @logfile ||= ::File.open(log_file_path, 'a+')
        rescue Exception => e
          puts "ERROR: #{e.inspect}"
          @logfile = $stdout
        end
        
      end
    end
    
    def pid
      @pid ||= File.file?(pid_file) ? open(pid_file).read.to_i : nil
    end
    
    def stop!
      log "Stopping daemon"
      log_file.close
      send_signal("INT")
      remove_stale_pid_file
      exit 0
    end
    
    def restart
      log "TODO: implement restart for HUP signal on #{__FILE__}"
    end
    
    def running?
      return false unless pid
      Process.getpgid(pid) != -1
    rescue Errno::ESRCH
      false
    end
    
    def send_signal(signal)
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
    
    protected
    def remove_pid_file
      File.delete(pid_file) if pid_file && File.exists?(pid_file)
    end
    
    def write_pid_file(pid)
      FileUtils.mkdir_p File.dirname(pid_file)
      open(pid_file,"w") { |f| f.write(pid) }
      File.chmod(0644, pid_file)
    end
    
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
end