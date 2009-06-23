require "open-uri"
require "json"
require "#{::File.dirname(__FILE__)}/base_monitor"

module PoolParty
  class MonitorDaemon < Monitors::BaseMonitor
    
    attr_reader :should_daemonize, :pid_file, :sleep_time
    
    def self.run(o={})
      new(o).run
    end
    
    def initialize(o={})
      @should_daemonize = o.delete(:daemonize)
      @pid_file = o.delete(:daemonize) || "/tmp/poolparty_monitor.pid"
      @sleep_time = o.delete(:sleep_time) || 20
            
      super
    end
    
    def pass_the_baton
      # Handle stats
      my_nominations = JSON.parse(open("http://localhost:8642/stats/get_nominations").read)
      unless my_nominations.empty?
        running_nodes = my_cloud.nodes(:status => "running")
        nominations = []
        running_nodes.each do |node|
          timeout(10) do
            log "Checking with #{node.internal_ip} for nominations: #{open("http://#{node.internal_ip}:8642/stats/get_nominations").read}"
            nominations << begin
              JSON.parse(open("http://#{node.internal_ip}:8642/stats/nominations").read) || "none"
            rescue
              log "Error when connecting to #{node.internal_ip}: #{e.inspect}"
              "none"
            end            
          end
        end
        log "Sending #{nominations.flatten.to_json} to #{server["/elections"].inspect}"
        # put to "http://localhost:8642/elections/handle_election", data => nominations.to_json
        server["/elections"].put(nominations.flatten.to_json)
      end
    end
    
    def run      
      if should_daemonize
        @should_daemonize = false
        daemonize
      else
        log "Starting MonitorDaemon"
        loop {          
          begin
            pass_the_baton
          rescue Exception => e
            log "There was an error with pass_the_baton: #{e}"
          end
          sleep sleep_time
        }
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
    
    def server
      if @server
        @server
      else
        opts = { :content_type  =>'application/json', 
                 :accept        => 'application/json',
                 :host          => 'http://localhost',
                 :port          => '8642'
                }
        @uri = "#{opts.delete(:host)}:#{opts.delete(:port)}"
        @server = RestClient::Resource.new( @uri, opts)
      end
    end
    
  end
end