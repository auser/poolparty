=begin rdoc
  BaseMonitor
  
  BaseMonitor adds a basic base monitor with callbacks for the 
  monitors
=end
module Monitors
  
  @available_monitors =[]
  def self.available
    @available_monitors
  end
  
  class BaseMonitor
    attr_reader :log_file, :log_file_path, :last_cloud_loaded_time
    
    def self.inherited(subclass)
      unless Monitors.available.include?(subclass)
        Monitors.available << subclass unless subclass.to_s =~ /MonitorDaemon/
      end
    end
    
    def initialize(env=nil)
      @env=env
      log_filename = "poolparty_monitor.log"
      @log_file_path = "/var/log/poolparty/#{log_filename}"
      
      unless ::File.file?(log_file_path)
        ::FileUtils.mkdir_p ::File.dirname(log_file_path) unless ::File.directory?(::File.dirname(log_file_path))
        ::File.open(log_file_path, 'a+') {|f| f << "------ #{Time.now} ------"}
        log_file_path
      end
    end
    
    def env(env=@env)
      @env=env
    end
    
    %w(close).each do |event|
      %w(before after).each do |time|
        module_eval <<-EOE
        def #{time}_#{event}(m=nil, &block)
          #{time}_#{event}_callbacks << block ? block : m.to_sym
        end
        EOE
      end
      
    end
    
    # Load the cloud
    # Reload the cloud if necessary after 60 seconds
    # of cache time
    def  my_cloud
      @my_cloud = nil if last_cloud_loaded_time && last_cloud_loaded_time > 60
      @my_cloud ||= begin
        require '/etc/poolparty/clouds.rb'
        name = open("/etc/poolparty/cloud_name").read
        last_cloud_loaded_time = Time.now.to_i
        clouds[name.chomp.to_sym]
      rescue Exception => e
        JSON.parse( open('/etc/poolparty/clouds.json' ).read )
      end
    end
    
    
    def before_close_callbacks
      @before_close_callbacks ||= []
    end
    def after_close_callbacks
      @after_close_callbacks ||= []
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
    
    
  end
end