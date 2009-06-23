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
    
    def self.inherited(subclass)
      unless Monitors.available.include?(subclass)
        Monitors.available << subclass
      end
    end
    
    def initialize(env=nil)
      @env=env
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
    
    def  my_cloud
      begin
        require '/etc/poolparty/clouds.rb'
        name = open("/etc/poolparty/cloud_name").read
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
    
  end
end