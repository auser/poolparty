=begin rdoc
  BaseMonitor
  
  BaseMonitor adds a basic base monitor with callbacks for the 
  monitors
=end
module Monitors
  class BaseMonitor
    
    @available_monitors =[]  
    def self.available_monitors
      @available_monitors     
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
    
    def before_close_callbacks
      @before_close_callbacks ||= []
    end
    def after_close_callbacks
      @after_close_callbacks ||= []
    end
    
    def self.inherited(subclass)
      (@available_monitors << subclass) unless @available_monitors.include?(subclass)
    end
    
  end
end