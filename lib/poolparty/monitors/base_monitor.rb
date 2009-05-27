=begin rdoc
  BaseMonitor
  
  BaseMonitor adds a basic base monitor with callbacks for the 
  monitors
=end
module Monitors
  
  @available_monitors =[]
  def self.available_monitors
    @available_monitors
  end
  
  class BaseMonitor
    
    def self.inherited(subclass)
      (Monitors.available_monitors << subclass) unless Monitors.available_monitors.include?(subclass)
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
    
  end
end