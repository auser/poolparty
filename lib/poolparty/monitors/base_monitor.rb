module Monitors
  class BaseMonitor
    
    def initialize(env=nil)
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