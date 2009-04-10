require ::File.dirname(__FILE__)+"/monitor_rack.rb"

module Monitors
    
  class Neighborhood
    attr_reader :stats, :request
    attr_accessor :response
    
    def initialize(env, o={})
      @env = env
      @request = Rack::Request.new env
      @response = Rack::Response.new
      puts "Started with #{o.inspect}"
      @cloud = JSON.parse( open( "/etc/poolparty/clouds.json" ).read ) 
      @remoter_base = Kernel.const_get(@cloud.options.remote_base.camelcase)
    end
    
    def default
      instances = @remoter_base.send :describe_instances, @cloud.options
      instances.inspect
    end
    
  end
end