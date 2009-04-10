require ::File.dirname(__FILE__)+"/monitor_rack.rb"
require ::File.dirname(__FILE__)+"/../lite.rb"
require ::File.dirname(__FILE__)+"/../core/hash.rb"

module Monitors
    
  class Neighborhood
    attr_reader :stats, :request
    attr_accessor :response
    
    def initialize(env, o={})
      @env = env
      @request = Rack::Request.new env
      @response = Rack::Response.new
      @cloud = JSON.parse( open( "/etc/poolparty/clouds.json" ).read )
      @opts = @cloud["options"]
      @remoter_base = PoolParty::Remote.const_get(@opts.remote_base.split("::")[-1].camelcase)
    end
    
    def default
      @remoter_base.send :describe_instances, @opts
    end
    
  end
end