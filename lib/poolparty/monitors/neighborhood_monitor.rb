require ::File.dirname(__FILE__)+"/monitor_rack.rb"
require ::File.dirname(__FILE__)+"/../lite.rb"
require ::File.dirname(__FILE__)+"/../core/hash.rb"
require "#{::File.dirname(__FILE__)}/../poolparty/neighborhoods"

module Monitors
    
  class Neighborhood
    attr_reader :stats, :request
    attr_accessor :response
    
    def initialize(env, o={})
      @env = env
      @request = Rack::Request.new env
      @response = Rack::Response.new
      @neighboorhoods = ::PoolParty::Neighborhoods.load_default
    end
    
    def default
      @neighboorhoods.to_json
    end
    
  end
end