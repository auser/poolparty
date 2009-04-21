require ::File.dirname(__FILE__)+"/../../core/hash.rb"
require "#{::File.dirname(__FILE__)}/../../poolparty/neighborhoods"

module Monitors
  
  class Neighborhood
    attr_reader :stats, :request
    attr_accessor :response
    
    def initialize(env, o={})
      @env = env
      @request = Rack::Request.new env
      @response = Rack::Response.new
      @neighboorhood = ::PoolParty::Neighborhoods.load_default
    end
    
    def get
      @neighboorhood.to_json
    end
    
    def put(data)
      received = JSON.parse(data)
      @neighboorhood.merge!(received) #TODO: need sanity checks and need to remove old neighboors
      save
    end
    
    def post(data)
      @neighboorhood = JSON.parse(data)
      save
    end
    
    private
    def save(filepath='/etc/poolparty/neighborhood.json')
      ::File.open(filepath, "w") {|f| f << @neighboorhood.to_json }
      @neighboorhood
    end
    
  end
end