Dir["#{::File.dirname(__FILE__)}/../monitors/*"].each {|lib| require lib }

module Butterfly
  class StatsAdaptor < AdaptorBase
    attr_reader :data
    def initialize(o={})
      super
    end
    def get(req, resp)
      begin      
        { "load" => load,
          "nominations" => nominations }
      rescue Exception => e
        resp.fail!
        "Error: #{e}"
      end
    end
    
    private
    
    def load
      # http request to localhost/server_monitor/m
      # request_from_local("/server_monitor/load")
      endpoint = "http://localhost:#{PoolParty::Default.monitor_port}/server_monitor/load"
      request_from_local("/server_monitor/load") 
    end
    def nominations
      # http request to localhost/vote
      # request_from_local("/server_monitor/vote")
      # %x{ "curl http://localhost:#{PoolParty::Default.monitor_port}/server_monitor/vote" }
      request_from_local("/vote")
    end
    # HTTP request to localhost/neighborhood/ips
    def get_neighborhood
    end

    def data
      @data ||= reload_data!
    end
    def reload_data!
      super 
      @data = {}
    end
  end
end