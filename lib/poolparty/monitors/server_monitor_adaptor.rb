# require "rubygems"
# require "butterfly"
# require "poolparty/poolparty/default"
# require 'net/http'
# Dir["#{::File.dirname(__FILE__)}/butterfly_adaptors/*"].each {|lib| require lib }

module Butterfly
  class AdaptorBase
    def request_from_local(uri)
      res = Net::HTTP.start("127.0.0.1", 8082) {|http| http.get(uri) }
      res.body
    end
  end
  class ServerMonitorAdaptor < AdaptorBase
    attr_reader :data
    def initialize(o={})
      super      
    end
    def get(req, resp)
      begin
        if req.params.empty?
          data["load"] ||= ServerMonitor.send(:load)
        else
          data[req.params[0]] ||= ServerMonitor.send(req.params[0])
        end        
      rescue Exception => e
        resp.fail!
        "Error: #{e}"
      end 
    end
    
    private
    
    def data
      @data ||= reload_data!
    end
    def reload_data!
      super
      @data = {}
    end
  end
end

# fork do
#   Butterfly::Server.new(:clouds_json_file => "#{::File.dirname(__FILE__)}/../../../spec/poolparty/fixtures/clouds.json").start!
# end
# fork do
#   Butterfly::Server.new(:port => 8082, :clouds_json_file => "#{::File.dirname(__FILE__)}/../../../spec/poolparty/fixtures/clouds.json").start!
# end