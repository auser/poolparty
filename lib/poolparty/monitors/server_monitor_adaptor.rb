module Butterfly
  class ServerMonitorAdaptor < AdaptorBase
    attr_reader :data
    def initialize(o={})
      super      
    end
    def get(req, resp)
      begin
        if req.params.empty?
          default_data
        else
          data[req.params[0]] ||= ServerMonitor.send(req.params[0])
        end        
      rescue Exception => e
        resp.fail!
        "Error: #{e}"
      end 
    end
    
    private
    def default_data
      %w(load).each do |var|
        data["#{var}"] ||= ServerMonitor.send(var.to_sym)
      end
      data
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

# fork do
#   Butterfly::Server.new(:clouds_json_file => "#{::File.dirname(__FILE__)}/../../../spec/poolparty/fixtures/clouds.json").start!
# end
# fork do
#   Butterfly::Server.new(:port => 8082, :clouds_json_file => "#{::File.dirname(__FILE__)}/../../../spec/poolparty/fixtures/clouds.json").start!
# end