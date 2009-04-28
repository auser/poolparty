=begin rdoc
  MonitorRack is a rack application that maps url requests to method calls on Monitor classes.
=end

require ::File.dirname(__FILE__)+"/../aska/aska"
require ::File.dirname(__FILE__)+"/../lite"
require ::File.dirname(__FILE__)+"/base_monitor"

require 'rubygems'
require 'rack'
require 'json'

# We add an after hook to Rack::Response so that we can initiate a connection after 
# The response is sent back to client.
# PoolParty uses this to update a value, and then pass it on to another node.
class Rack::Response
  %w(close).each do |event|
    module_eval "def before_#{event}_callbacks;@before_#{event}_callbacks ||= [];end"
    module_eval "def after_#{event}_callbacks;@after_#{event}_callbacks ||= [];end"
  end
  
  def close
    before_close_callbacks.flatten.each {|a| a.call }
    body.close if body.respond_to?(:close)
    after_close_callbacks.flatten.each {|a| a.call }
  end
  
end

Dir[::File.dirname(__FILE__)+"/monitors/*"].each {|m| require m}
require_user_directory "monitors"

module Monitors

  class MonitorRack
    
    
    def call(env)
      @env = env
      @data = env['rack.input'].read rescue nil
      @request = Rack::Request.new env
      @response = Rack::Response.new
      @instance = nil
      begin
        path_array= path_map(env['REQUEST_PATH']) || []
        verb = env['REQUEST_METHOD'].downcase
        @response.write map_to_method(path_array, verb).to_json
        
        if instance.respond_to? :before_close_callbacks
          @response.before_close_callbacks << instance.before_close_callbacks
        end
        if instance.respond_to?(:after_close_callbacks)
          @response.after_close_callbacks << instance.after_close_callbacks
        end
      # rescue Exception=>e
      #   @response.write e
      #   @response.status = 500
      end
      @response.finish # this is [response.status, response.headers, response.body]
    end
    
    private
    def instance
      @instance ||= constantize( path_map.first ).new(env)
    end
    
    def env
      @env
    end
    def response
      @response
    end
    def request
      @request
    end
    
    # Split the request path into an array
    def path_map(requested_path=env['REQUEST_PATH'])
      requested_path.split('.')[0].split('/')[1..-1]
    end
  
    # Find class and call method from the pattern /class_name/method/args
    # GET /neighborhood => ::Monitors::Neighboorhood.get
    # POST /neighborhood => ::Monitors::Neighboorhood.post(params)
    # GET /neighborhood/size => ::Monitors::Neighboorhood.get_size
    def map_to_method(path, verb='get')
      if !path or path.empty? or path[0].nil?
        response.write 'cannot map an empty path' 
        response.status='404'
      else
        raise "#{path[0]} did not map to a Constant" if !instance
        case path.size
        when 0 # usefull if you want to subclass from MonitorRack
          self.respond_to?(verb.to_sym) ? self.send(verb.to_sym) : response.status='404'
        when 1 # example: /stats
          instance.send(verb.to_sym, @data)
        when 2 # example: /stats/load
          instance.send("#{verb}_#{path[1]}".to_sym, @data)
        else # example: /stats/load/average/5/minutes
          instance.send("#{verb}_#{path[1]}".to_sym, env['rack.input'].read, *path[2..-1])
        end
      end
    end
    
    # Take a string and return a ruby object if a match is found in the base_objects namespace.
    def constantize(name, base_object=Monitors)
      begin
        const = base_object.constants.find{|cnst| cnst == camelcase(name)}
        base_object.module_eval const
      rescue Exception => e
        puts "#{name.camelcase} is not defined. #{e}"
        nil
      end
    end
    
    def camelcase(str)
      str.gsub(/(^|_|-)(.)/) { $2.upcase }
    end
  
  end
  

end