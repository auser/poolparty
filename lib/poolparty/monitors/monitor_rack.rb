require ::File.dirname(__FILE__)+"/../aska/aska.rb"
require ::File.dirname(__FILE__)+"/../lite.rb"

require 'rubygems'
require 'rack'
require 'json'

class String
  def camelcase
    gsub(/(^|_|-)(.)/) { $2.upcase }
  end
end

module Monitors
  
  # def self.call(env)
  #   @request = Rack::Request.new(env)
  #   @response = Rack::Response.new
  #   begin
  #     response.write map_to_class_method(path_map(env)).to_json
  #   rescue Exception=>e
  #     @response.write e
  #     @response.status = 500
  #   end
  #   @response.finish # [response.status, response.headers, response.body]
  # end

  class MonitorRack
    
    def new(opts={})
      @opts=opts
    end
    
    def call(env)
      dup._call(env)
    end
    
    def _call(env)
      @env = env
      @request = Rack::Request.new env
      @response = Rack::Response.new
      begin
        path_array= path_map(env['REQUEST_PATH']) || []
        puts "-- trying to map #{path_array.inspect}"
        @response.write map_to_class_method(path_array).to_json
      # rescue Exception=>e
      #   @response.write e
      #   @response.status = 500
      end
      @response.finish # this is [response.status, response.headers, response.body]
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
    
    def path_map(requested_path)
      requested_path.split('.')[0].split('/')[1..-1]
    end
  
    # Find class and call method from the pattern /class_name/method/args
    def map_to_class_method(path)
      if !path or path.empty? or path[0].nil?
        response.write 'cannot map an empty path' 
        response.status='404'
      else
        klass = module_eval(path[0])
        raise "#{path[0]} did not map to a Constant" if !klass
        case path.size
        when 0
          self.respond_to?(:default) ? self.send(:default) : response.status='404'
        when 1
          puts "Calling: #{klass.new(env)}"
          klass.send(:default) rescue klass.new(env).send(:default)
        when 2
          klass.send(path[1].to_sym) rescue klass.new(env).send(path[1].to_sym)
        else
          klass.send(path[1].to_sym, *path[2..-1])
        end
      end
    end
    
    def constantize(name, base_object=Monitors)
      begin
        const = base_object.constants.find{|cnst| cnst == name.camelcase}
        base_object.module_eval const
      rescue Exception => e
        puts "#{name.camelcase} is not defined. #{e}"
        nil
      end
    end
  
    def default
      'default response'
    end
  
  end
  
  #load our monitors
  require ::File.dirname(__FILE__)+"/stats_monitor.rb"
  require ::File.dirname(__FILE__)+"/neighborhood_monitor.rb"
  

  # just here as an example
  # access it at /monitor_time/ or /monitor_time/now
  class MonitorBase
    def initialize(env={})
      @env = env
    end
  end
  class MonitorTime
    def self.default
      now
    end
    def self.now
      Time.now
    end
  end
  
  class Favicon
    def self.default
      ''
    end
  end
end