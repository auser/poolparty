=begin rdoc
  MonitorRack is a rack application that maps url requests to method calls on Monitor classes.
=end

require ::File.dirname(__FILE__)+"/../aska/aska.rb"
require ::File.dirname(__FILE__)+"/../lite.rb"

require 'rubygems'
require 'rack'
require 'json'

Dir[::File.dirname(__FILE__)+"/monitors/*"].each {|m| puts "require #{m}"; require "#{m}"}

class String
  def camelcase
    gsub(/(^|_|-)(.)/) { $2.upcase }
  end
end

module Monitors

  class MonitorRack
    
    def call(env)
      @env = env
      @request = Rack::Request.new env
      @response = Rack::Response.new
      begin
        path_array= path_map(env['REQUEST_PATH']) || []
        verb = env['REQUEST_METHOD'].downcase
        # puts "-- trying to map #{path_array.inspect}"
        @response.write map_to_class_method(path_array, verb).to_json
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
    
    # Split the request path into an array
    def path_map(requested_path)
      requested_path.split('.')[0].split('/')[1..-1]
    end
  
    # Find class and call method from the pattern /class_name/method/args
    # GET /neighborhood => ::Monitors::Neighboorhood.get
    # POST /neighborhood => ::Monitors::Neighboorhood.post(params)
    # GET /neighborhood/size => ::Monitors::Neighboorhood.get_size
    def map_to_class_method(path, verb='get')
      if !path or path.empty? or path[0].nil?
        response.write 'cannot map an empty path' 
        response.status='404'
      else
        klass = constantize(path[0])
        raise "#{path[0]} did not map to a Constant" if !klass
        case path.size
        when 0
          self.respond_to?(verb.to_sym) ? self.send(verb.to_sym) : response.status='404'
        when 1 #/stats
          klass.send(verb.to_sym) rescue klass.new(env).send(verb.to_sym)
        when 2 #/stats/load
          klass.send("#{verb}_#{path[1]}".to_sym) #rescue klass.new(env).send("#{verb}_#{path[1]}".to_sym)
        else
          klass.send("#{verb}_#{path[1]}".to_sym, *path[2..-1])
        end
      end
    end
    
    # Take a string and return a ruby object if a match is found in the base_objects namespace.
    def constantize(name, base_object=Monitors)
      begin
        const = base_object.constants.find{|cnst| cnst == name.camelcase}
        base_object.module_eval const
      rescue Exception => e
        puts "#{name.camelcase} is not defined. #{e}"
        nil
      end
    end
  
  end

end