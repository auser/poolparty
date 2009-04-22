#!/usr/bin/env ruby
require 'rubygems'
require 'rack'
# require 'rack/contrib'
require 'json'
require 'thin'
require ::File.join(::File.dirname(__FILE__),'..','lib/poolparty/monitors/', 'monitor_rack.rb') 

class Baton
  def initialize(app)
    @app = app
  end
  def call(env)
    self.class.call(env)
  end
  def self.call(env)
    puts "received: #{env.to_yaml}\n------\n"
    response = Rack::Response.new
    response.write "requested: #{env["REQUEST_PATH"]}"
    response.finish
  end
end


app = Rack::Builder.new do
  use Rack::Reloader, 2
  use Rack::ShowExceptions
  use Rack::Lint
  use Baton
  # use Rack::PostBodyContentTypeParser  #parses json requests to params hash
  run Monitors::MonitorRack.new()
  run_after
end

if __FILE__ == $0
# at_exit do
  Rack::Handler::Thin.run app, :daemonize=>true, :pid_file=>'/tmp/monitor.pid'
  # Thin::Server.new(app, :daemonize=>true, :pid_file=>'/tmp/monitor.pid').daemonize
# end
else
  run app
end
