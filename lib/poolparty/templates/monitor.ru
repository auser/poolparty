#!/usr/bin/env ruby
require 'rubygems'
require 'rack'
require 'json'
require 'thin'
require 'rest_client'
$:.unshift("#{::File.dirname(__FILE__)}/../../")
require "poolparty/monitors/monitor_rack" 

app = Rack::Builder.new do
  # use Rack::Reloader, 2
  use Rack::ShowExceptions
  use Rack::CommonLogger
  # use Rack::PostBodyContentTypeParser  #parses json requests to params hash
  run Monitors::MonitorRack.new()
  
  # Dumb daemon for now
  begin
    PoolParty::MonitorDaemon.run  :daemonize => true, 
                                  :sleep_time => 10, 
                                  :log_file_path => "monitor_daemon.log" #"/var/log/poolparty/monitor_daemon.log"

  rescue Exception => e
    puts "Error with daemon: #{e.inspect}"
  end
  
end

run app
