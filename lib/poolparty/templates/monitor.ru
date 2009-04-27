#!/usr/bin/env ruby
require 'rubygems'
require 'rack'
require 'json'
require 'thin'
require 'rest_client'
begin
  require "poolparty/monitors/monitor_rack" 
rescue LoadError
  require ::File.join(::File.dirname(__FILE__),'..','monitors/', 'monitor_rack.rb') 
end

app = Rack::Builder.new do
  # use Rack::Reloader, 2
  use Rack::ShowExceptions
  # use Rack::PostBodyContentTypeParser  #parses json requests to params hash
  run Monitors::MonitorRack.new()
end

run app
