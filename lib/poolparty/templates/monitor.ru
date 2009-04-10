#!/usr/bin/env ruby
require 'rubygems'
require 'rack'
require 'rack/contrib'
require 'json'
require 'thin'
require ::File.join(::File.dirname(__FILE__),'..','lib/poolparty/monitors/', 'monitor_rack.rb') 


app = Rack::Builder.new do
  use Rack::Reloader, 2
  use Rack::ShowExceptions
  use Rack::Lint
  # use Rack::PostBodyContentTypeParser  #parses json requests to params hash
  run Monitors::MonitorRack.new()
end

run app
