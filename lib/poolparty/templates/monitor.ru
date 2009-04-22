#!/usr/bin/env ruby
require 'rubygems'
require 'rack'
require 'json'
require 'thin'
require "poolparty/monitors/monitor_rack"

app = Rack::Builder.new do
  use Rack::Reloader, 2
  use Rack::ShowExceptions
  use Rack::Lint
  # use Rack::PostBodyContentTypeParser  #parses json requests to params hash
  run Monitors::MonitorRack.new()
end

run app
