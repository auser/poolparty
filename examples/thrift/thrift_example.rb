#!/usr/bin/env ruby

$:.unshift(File.dirname(__FILE__) + "/../../lib")
$:.unshift(File.dirname(__FILE__) + "/../../examples")
require "poolparty"
require "simple"

$:.unshift("#{File.dirname(__FILE__)}/../../lib/proto/gen-rb")

require "thrift"
require "command_interface"
require "poolparty_constants"
require "poolparty_types"

port = ARGV.pop || 11223

transport = Thrift::BufferedTransport.new(Thrift::Socket.new('localhost', port))
protocol = Thrift::BinaryProtocol.new(transport)

client = CloudThrift::CommandInterface::Client.new(protocol)
transport.open()

cld = CloudThrift::CloudQuery.new
cld.name = 'monitored_app'

resp = client.run_command(cld, "name", [])
puts resp.response

resp = client.run_command(cld, "maximum_instances", [])
puts resp.response

resp = client.run_command(cld, "run_monitor", ["cpu", "0.91"])
p resp.response

resp = client.run_command(cld, "run_monitor", ["cpu", "0.01"])
p resp.response