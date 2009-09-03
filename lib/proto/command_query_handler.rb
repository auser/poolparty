class CommandQueryHandler
  def self.run_query(on_cloud, command, args, opts={})
    port = opts[:port] || 11223
    host = opts[:host] || "localhost"

    transport = Thrift::BufferedTransport.new(Thrift::Socket.new(host, port))
    protocol = Thrift::BinaryProtocol.new(transport)

    client = CloudThrift::CommandInterface::Client.new(protocol)
    transport.open()

    cld = CloudThrift::CloudQuery.new
    cld.name = on_cloud.name
    
    ddputs("Running command: #{command} on #{cld.name} at #{host}:#{port}")
    resp = client.run_command(cld, command, args)
    resp.response
  end
end