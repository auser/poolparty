module PoolParty
  module Verifiers
=begin
== Ping

Open a TCPSocket and verify you can connect.

== Example:

    verify do
      ping
      ping(22)
    end

=end
    class Ping < VerifierBase
      include ::PoolParty::Pinger
      
      attr_reader :port
      def initialize(port=80)
        @port = port
      end
      def passing?
        ping_port(host, port, 3)
      end

      def to_s
        "<#{self.class.to_s} host:#{host} port:#{port}>"
      end

    end
    
  end
end