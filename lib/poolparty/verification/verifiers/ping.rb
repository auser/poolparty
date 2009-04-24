module PoolParty
  module Verifiers
    
    class Ping < VerifierBase
      include PoolParty::Pinger
      
      attr_reader :port
      def initialize(port=80)
        @port = port
      end
      def passing?
        ping_port(host, port)
      end
    end
    
  end
end