require "timeout"
module PoolParty
  module Pinger
    module ClassMethods
      # Test to see if the host has the available port open
      # for response. 
      def ping_port(host, port=22, retry_times=400)
        connected = false
        dputs "pinging #{host}:#{port} for #{retry_times}"
        retry_times.times do |i|
          begin
            timeout 5 do
              break if connected = TCPSocket.new(host, port).is_a?(TCPSocket)
            end            
          rescue Exception => e
            sleep(2)
          end
        end
        connected
      end
    end
  
    module InstanceMethods
      def ping_port(ip, port=22, retry_times=500);self.class.ping_port(ip, port, retry_times);end
      def ping_port_and(ip, port=22, retry_times=500, &block)        
        block.call if ping_port(ip, port, retry_times) && block
      end
    end
  
    def self.included(receiver)
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods
    end
  end
end