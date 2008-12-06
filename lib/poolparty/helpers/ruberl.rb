require "socket"
module PoolParty
  module Ruberl
    class Base
      attr_accessor :host, :port
      def initialize(host="localhost", port=7050)
        @host = host
        @port = port
      end
      def with_socket(&block)
        begin
          socket = TCPSocket.open(@host, @port)
          out = yield(socket)
          socket.close
          out
        rescue Exception => e
        end
      end
      def messenger_send!(msg="get_current_load cpu")
        with_socket do |sock|
          sock.send(msg, 0)
          @str = sock.recv(2000)      
        end
        @str
      end
      def messenger_cast!(msg="force_reconfig")
        with_socket do |sock|
          sock.send(msg, 0)
        end
      end
    end
  end
end