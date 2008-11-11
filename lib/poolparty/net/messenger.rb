=begin rdoc
  The connection to the messenger from poolparty, the client
=end
module PoolParty
  def reconfigure_cloud!
    Messenger.messenger_send!("reconfigure_cloud")
  end
  module Messenger
    def with_socket(testing=false, &block)
      host = testing ? "localhost" : (master.ip)
      socket = TCPSocket.open(host, 7050)
      out = yield(socket)
      socket.close
      out
    end
    # TODO: Fix cookie setting
    def self.erl_command(hostname, extra="", min_ports=7000, max_ports=7050)
      command_line_opts = "-pa #{append_dir}/ebin -kernel inet_dist_listen_min #{min_ports} inet_dist_listen_max #{max_ports} -sname #{hostname} -setcookie poolparty"
      
      "erl #{command_line_opts} #{extra}"
    end
    
    def self.append_dir
      ::File.expand_path(::File.join( ::File.dirname(__FILE__), "..", "..", "erlang/messenger" ))
    end
    
    def messenger_send!(msg="get_current_load cpu")
      vputs "Calling #{msg} on the client server"
      with_socket do |sock|
        sock.send(msg, 0)
        @str = sock.recv(100)
      end
    end
    
    def self.messenger_send!(cmd="", testing=false)      
      command = Messenger.erl_command("client#{Time.now.to_i}", "-s pm_client -run pm_client #{cmd} -s erlang halt -noshell")
      testing ? command : %x[#{command}]
    end
    
    # Helper methods
    def self.startup_remote_messenger(hostname, testing=false)
      # messenger_send!("")
      # testing ? command : %x[#{command}]
    end
    
  end
end

module PoolParty
  module Cloud
    class Cloud
      include PoolParty::Messenger
    end
  end
end