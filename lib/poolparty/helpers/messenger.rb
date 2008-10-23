module PoolParty
  module Messenger
    
    # TODO: Fix cookie setting
    def self.erl_command(hostname, extra="")
      command_line_opts = "-pa #{append_dir}/ebin -sname #{hostname} -setcookie poolparty"
      
      "erl #{command_line_opts} #{extra}"
    end
    
    def self.append_dir
      ::File.join( ::File.dirname(__FILE__), "..", "..", "erlang/messenger" )
    end
    
    def messenger_send!(cmd="", testing=false)
      cmd = Messenger.erl_command("client", "-rsh ssh -noshell -run pm_client #{cmd} -s erlang halt")
      testing ? cmd : Kernel.system(cmd)
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