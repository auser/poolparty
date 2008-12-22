require 'eventmachine'

module PoolParty
  
  module AgentServer
    attr_reader :cloud
    def initialize(cloud)
      @cloud = cloud
    end
    def receive_data(data)
      begin        
        meth, args = data.split(" ")[0], data.split(" ")[1..-1]
        puts "meth: #{meth}"
        if meth
          msg = "#{meth} #{args.join(" ") if args && args.length > 0}"
          open("log/agent.log", "a+") {|f| f << msg }
          send_data msg if @cloud.debugging
          out = @cloud.send meth.to_sym, *args
          send_data "#{out}"          
        end
        close_connection if data =~ /quit|exit/i
      rescue Exception => e
        send_data "#{e}"
      end
    end
  end
  
  class Agent
    include Daemonizeable
    attr_reader :cloud
    
    def initialize(cld)
      @cloud = cld
    end
    
    def verbose;@cloud.verbose;end
    def debug;@cloud.debugging;end
    def kill
      self.class.kill pid_file
    end
    # Run the agent
    def run
      unless running?
        kill if cloud.kill
        
        if debug
          EventMachine::run {EventMachine::start_server "127.0.0.1", Base.agent_port, AgentServer, @cloud}
        end
        daemonize do
          EventMachine::run {EventMachine::start_server "127.0.0.1", Base.agent_port, AgentServer, @cloud}
        end
      end
    end
    def self.run_for(cld)
      new(cld).run
    end
  end
  
end