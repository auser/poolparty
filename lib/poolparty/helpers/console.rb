require File.dirname(__FILE__) + "/display"

module PoolParty
  module Console
    
    include Display
    
    # Print help commands for the console
    # level 0 - Basic help
    # level 1 - Commands
    def help(level=0)
      @np = NicePrinter.new(60)
      @np.header
      @np.center "PoolParty console help"      
      @np.center "Basics"
      @np << "Load your pool with load_pool(filename)"
      @np << "Reference clouds with"
      @np << "c = cloud :cloudname"
      @np.empty
      if level >= 1
        @np.center "CloudSpeak"
        @np << "All the commands set on your cloud can be called within the console"
        @np << "instances_by_status('running') - get list of running nodes"
        @np << "list_of_pending_instances - get list of pending nodes"
        @np << "available_monitors - get list of the available monitors on the cloud"
        @np << "  note: all monitors can be called as a method on the cloud"
        @np << "    i.e. CpuMonitor makes the method cpu available on the cloud"
        @np.empty
      end
      @np.footer
      @np.print
    end
    
    # Clear all the pools and reload the console
    # Call within console to reset and reload the entire poolparty base
    # as well
    def reload!      
      reset!
      require File.dirname(__FILE__) + "/../../../poolparty"
      require File.dirname(__FILE__) + "/../../../poolpartycl"
    end
    
  end
end

class Object
  include PoolParty::Console
end