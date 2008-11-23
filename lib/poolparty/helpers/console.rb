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
      if level.zero?
        @np.center "Basics"
        @np << "Load your pool with load_pool(filename)"
        @np << "Reference clouds with"
        @np << "c = cloud :cloudname"
        @np.empty
      end
      @np.center "CloudSpeak"
      @np << "You can check the number of instances with"
      @np << "list_of_running_nodes"
      @np.empty
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