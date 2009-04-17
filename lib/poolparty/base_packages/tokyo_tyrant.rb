module PoolParty
  class Base
    plugin :tokyo_tyrant do
      
      def enable
        has_package "build-essential"
        has_package "zlib1g-dev"
        has_package "libbz2-dev"
        has_gem_package "rufus-tokyo"
        
        has_exec "install tokyo-cabinet" do
          command "cd ~ && git clone git://github.com/etrepum/tokyo-cabinet.git && cd tokyo-cabinet/ && ./configure && make && make install && cd ~"
          not_if "which tcrtest"
        end
        has_exec "install tokyo-tyrant" do
          command "cd ~ && git clone git://github.com/etrepum/tokyo-tyrant.git && cd tokyo-tyrant/ && ./configure && make && make install && cd ~"
          not_if "which ttserver"
        end
      end
      
    end
  end
end