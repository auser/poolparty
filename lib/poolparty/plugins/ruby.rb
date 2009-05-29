module PoolParty
  module Plugin
    class Ruby < Plugin
      
      def enable
        install_base_packages
      end
      
      def install_base_packages
        has_package(:name => "libreadline-ruby1.8")
        has_package(:name => "libruby1.8")
        has_package(:name => "ruby1.8-dev")
        has_package(:name => "ruby1.8")
        
        has_line_in_file(:line => "export PATH=$PATH:/var/lib/gems/1.8/bin", :file => "/etc/profile")
        
        # exec(:name => "update-rubygems") do
        #   command "gem update --system"
        #   onlyif "gem -v | grep 1."
        #   notify 'Exec["fix-updated-rubygems"]'
        # end
        # exec(:name => "fix-updated-rubygems") do
        #   command "awk \'{print} NR == 9 {print \"require \"rubygems/gem_runner\"\"}\' /usr/bin/gem"
        #   ifnot "awk \"/gem_runner/\" /usr/bin/gem"
        # end
      end
      
      def enable_ri
        has_package(:name => "ri1.8")
      end
      
      def enable_irb
        has_package(:name => "irb1.8")
      end
      
      def enable_rdoc
        has_package(:name => "rdoc1.8")
      end
      
    end  
  end
end