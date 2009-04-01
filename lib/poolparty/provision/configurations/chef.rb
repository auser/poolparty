module PoolParty
  module Provision
    
    class Chef
      def self.commands
        [
          "/usr/bin/chef-solo -c /etc/chef/solo.rb -j /etc/chef/dna.json"
        ]
      end
      def self.files_to_upload
        [ 
        ]
      end
    end
    
  end
end