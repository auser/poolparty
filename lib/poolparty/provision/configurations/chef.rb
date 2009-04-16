module PoolParty
  module Provision
    
    class Chef
      def self.commands
        [
          "mkdir -p /etc/chef/cookbooks /etc/chef/cache",
          "cp -R /var/poolparty/dr_configure/chef/recipes/* /etc/chef/cookbooks",
          "cp /var/poolparty/dr_configure/solo.rb /etc/chef/solo.rb",
          "/usr/bin/chef-solo -c /etc/chef/solo.rb -j /etc/chef/dna.json"
        ]
      end
      def self.files_to_upload
        [ 
          "#{Default.tmp_path}/solo.rb",
          "#{Default.tmp_path}/dna.json"
        ]
      end
    end
    
  end
end