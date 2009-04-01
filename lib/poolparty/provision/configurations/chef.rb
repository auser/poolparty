module PoolParty
  module Provision
    
    class Chef
      def self.commands
        [
          "mkdir -p /etc/chef/cookbooks /etc/chef/cache",
          "cp -R /var/poolparty/dr_configure/chef/recipes/* /etc/chef/cookbooks",
          "cp /var/poolparty/dr_configure/chef_config.rb /etc/chef/solo.rb",
          "cp /var/poolparty/dr_configure/dna.json /etc/chef/dna.json",
          "/usr/bin/chef-solo -c /etc/chef/solo.rb -j /etc/chef/dna.json"
        ]
      end
      def self.files_to_upload
        [ 
          "/tmp/poolparty/chef_config.rb",
          "/tmp/poolparty/dna.json"
        ]
      end
    end
    
  end
end