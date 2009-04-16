module PoolParty
  module Provision
    
    class Chef
      def self.commands
        [
          "mkdir -p /etc/chef/cookbooks /etc/chef/cache",
          "cp -R /var/poolparty/dr_configure/chef/cookbooks/* /etc/chef/cookbooks",
          "cp /var/poolparty/dr_configure/chef/solo.rb /etc/chef/solo.rb",
          "cp /var/poolparty/dr_configure/chef/dna.json /etc/chef/dna.json",
          "/usr/bin/chef-solo -c /etc/chef/solo.rb -j /etc/chef/dna.json"
        ]
      end
      def self.files_to_upload
        [ 
          "#{Default.tmp_path}/dr_configure/chef/solo.rb",
          "#{Default.tmp_path}/dr_configure/chef/dna.json"
        ]
      end
    end
    
  end
end