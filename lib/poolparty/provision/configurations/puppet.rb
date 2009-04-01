module PoolParty
  module Provision
    
    class Puppet
      def self.commands
        [
          "mkdir -p /etc/puppet/manifests/classes",
          "mkdir -p /var/puppet",
          "cp /var/poolparty/dr_configure/site.pp  /etc/puppet/manifests/site.pp",
          "ruby /var/poolparty/dr_configure/add_puppet_to_hosts",
          "cp /var/poolparty/dr_configure/puppet.conf /etc/puppet/",
          "cp /var/poolparty/dr_configure/site.pp /etc/puppet/manifests/",
          "cp /var/poolparty/dr_configure/poolparty.pp /etc/puppet/manifests/classes",
          "cp /var/poolparty/dr_configure/puppetrunner /usr/bin/",
          "/usr/bin/puppetrunner"
        ]
      end
      def self.files_to_upload
        [ "#{::File.dirname(__FILE__)}/../../templates/puppet/add_puppet_to_hosts",
          "#{::File.dirname(__FILE__)}/../../templates/puppet/puppet.conf",
          "#{::File.dirname(__FILE__)}/../../templates/puppet/puppetrunner",
          "#{::File.dirname(__FILE__)}/../../templates/puppet/site.pp" 
        ]
      end
    end
    
  end
end