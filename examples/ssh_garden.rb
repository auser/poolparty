$:.unshift File.dirname(__FILE__)+'/../lib/'
require "poolparty"

pool :acres do
  
  cloud :garden do
    os :centos
    instances 1 #this should equal the number of hosts listed below
    keypair 'id_rsa'  # this keypair must be in /root/.ssh/authorized_keys on each host
    has_file '/etc/motd', :content=>"Welcome to the #{self.name} cloud"
    has_file '/touhced'
    has_directory '/mnt/ebs', :owner => 'poolparty'
    has_exec 'touch /tmp/touched'
    
    using :ssh do
      hosts %w(67.23.21.192)  #the hosts to use in this cloud
    end
    
  end
  
end