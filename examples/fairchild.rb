# Basic pool spec
# Shows global settings for the clouds
pool :application do
  instances 1..3
  keypair "auser-work"
  verbose true
  ami 'ami-7cfd1a15'  
  
  cloud :pp1 do
    has_file :name => "/etc/motd", 
      :content => "Welcome to your PoolParty instance: <%= @node[:fqdn] %>", :mode => 644

    has_package :name => 'nmap'
    
    has_git_repos :name => "paparazzi" do
      source "git://github.com/auser/paparazzi.git"
      at "/var/www"
    end

    chef do
      include_recipes "~/.poolparty/chef/cookbooks/*"
            
      recipe "#{::File.dirname(__FILE__)}/examples/fairchild_chef.rb", 
              :templates => [
                              "#{::File.dirname(__FILE__)}/examples/paparazzi.conf.erb"
                            ]
    end
    
  end

end