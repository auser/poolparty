# Basic pool spec
# Shows global settings for the clouds
pool :application do
  instances 1..3
  # keypair "#{ENV["HOME"]}/.ssh/r_and_d.pem"
  keypair "auser-work"
  verbose true
  ami 'ami-7cfd1a15'  
  # testing false
  
  cloud :pp1 do
    # junk_yard_dogs "pains"
    has_file :name => "/etc/motd", :content => "Welcome to your PoolParty instance"
    has_package :name => 'nmap'
            
    chef do
      include_recipes "~/.poolparty/chef/37s_signals/*"
      
      has_git_repos :name => "/srv/paparazzi" do
        source "git://github.com/auser/paparazzi.git"
        at "/srv"
      end
      
      has_exec :name => "setup var for passenger" do
        command "chmod 755 -R /srv && chown -R www-data:www-data /srv"
      end
            
      recipe "#{::File.dirname(__FILE__)}/examples/default.rb", 
              :templates => [
                              "#{::File.dirname(__FILE__)}/examples/paparazzi.conf.erb"
                            ]
      
      json do
        gems [
          { "name" => "rake",
            "version" => "0.8.3"
          },
          { "name" => "tmm1-amqp",
            "source" => "http://gems.github.com",
            "version" => "0.6.0"
          }]
      end
    end
    
    # apache do
    #     installed_as_worker
    #     has_virtualhost do
    #       name "poolpartyrb.com"
    #       listen("8080")
    #       # virtual_host_entry ::File.join(File.dirname(__FILE__), "virtual_host.conf.erb")
    #       
    #       # We are going to have a repository that is updated across the servers
    #       has_git(:name => "poolpartyrepos", 
    #               :source => "git://github.com/auser/poolparty-website.git", 
    #               :at => "/var/www/poolpartyrb.com") 
    #     end
    #   end
  end

end