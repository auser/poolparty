require 'rubygems'
$:.unshift "#{File.dirname(__FILE__)}/../lib"
require "poolparty"

# Basic pool spec
# Shows global settings for the clouds
pool :party do
  instances 1..3
  image_id 'ami-7cfd1a15'
  debugging true
  
  # access_key "XXXXXX"
  # secret_access_key "XXXXXXX"
  
  cloud :app do
    keypair 'app'
    using :ec2 do
      image_id 'ami-7cfd1a15'
      availability_zone 'us-east-1c'
    end
    
    has_file "/etc/motd", :content => "Welcome to LARubyConf"
    # has_directory "/var/www"
    # has_file :name => "/var/www/index.html" do
    # content "<h1>Welcome to your new poolparty instance</h1>"
    # mode 0644
    # owner "www-data"
    # end
    
    # has_git "paparazzi" do
    #   source "git://github.com/auser/paparazzi.git"
    #   dir "/var/www"
    # end
    
  end

end