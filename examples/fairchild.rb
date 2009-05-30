require 'rubygems'
$:.unshift "#{File.dirname(__FILE__)}/../lib"
require "poolparty"

# Basic pool spec
# Shows global settings for the clouds
pool :application do
  instances 1..3
  image_id 'ami-7cfd1a15'  
  
  cloud :la do
    using :ec2
    image_id 'ami-7cfd1a15'
    keypair 'brighthouse_front'
    has_directory "/var/www"
    
    has_file "/etc/motd", 
      :content => "Welcome to LARubyConf"
    has_file :name => "/var/www/index.html" do
      content "<h1>Welcome to your new poolparty instance</h1>"
      mode 0644
      owner "www-data"
    end
    
    # has_git "paparazzi" do
    #   source "git://github.com/auser/paparazzi.git" rescue require 'ruby-debug'; debugger
    #   dir "/var/www"
    # end
    
    apache
    
  end

end