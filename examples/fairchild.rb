# Basic pool spec
# Shows global settings for the clouds
pool :application do
  instances 1..3
  ami 'ami-7cfd1a15'  
  
  cloud :example_one do
    
    has_directory "/var/www"
    
    has_file "/etc/motd", 
      :content => "Welcome to LARubyConf"
    has_file :name => "/var/www/index.html" do
      content "<h1>Welcome to your new poolparty instance</h1>"
      mode 0644
      owner "www-data"
    end
    
    has_git_repo "paparazzi" do
      source "git://github.com/auser/paparazzi.git"
      at "/var/www"
    end
    
    apache
    
  end

end