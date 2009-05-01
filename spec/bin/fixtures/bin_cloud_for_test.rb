cloud :binary_testing_cloud do
  keypair "bob"
  has_file :name => "/etc/motd", :content => "Welcome to the cloud"        
  has_file :name => "/etc/profile", :content => "profile info"
  has_directory :name => "/var/www"
  # has_package :name => "bash"        
  # parent == cloud
  apache do
    # parent == apache
    listen "8080"
    has_file :name => "/etc/apache2/apache2.conf"
  end
end