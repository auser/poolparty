
pool "poolparty" do
  
  cloud "chef" do
    instances 1
    using :ec2
    chef_repo File.dirname(__FILE__)+"/chef_cloud/chef_repo"
    recipe "apache2"
    recipe "rsyslog::server"
    recipe "collectd"
    chef_attributes :apache2 => {:listen_ports => ["80", "8080"]}
    user_data open(File.dirname(__FILE__)+"/chef_cloud/user_data").read
    security_group do
      authorize :from_port => "22", :to_port => "22"
      authorize :from_port => "80", :to_port => "80"
    end
    
  end
end
