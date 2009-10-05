$:.unshift("#{File.dirname(__FILE__)}/../lib")
require "rubygems"
require "poolparty"

pool "skinnytest2" do
  
  cloud "app" do
    load_balancer "mapA" do
      listener :external_port => 8081, :internal_port => 81
      listener :external_port => 8082, :internal_port => 82, :protocol => 'tcp'
    end
    load_balancer "mapB" do
      listener :internal_port => 8443, :external_port => 443 
    end
    autoscale
    using :ec2 do
      security_group "test_cloud" do
        revoke :from_port => "8080", :to_port => "8081"
        authorize :from_port => "22", :to_port => "22"
      end
      instances 0
    end
  end
end
