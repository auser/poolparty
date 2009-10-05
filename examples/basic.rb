$:.unshift("#{File.dirname(__FILE__)}/../lib")
require "rubygems"
require "poolparty"

pool "skinnytest2" do
  
  cloud "app" do
    load_balancer "mapA", :external_port => 8000 do
      internal_port '81'
    end
    load_balancer "mapB", :external_port => 443 do
      internal_port 8443
    end
    autoscale "a"
    using :ec2 do
      security_group "test_cloud" do
        revoke :from_port => "8080", :to_port => "8081"
        authorize :from_port => "22", :to_port => "22"
      end
      minimum_instances 1
      maximum_instances 1
    end
  end
  
end
