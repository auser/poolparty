$:.unshift("#{File.dirname(__FILE__)}/../lib")
require "rubygems"
require "poolparty"

pool "poolparty" do
  
  cloud "simple" do
    using :ec2
    image_id "ami-ccf615a5" #alestic jaunty
    availability_zones ['us-east-1c']
    security_group do
      %w(22 80 443 8642).each {|port|  authorize :from_port => port, :to_port => port}
    end
    autoscale
  end
  
end