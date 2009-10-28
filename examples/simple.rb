$:.unshift("#{File.dirname(__FILE__)}/../lib")
require "rubygems"
require "poolparty"

pool "poolparty" do
  
  cloud "simple" do
    using :ec2
    instances 1
    image_id "ami-ccf615a5" #alestic jaunty
    availability_zones ['us-east-1c']
  end
  
end