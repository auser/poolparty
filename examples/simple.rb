$:.unshift("#{File.dirname(__FILE__)}/../lib")
require "rubygems"
require "poolparty"

pool "poolparty" do
  
  cloud "simple" do
    using :ec2
    instances 1
    image_id "ami-ccf615a5" #alestic jaunty
    availability_zones ['us-east-1c']
    
    security_group(:name => 'start_test') do
      authorize :from_port => '22', :to_port => '22'
      authorize :from_port => '80', :to_port => '80'
    end
    
  end
  
end