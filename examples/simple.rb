$:.unshift("#{File.dirname(__FILE__)}/../lib")
require "rubygems"
require "poolparty"

pool "junk" do
  
  cloud "app" do
    
    security_group do
      %w(22 80 443 8642).each {|port|  authorize :from_port => port, :to_port => port}
    end
    
    instances 1..3
    
    using :ec2 do
      image_id "ami-ccf615a5" #alestic jaunty
      availability_zones ['us-east-1c']
    end
    
  end
  
end