$:.unshift("#{File.dirname(__FILE__)}/../lib")
require "rubygems"
require "poolparty"

pool "junk" do
  
  cloud "app" do
    
    security_group do
      authorize :from_port => '22', :to_port => '22'
      authorize :from_port => '8080', :to_port => '8080'
    end
    
    using :ec2 do
    end
    
  end
  
end