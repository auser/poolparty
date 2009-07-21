require 'rubygems'
$:.unshift File.dirname(__FILE__)+'/../lib/'
require "poolparty"

pool :eucalyptus do
    
  cloud :sample do
    instances 2
    keypair "eucalyptus-sample"
    using :ec2 do
      access_key ENV['EC2_ACCESS_KEY']
      secret_access_key ENV['EC2_SECRET_KEY']
      image_id 'emi-39921602'
    end
  end
  
end