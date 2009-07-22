require 'rubygems'
$:.unshift File.dirname(__FILE__)+'/../lib/'
require "poolparty"

pool :eucalyptus do
    
  cloud :sample do
    instances 2
    keypair "eucalyptus_sample"
    using :ec2 do
      image_id 'emi-39921602'
    end
  end
  
end