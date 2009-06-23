require 'rubygems'
$:.unshift File.dirname(__FILE__)+'/../lib/'
require "poolparty"

pool :multiverse do
  
  cloud :vv do
    keypair "front"
    instances 1
    has_file "/etc/motd", :content => "Welcome to your poolparty instance!"
    using :vmrun do
      vmx_hash "/Users/stimble/Documents/Virtual Machines.localized/Jaunty.vmwarevm/Jaunty.vmx" => '172.16.68.143'
    end
  end
  
end

