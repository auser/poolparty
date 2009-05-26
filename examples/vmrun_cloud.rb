require 'rubygems'
$:.unshift File.dirname(__FILE__)+'/../lib/'
require "poolparty"

pool :multiverse do
  
  cloud :vv do
    keypair "front"
    instances 2
    has_file "/etc/motd", :content => "Welcome to your poolparty instance!"
    using :vmrun do
      vmx_files [
        "/Users/mfairchild/Documents/Virtual\ Machines.localized/Ubuntu-jaunty.vmwarevm/Ubuntu-jaunty.vmx",
        "/Users/mfairchild/Documents/Virtual\ Machines.localized/metavirts/Ubuntu32bitVM copy.vmwarevm/Ubuntu32bitVM.vmx"
        ]
    end
  end
  
end

