require "poolparty"
require 'poolparty-extensions'

pool "poolparty" do
  instances 1
  
  cloud "vmware" do
    keypair "id_rsa"
    using :vmware do
      image_id  "/Users/nmurray/Documents/VMware/Ubuntu-jaunty.vmwarevm/Ubuntu-jaunty.vmx"
      public_ip "192.168.133.128"
    end
    has_file "/etc/motd", :content => "welcome to your instance"

    has_convenience_helpers
  end

end

# hermes
# want to be able to run cloud configure to update it
# but want to be able to pull from locally to configure it
# submodules?  
# node name
# install path
# how to upload the escript target system
# how to upload the tar.gz of the release
