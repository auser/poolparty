pool "local" do
  
  cloud "local_app" do
    keypair "id_rsa"
    using :vmware do
      image_id "/Users/alerner/Documents/vm/Ubuntu32bitVM.vmwarevm/Ubuntu32bitVM.vmx"
      public_ip "192.168.248.133"
    end
    
    has_file "/etc/motd", :content => "BURN! Pocket Aces"
    
    apache
    
  end
  
end