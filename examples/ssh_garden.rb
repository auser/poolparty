$:.unshift File.dirname(__FILE__)+'/../lib/'
require "poolparty"

pool :acres do
  
  cloud :garden do
    instances 2
    keypair 'id_rsa'
    using :ssh do
      user 'fairchild'  #default is root
      hosts %w(beet squash)
    end
    has_file '/etc/poolparty/welcome', :content=>"Welcome to the #{self.name} cloud"
    
  end
  
end