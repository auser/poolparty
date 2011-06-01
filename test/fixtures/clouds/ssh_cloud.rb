pool :tssh do
  cloud :tgarden do
    instances 2
    keypair File.join(FIXTURES_PATH, 'keys/test_key')
    using :ssh do
      user 'fairchild'  #default is root
      hosts %w(beet squash)
    end
    has_file '/etc/poolparty/welcome', :content=>"Welcome to the #{self.name} cloud" 
  end
end
