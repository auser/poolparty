pool :boxed do
  
  cloud :app do
    has_file "/etc/junk", :content => "junk"
  end
  
  after_all_loaded do
    clouds[:app].has_file "/etc/junkie", :content => "2"
  end
  
end