require File.dirname(__FILE__) + '/spec_helper'

pool :app do
  
  plugin_directory File.join(File.dirname(__FILE__), "pool", "test_plugins")
  instances 2..10
  
  cloud :app do
    # minimum 2 instances
    # maximum 10 instances
    apache do
      enable_php
    end    
  end
  
  cloud :db do
    # minimum 2 instances
    # maximum 10 instances
    mysql # default mysql setup    
  end
  
end