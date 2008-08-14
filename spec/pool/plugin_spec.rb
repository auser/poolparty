require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/test_plugins/webserver'

describe "Plugin" do

  before(:each) do  
    @p = pool :poolpartyrb do
      cloud :app do
        apache do
          enable_php
          virtual_host("heady", {
            :document_root => "/root"
          })
        end
      end
    end
  end
  
  it "should add has_file to the output" do
    false
  end
  
end