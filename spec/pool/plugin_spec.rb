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
  
  it "the output should collect on the clouds" do
    @p.cloud(:app).output.should_not be_empty
  end
  it "should add to the output" do
    @p.output.should_not be_empty
  end
end