require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/test_plugins/webserver'

describe "Plugin" do

  before(:each) do
    @p = pool :poolpartyrb do
      cloud :app do
        apache do                
          enable_php
          site("heady", {
            :document_root => "/root"
          })
        end
      end
    end
    @c = @p.cloud(:app)
  end
  
  it "should allow access to the pool on the cloud" do
    @p.cloud(:app).parent.should == @p
  end
  describe "instance" do
    before(:each) do
      @plugin = "apache".class_constant.new(@c)
    end
    it "should not be empty" do
      @plugin.class.should == ApacheClas
    end
    it "should have access to the pool's container" do
      @plugin.container.should == @p.container
    end
    it "should not have any packages before eval'ing" do
      @plugin.container.packages.should be_empty
    end
    describe "after eval'ing" do
      before(:each) do
        @plugin.instance_eval do
          enable_php
          
          set do
            gem "aska"
          end
        end
      end
      it "should call enable_php on the class" do
        @plugin.php.should == true
      end
      it "the output should collect on the pool's container" do
        @p.cloud(:app).parent.container.lines.should_not be_empty
      end
      it "should yield the set block in the plugin" do
        @plugin.container.packages.should_not be_empty
      end
    end
    describe "before eval'ing" do
      it "should call has_line_in_file" do
        @plugin.should_receive(:has_line_in_file).at_least(1).and_return true
      end
      after do
        @plugin.instance_eval do
          enable_php
        end
      end
    end
  end
end