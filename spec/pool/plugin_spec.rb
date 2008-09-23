require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/test_plugins/webserver'

describe "Plugin" do

  before(:each) do
    reset!
    pool :poolpartyrb do      
      cloud :app do
        apache do
          enable_php
          site("heady", {
            :document_root => "/root"
          })
        end
      end      
    end
    @p = pool :poolpartyrb
    @c = @p.cloud(:app)
  end
  
  it "should allow access to the pool on the cloud" do
    @c.parent.should == @p
  end
  describe "instance" do
    before(:each) do
      @plugin = "apache".class_constant.new(@c)
    end
    it "should not be empty" do
      @plugin.class.should == ApacheClas
    end
    it "should have access to the cloud's container" do
      @plugin.container.should == @p.cloud(:app).container
    end
    it "should have enable_php as a method" do
      @plugin.respond_to?(:enable_php).should == true
    end
    describe "after eval'ing" do
      before(:each) do
        @plugin.instance_eval do
          enable_php
          has_gem :name => "aska"
        end
      end
      it "should call enable_php on the class" do
        @plugin.php.should == true
      end
    end
    describe "before eval'ing" do
      before(:each) do
        reset!
        @plugin = "apache".class_constant.new(@c)
      end
      it "should call has_line_in_file" do
        @plugin.should_receive(:php).and_return true
      end
      it "should call site" do
        @plugin.should_receive(:site).with("frank").and_return true
      end
      after do
        @plugin.instance_eval do
          enable_php
          site("frank")
        end
      end
      
    end
  end
end