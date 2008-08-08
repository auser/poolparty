require File.dirname(__FILE__) + '/../spec_helper'

class WebServers
  register_plugin :apache do
    puts "hi"
    def enable_php
      @php = true
    end
  end
end

describe "Plugin" do
  before(:each) do
    
    @p = pool :poolpartyrb do
      cloud :app do
        apache do
          enable_php
        end
      end
    end
        
  end
  describe "methods should include" do
    it "register_plugin(plugin)" do;WebServers.respond_to?(:register_plugin).should == true;end
  end
  describe "registered" do
    before(:each) do
      @plugin = @p.plugin(:apache)
    end
    it "should store the regsitered plugins in an array" do
      @plugin.should_not be_nil
    end
    it "should have the plugin name as a method on the cloud " do
      Cloud.respond_to?(:apache).should == true
    end
    it "should call the method when in the defininition of the cloud" do
      puts @plugin.methods.sort.grep(/php/)
      @plugin.respond_to?(:enable_php).should == true
    end
  end
end