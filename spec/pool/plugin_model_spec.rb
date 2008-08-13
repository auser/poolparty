require File.dirname(__FILE__) + '/../spec_helper'

class WebServers
  register_plugin :apache do
    def enable_php
      @php = true
    end
    def virtual_host(name=:domain1, &block)
    end
    def document_root(*args)
      args.empty? ? @document_root : @document_root = args
    end
  end
end

describe "Plugin" do
  before(:each) do
    
    @p = pool :poolpartyrb do
      cloud :app do
        apache do
          enable_php
          virtual_host do
            document_root "/root"
          end
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
      PoolParty::Cloud::Cloud.respond_to?(:apache).should == true
    end
    describe "methods" do
      it "should call the enable_php method when in the defininition of the cloud" do
        @plugin.respond_to?(:enable_php).should == true
      end
      it "should call the virtual_host method when in the defininition of the cloud" do
        @plugin.respond_to?(:virtual_host).should == true
      end
      it "should be able to call the plugin method virtual_host" do
        @plugin.document_root.should == "/root"
      end
    end
  end
end