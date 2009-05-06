require File.dirname(__FILE__) + '/../spec_helper'

describe "File" do
  describe "instances" do
    before(:each) do
      @tc = TestBaseClass.new do
        has_file({:name => "/etc/apache2/puppetmaster.conf", :owner => "herman"}) do
          mode 755
        end
      end
      @file = @tc.resource(:file).first
    end
    it "have the name in the options" do
      @file.name.should == "/etc/apache2/puppetmaster.conf"
    end
    it "should store the owner's name as well" do
      @file.owner.should == "herman"
    end
    it "should store the mode (from within the block)" do
      @file.mode.should == 755
    end
    describe "template" do
      before(:each) do
        ::File.stub!(:basename).and_return "template"
        @file = "<%= friends %> <%= mode %>"
        @file.stub!(:read).and_return @file
        Template.stub!(:open).and_return @file
        
        @tc = TestBaseClass.new do
          has_file({:name => "/etc/apache2/puppetmaster.conf", :owner => "herman"}) do
            template "/absolute/path/to/template"
            render_as :erb
            variable :mode, "is super fast"
            variable :friends, "bob"
          end
        end
        @file = @tc.resource(:file).first
      end
      it "should have content in the options" do
        @file.content.nil?.should == false
      end
      it "should fill in the template (Erb) with the variables" do
        @file.content.should == "bob is super fast"
      end
      it "should remove the template from the options" do
        @file.template?.should == false
      end
    end
    describe "into PuppetResolver" do
      before(:each) do
        @compiled = PuppetResolver.new(@tc.to_properties_hash).compile
      end
      it "should set the filename to the name of the file" do
        @compiled.should match(/file \{ "\/etc\/apache2\/puppetmaster\.conf"/)
      end
      it "set the owner as the owner" do
        @compiled.should match(/owner => "herman"/)
      end
      it "have the mode set in the puppet output" do
        @compiled.should match(/mode => 755/)
      end
    end
  end
end
