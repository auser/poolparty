require ::File.dirname(__FILE__) + '/../spec_helper'

describe "exec" do
  describe "instances" do
    before(:each) do
      @tc = TestBaseClass.new do
        has_exec("list /var/www directory") do
          command "/usr/bin/ls -l /var/www"
        end
      end
      @exec = @tc.resource(:exec).first
    end
    it "have the name in the options" do
      @exec.name.should == "list /var/www directory"
    end
    it "should store the owner's name as well" do
      @exec.command.should == "/usr/bin/ls -l /var/www"
    end
    it "should have a path" do
      @exec.path.should == ["/usr/bin:/bin:/usr/local/bin:$PATH"]
    end
    describe "into PuppetResolver" do
      before(:each) do
        @compiled = PuppetResolver.new(@tc.to_properties_hash).compile
      end
      it "should set the execname to the name of the exec" do
        @compiled.should match(/exec \{ "list \/var\/www directory"/)
      end
      it "have the mode set in the puppet output" do
        @compiled.should match(/command => "\/usr\/bin\/ls -l \/var\/www"/)
      end
      it "have the path set in the puppet output" do
        @compiled.should match(/path => \[ "\/usr\/bin:\/bin:\/usr\/local\/bin:\$PATH" \]/)
      end
    end
  end
end
