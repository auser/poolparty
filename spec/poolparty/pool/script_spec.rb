require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Resources

describe "Script" do
  before(:each) do
    reset_resources!
  end
  describe "wrapped" do
    it "should have inflate as a class method" do
      Script.respond_to?(:inflate).should == true
    end
    it "should have inflate_file as an instance method" do
      Script.respond_to?(:inflate_file).should == true
    end
    it "should have inflate as an instance method" do
      Script.new.respond_to?(:inflate).should == true
    end

    describe "with a script" do
      before(:each) do
        @script = 'script'
        @filename = 'filename'

        @pool = Script.new
        Script.stub!(:new).and_return(@pool)
        @pool.stub!(:inflate).and_return true
      end

      it "should create a new Script when calling on the class method" do
        Script.should_receive(:new).and_return @pool      
      end    
      it "should instance eval the script" do
        @pool.should_receive(:instance_eval).with(@script, @filename).and_return true
      end
      it "should call inflate on itself" do
        @pool.should_receive(:inflate).and_return true
      end
      after do
        Script.inflate(@script, @filename)
      end
      describe "save!" do
        before(:each) do
          reset!
          reset_resources!
          pool :appdotcom do
            keypair "snoodle"
            ami "ami-123456"
            cloud :app do
              has_file :name => "/etc/httpd/httpd.conf"
            end
          end
          @saved = Script.save!(false)
        end
        it "should save the keypair" do
          @saved.should =~ /keypair "snoodle"/
        end
        it "should save the ami" do
          @saved.should =~ /ami "ami-123456"/
        end
      end
    end
  end
end