require File.dirname(__FILE__) + '/../spec_helper'

describe "String" do
  describe "hasherize" do
    before(:each) do
      @str=<<-EOE
hello world
you are my
hero
      EOE
      @hashed = @str.hasherize(%w(hello you))
    end
    it "should return a hash from the string" do
      @hashed.class.should == Hash
    end
    it "should have hello as a key" do
      @hashed.has_key?(:hello).should == true
    end
    describe "arrayable" do
      it "should be able to turn the string into an array" do
        @str.arrayable.class.should == Array
      end
    end    
  end
  before(:each) do
    @string = "string"
    @string.stub!(:bucket_objects).and_return([])
  end
  # Dumb test
  it "should be able to call bucket_objects on itself" do
    @string.should_receive(:bucket_objects)
    @string.bucket_objects
  end
  describe "with config replacements" do
    it "should replace those syms in the string" do
      ("new :port" ^ {:port => 100}).should == "new 100"
    end
    it "should be able to detect vars" do
      @string=<<-EOC
listen web_proxy 127.0.0.1::client_port
\tserver web1 127.0.0.1::port weight 1 minconn 3 maxconn 6 check inter 30000
      EOC
      (@string ^ {:client_port => 3000, :port => 3001}).should ==<<-EOO
listen web_proxy 127.0.0.1:3000
\tserver web1 127.0.0.1:3001 weight 1 minconn 3 maxconn 6 check inter 30000
      EOO
    end
  end
  describe "collect_each_line_with_index" do
    before(:each) do
      @longer_string = "hot\npotato\nthrough\nthe\nwindow"
    end
    it "should run the same code on the entire string" do
      @longer_string.collect_each_line_with_index do |str, index|
        "#{index}_#{str}"
      end.should == ["0_hot", "1_potato", "2_through", "3_the", "4_window"]
    end
  end
  describe "String" do
    before(:each) do
      @str =<<-EOS
        echo 'hi'
        puts 'hi'
      EOS
    end
    it "should be able to convert a big string with \n to a runnable string" do
      @str.runnable.should == "echo 'hi' &&         puts 'hi'"
    end
  end
  describe "Constantize" do
    before(:each) do
      @str = "prok"
    end
    it "should be able to turn itself into constant" do
      @str.class_constant.should == PoolPartyProkClass
    end
    it "should turn itself into a class constant" do
      @str.class_constant.class.should == Class
    end
    it "should not recreate the constant if it exists" do
      Class.should_receive(:new).once.and_return Class.new
      @str.class_constant
      @str.class_constant
    end
    it "should set the parent class when sent with the superclass" do
      "stirer".class_constant(String).ancestors[1].should == String
    end
    it "should be able to create a module into a constant" do
      @str.module_constant.should == ProkModule
    end
    it "should turn itself into a class constant" do
      @str.module_constant.class.should == Module
    end
    it "should create the module on preserved module constant" do
      Module.should_receive(:new).once
      @str.preserved_module_constant.should == Prok
      @str.preserved_module_constant
      @str.preserved_module_constant
    end
    describe "with a block" do
      before(:each) do
        @str = "nack"
      end
      it "should be able to yield a block on the module and set methods" do
        "tippy".module_constant do
          def tippy
            puts "etc"
          end
        end
        Class.new.extend("tippy".module_constant).methods.include?("tippy").should == true
      end
    end
  end
  describe "top level class" do
    it "should be able to get the top level class" do
      "PoolParty::Resources::File".top_level_class.should == "file"
    end
  end
  describe "to_option_string" do
    it "should not touch a string, but return the string with single quotes" do
      "rocks".to_option_string.should == "'rocks'"
    end
    it "should return a string of the format Service[nagios] with no single quotes" do
      "Service[nagios]".to_option_string.should == "Service[nagios]"
    end
    it "should also return a string if there is a string within the string" do
      'Package["heartbeat-2"]'.to_option_string.should == 'Package["heartbeat-2"]'
    end
    it "should return File['/etc/apache2/conf.d/base.conf'] as a string" do
      'File["/etc/apache2/conf.d/base.conf"]'.to_option_string.should == "File[\"/etc/apache2/conf.d/base.conf\"]"
    end
  end
  describe "sanitize" do
    it "should remove the periods from the string" do
      "xnot.org".sanitize.should == "xnotorg"
    end
  end
  describe "nice_runnable" do
    before(:each) do
      @tasks = ["ls -l", "echo 'hello'"]
    end
    it "should turn an array into a string" do
        @tasks.nice_runnable.class.should == String
    end
    it "should add \\n between the commands" do
      @tasks.nice_runnable.should == "ls -l \n echo 'hello'"
    end
    it "should strip out excess lines" do
      (@tasks << []).nice_runnable.should == "ls -l \n echo 'hello'"
    end
  end
end