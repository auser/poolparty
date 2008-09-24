require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Resources

describe "Custom Resource" do
  it "should provide custom_resource as a method" do
    self.respond_to?(:define_resource).should == true
  end
  describe "defining" do
    it "should not raise ResourceException if custom_function and custom_usage are defined" do
      lambda {
        define_resource(:rockstar) do          
          def has_line_in_file(line="line_in_file", file="file")
            call_function "line(#{file}, #{line})"
          end
          custom_function <<-EOF
          define line($file, $line, $ensure = 'present') {
            case...
          }
          EOF
        end
      }.should_not raise_error
    end
    describe "define_resource" do
      before(:each) do
        define_resource(:rockstar) do
          def has_line_in_file(line="line_in_file", file="file")
            call_function "line(#{file}, #{line})"
          end
          custom_function <<-EOF
          define line($file, $line, $ensure = 'present') {
            case...
          }
          EOF
        end
      end
      it "should create a custom resource available as a class" do
        lambda {PoolParty::Resources::Rockstar.new }.should_not raise_error
      end
      it "should add the method has_line_in_file to Resources" do
        PoolParty::Resources.methods.include?("has_line_in_file").should == true
      end      
    end
    describe "printing" do
      before do
        reset_resources!
        define_resource(:rockstar) do
          def has_line_in_file(line="line_in_file", file="file")
            call_function "line(#{file}, #{line})"
          end
          custom_function <<-EOF
          define line($file, $line, $ensure = 'present') {
            case...
          }
          EOF
        end
        @resource = resource(:rockstar)
      end
      it "should not be nil after it is defined" do
        @resource.should_not be_nil
      end
      it "should store the custom_function in the class" do
        PoolParty::Resources::Rockstar.custom_functions.select {|a| a if a =~ /define line/}.should_not be_empty
      end
      it "should allow for the has_line_in_file to be called from within a plugin" do        
        @resource.should_receive(:has_line_in_file)
        @resource.instance_eval do
          has_line_in_file("hi", "filename")
        end        
      end
      it "should be stored in an array" do
        @resource.class.should == Array
      end
      describe "call function" do
        it "should have the class CallFunction available" do
          lambda {PoolParty::Resources::CallFunction}.should_not raise_error
        end
        it "should create a new CallFunction instance when calling call_function with a string" do
          PoolParty::Resources::CallFunction.should_receive(:new).and_return "bunk"
          add_resource(:call_function, "line")
        end
        it "should create a call function in the function call array" do
          add_resource(:call_function, "heyyohey")
          resource(:call_function).size.should == 1
        end
        it "should call call_function in the context of the custom resource" do
          PoolParty::Resources::CallFunction.should_receive(:new).and_return "bunk"
          has_line_in_file("hi", "filename").should == "bunk"
        end
        it "should store the call in the call_function resource" do
          has_line_in_file("hi", "filename")
          resource(:call_function).size.should == 1
        end
      end

    end
  end
end