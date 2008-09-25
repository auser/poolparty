require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Resources

describe "Custom Resource" do
  before(:each) do
    @cloud = cloud :name do; end
  end
  it "should provide custom_resource as a method" do
    self.respond_to?(:define_resource).should == true
  end
  describe "defining" do
    it "should not raise ResourceException if custom_function and custom_usage are defined" do
      lambda {
        define_resource(:rockstar) do
          def has_a_line_in_file(line="line_in_file", file="file")
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
          def has_a_line_in_file(line="line_in_file", file="file")
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
        lambda {RockstarClass.new }.should_not raise_error
      end
      it "should add the method has_a_line_in_file to Resources" # do
      #         PoolParty::Resources.methods.include?(:has_a_line_in_file).should == true
      #       end
    end
    describe "printing" do
      before do
        reset_resources!
        define_resource(:rockstar) do
          def has_a_line_in_file(line="line_in_file", file="file")
            call_function "line(#{file}, #{line})"
          end
          custom_function <<-EOF
          define line($file, $line, $ensure = 'present') {
            case...
          }
          EOF
        end
        @resource = RockstarClass.new
      end
      it "should not be nil after it is defined" do
        @resource.should_not be_nil
      end
      it "should store the custom_function in the class" do
        RockstarClass.custom_functions.select {|a| a if a =~ /define line/}.should_not be_empty
      end
      it "should allow for the has_a_line_in_file to be called from within a plugin" do        
        @resource.instance_eval do
          has_a_line_in_file("hi", "filename")
        end
        @resource.resource(:call_function).to_string.should == "line(filename, hi)"
      end
      it "should be stored in an array" do
        resource(:rockstar).class.should == Array
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
        it "should add the method to the CustomMethods module with add_methods_from" do
          PoolParty::Resources::CustomMethods.should_receive(:add_methods_from).once
          define_resource(:imarockstar) do
          end
        end
        it "should add the methods to the class through module_eval" do
          PoolParty::Resources::CustomMethods.should_receive(:module_eval).once
          define_resource(:imarockstar2) do
          end
        end
        it "should store the new methods in the added_methods hash" do
          define_resource(:imarockstar3) do
            def i_do_nothing              
            end
          end
          PoolParty::Resources::CustomMethods.added_methods[:imarockstar3].should == ["i_do_nothing"]
        end
        describe "calling" do
          before(:each) do
            reset_resources!
            define_resource(:youarealsoarockstar_dude) do
              def silly_method_that_does_silly_things
                call_function "silly_custom_method"
              end
            end
          end
          it "should include methods on the custom_resource in the CustomMethods module" do
            PoolParty::Resources::CustomMethods.available_methods.include?("silly_method_that_does_silly_things").should == true
          end
          it "should try to call it with method_missing" do
            @cloud.should_receive(:method_missing).with(:silly_method_that_does_silly_things).and_return true
            @cloud.silly_method_that_does_silly_things
          end
          it "should try to find the method in available_methods" do
            PoolParty::Resources::CustomMethods.available_methods.should_receive(:include?)
            @cloud.silly_method_that_does_silly_things
          end
          it "should run call_methods on CustomMethods when running a method" do            
            PoolParty::Resources::CustomMethods.should_receive(:call_method)
            @cloud.silly_method_that_does_silly_things
          end
          it "should call call_function in the context of the custom resource" do
            PoolParty::Resources::CallFunction.should_receive(:new)
            @cloud.silly_method_that_does_silly_things
          end
          it "should store the call in the call_function resource" do
            @cloud.has_a_line_in_file_dude("hi", "filename")
            resource(:call_function).size.should == 1
          end
          
        end
      end
      
      describe "within context" do
        before(:each) do
          @cloud = cloud :app do
            has_a_line_in_file("hello", "messages")
          end
        end
        it "should have 1 resource (the line resource)" do
          @cloud.resources.should_not be_empty
        end
        it "should have one call_function resource" do
          @cloud.resource(:call_function).first.to_string.should == "line(messages, hello)"
        end
      end
    end
  end
end