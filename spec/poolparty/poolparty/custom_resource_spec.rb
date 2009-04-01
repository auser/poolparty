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
            call_custom_function "line(#{file}, #{line})"
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
            call_custom_function "line(#{file}, #{line})"
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
      it "should add the method has_a_line_in_file to Resources" do
        PoolParty::Resources.methods.include?("has_a_line_in_file").should == true
      end
    end
    describe "printing" do
      before do
        
        define_resource(:rockstar) do
          def has_a_line_in_file(line="line_in_file", file="file")
            call_custom_function "line(#{file}, #{line})"
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
        @resource.resource(:call_function).first.to_string.should == "line(filename, hi)"
      end
      it "should be stored in an array" do
        @resource.resource(:rockstar).class.should == Array
      end
    end
  end
end