require File.dirname(__FILE__) + '/../spec_helper'

describe "Template" do
  describe "of string" do
    before(:each) do
      @template_string = "Hello <%= name %>"
    end
    it "should have a method compile on Template class" do
      Template.respond_to?(:compile_string).should == true
    end
    it "should return rendered content" do
      Template.compile_string(@template_string, {:name => "bob"}).should == "Hello bob"
    end
  end
  describe "of file" do
    before(:each) do
      @template_file = ::File.dirname(__FILE__) + "/../fixtures/test_template.erb"
    end
    it "have the method compile_file on the Template class" do
      Template.respond_to?(:compile_file).should == true
    end
    it "return the rendered content into a string" do
      Template.compile_file(@template_file, {:friends => "bob"}).should == "Hello bob"
    end
  end
end