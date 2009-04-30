require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestDrConfigure < Test::Unit::TestCase
  context "Rendering of a string" do
    setup do
      @template_string = "Hello <%= name %>"
    end
    should "should have a method called compile_string on the class" do assert Template.respond_to?(:compile_string);end
    should "should return rendered content" do assert Template.compile_string(@template_string, {:name => "bob"}), "Hello bob" end
  end
  context "Rendering of a file" do
    setup do
      @template_file = ::File.dirname(__FILE__) + "/../../fixtures/test_template.erb"
    end
    should "should have a method called compile_file on the class" do assert Template.respond_to?(:compile_file);end
    should "should return rendered content" do assert Template.compile_file(@template_file, {:friends => "bob"}), "Hello bob" end
  end
  context "render_as" do
    setup do
      @template_options = {:render_as => :erb}
    end

    should "should call render with as erb" do
      assert Template.compile_string("Hi <%= world %>", @template_options.merge(:world => "world")), "Hi world"
    end
    should "call render with haml if rendered with haml" do
      # assert Template.compile_string("Hi <%= world %>", @template_options.merge(:render_as => :haml, :world => "world")), nil
    end
  end
  
end