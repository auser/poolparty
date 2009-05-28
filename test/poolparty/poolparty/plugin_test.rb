require "#{::File.dirname(__FILE__)}/../../test_helper"

class PluginClass
  plugin :box do
  end
end

class TestPlugins < Test::Unit::TestCase
  context "services" do
    # setup do
    #   reset!
    #   @tbc = TestBaseClass.new do
    #     box do
    #     end
    #   end
    # end
    
    should " have array of availble plugins" do
      assert Plugin.available.include?(Kernel::BoxClass)
      assert Plugin.available.include?(Kernel::GitClass)
    end
    
  end
end