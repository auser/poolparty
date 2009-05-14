require "#{::File.dirname(__FILE__)}/../../test_helper"

class PluginClass
  plugin :box do
  end
end

class TestPlugins < Test::Unit::TestCase
  context "services" do
    setup do
      reset!
      @tbc = TestBaseClass.new do
        box do
        end
      end
    end
  end
end