require "#{::File.dirname(__FILE__)}/../../test_helper"

module PoolParty
  module Plugins
    class Box < Plugin
    end
  end
end

class TestPlugins < Test::Unit::TestCase
  context "services" do
    
    should " have array of availble plugins" do
      assert PoolParty::Plugin.available.include?(:box)
      assert !PoolParty::Plugin.available.include?(:git)
    end
    
  end
end