require "#{::File.dirname(__FILE__)}/../../test_helper"

module PoolParty
  module Plugin
    class Box < Plugin
    end
  end
end

class TestPlugins < Test::Unit::TestCase
  context "services" do
    
    should " have array of availble plugins" do
      assert Plugin.available.include?(PoolParty::Plugin::Box)
      assert Plugin.available.include?(PoolParty::Plugin::Git)
    end
    
  end
end