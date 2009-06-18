require "#{File.dirname(__FILE__)}/../../test_helper"
require 'installer'

class VmwareInstallerTest < Test::Unit::TestCase
  context "vmware installer" do
    setup do
      @installer = PoolParty::Installers::Vmware.new
      
    end
    should "have the meta-data setup" do
      assert_equal "Vmware", @installer.name
      assert_equal "Vmware Fusion installer", @installer.description
    end
  end
  
end