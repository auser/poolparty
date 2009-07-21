require "#{File.dirname(__FILE__)}/../../test_helper"

class ProvisionerBaseTest < Test::Unit::TestCase
  context "root" do

    setup do
      @base = Provision
    end
    
    should "have a compile method" do
      assert @base
    end
        
  end
  
end