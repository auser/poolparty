require "#{File.dirname(__FILE__)}/../../test_helper"

class BootstrapperTest < Test::Unit::TestCase
  
  context "bootstrap_script" do
    should "get the script for ubuntu" do
      bootstrap_dir = File.dirname(__FILE__)/"../../../lib/provision/bootstrap_scripts"
      ubuntu_script = File.expand_path(bootstrap_dir/"build_ubuntu.sh")
      assert_equal ubuntu_script, Provision::Bootstrapper.bootstrap_script(:ubuntu)
    end
    
    should "raise an exception if the os isn't supported yet" do
      assert_raises StandardError do
        Provision::Bootstrapper.bootstrap_script(:non_existant_os)
      end
    end
  end
  
  context "configure_script" do
    should "get the script for ubuntu" do
      bootstrap_dir = File.dirname(__FILE__)/"../../../lib/provision/configure_scripts"
      ubuntu_script = File.expand_path(bootstrap_dir/"configure_ubuntu.sh")
      assert_equal ubuntu_script, Provision::Bootstrapper.configure_script(:ubuntu)
    end
    
    should "raise an exception if the os isn't supported yet" do
      assert_raises StandardError do
        Provision::Bootstrapper.configure_script(:non_existant_os)
      end
    end
  end
  
end