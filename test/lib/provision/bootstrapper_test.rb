require "#{File.dirname(__FILE__)}/../../test_helper"

class BootstrapperTest < Test::Unit::TestCase
  
  context "bootstrap_script" do
    should "get the script for ubuntu" do
      bootstrap_dir = File.dirname(__FILE__)/"../../../lib/provision/bootstrap_scripts"
      ubuntu_script = File.expand_path(bootstrap_dir/"build_ubuntu.sh")
      assert_equal ubuntu_script, Provision::Bootstrapper.bootstrap_script(:os=>:ubuntu)
    end
    
    should "raise an exception if the os isn't supported yet" do
      assert_raises StandardError do
        Provision::Bootstrapper.bootstrap_script(:os=>:non_existant_os)
      end
    end
  end
  
  context "configure_script" do
    setup do
      clear!
      @filepath = fixtures_dir/"clouds/simple_cloud.rb"
      @pool = PoolParty::Pool.load_from_file(@filepath)
      @cloud = @pool.clouds[@pool.clouds.keys.first]
      @outfile = test_dir/"configure_script.sh"
    end
    
    should "get the script for ubuntu" do
      assert_equal File.expand_path(@outfile), Provision::Bootstrapper.configure_script(@cloud, :ubuntu, @outfile)
    end
    
    should "output some stuffies" do
      assert_match /echo app > \/etc\/poolparty\/cloud_name/, open(@outfile).read
      assert_match /echo poolparty > \/etc\/poolparty\/pool_name/, open(@outfile).read
    end
    
    should "raise an exception if the os isn't supported yet" do
      assert_raises StandardError do
        Provision::Bootstrapper.configure_script(@cloud, :non_existant_os)
      end
    end
  end
  
end