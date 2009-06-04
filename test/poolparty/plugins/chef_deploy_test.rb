require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestChefDeploy < Test::Unit::TestCase
  context "base usage (chef_deploy)" do
    setup do
      reset!
      @cloud = cloud :test_chef_deploy_cloud do
        has_chef_deploy do
          branch "/var/www"
          repo "git://github.com/auser/paparazzi.git"
          user "www-data"
        end
      end
      @chef_deploy = @cloud.chef_deploys.first
    end

    should "place a resource on the cloud" do
      assert_equal PoolParty::Plugin::ChefDeploy, @cloud.ordered_resources.first.class
    end
    should "raise if :repo is not passed" do
      assert_raise ReposMissingError do
        cloud :boxy do
          has_chef_deploy do
            branch "/bweee"
          end
        end
      end
    end
    should "have the rails required gems installed" do
      n = @cloud.chef_deploys.first.chef_librarys.map {|a| a.name }.first
      assert_match /chef\-deploy\/lib\/chef\-deploy\.rb/, n
    end
    should "have a chef_deploy_definition" do
      assert_equal @cloud.chef_deploys.first.chef_deploy_definitions.size, 1
    end
  end  
end