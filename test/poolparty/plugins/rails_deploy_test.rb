require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestRailsDeploy < Test::Unit::TestCase
  context "base usage (rails_deploy)" do
    setup do
      reset!
      @cloud = cloud :test_rails_deploy_cloud do
        has_rails_deploy do
          dir "/var/www"
          repo "git://github.com/auser/paparazzi.git"
          user "www-data"
        end
      end
      @rails_deploy = @cloud.rails_deploys.first
    end

    should "place a resource on the cloud" do
      @cloud.ordered_resources.first.class.should == RailsDeployClass
    end
    should "raise if :repo is not passed" do
      assert_raise ReposMissingError do
        cloud :boxy do
          has_rails_deploy do
            dir "/bweee"
          end
        end
      end
    end
    should "have the rails required gems installed" do
      arr = @cloud.rails_deploys.first.gem_packages.map {|a| a.name }
      %w(rails actionmailer actionpack activerecord activesupport activeresource).each do |g|
        assert arr.include?(g)
      end
    end
    should "create the directory tree" do
      arr = @cloud.rails_deploys.first.directorys.map {|a| a.name }
      ["/shared", "/shared/config", "/shared/pids", "/shared/log", "/shared/system"].each do |dir|
        assert arr.include?("/var/www/paparazzi#{dir}")
      end      
    end
    should "have git-core defined as a package" do
      arr = @rails_deploy.packages.map {|a| a.name }
      assert arr.include?("git-core")
    end
    should "have the chef-deploy" do
      arr = @rails_deploy.chef_deploys.map {|a| a.name }
      assert arr.include?("/var/www/paparazzi")
    end
  end  
end