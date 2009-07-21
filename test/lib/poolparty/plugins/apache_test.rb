require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources
PoolParty::Resource.define_resource_methods

class ApacheTest < Test::Unit::TestCase
  context "calling apache" do
    setup do
      clear!
      @pool = pool :apache_test_pool do
        cloud :httpd do
          apache do
            port 8080
          end
        end
      end
      @cloud = clouds[clouds.keys.first]
    end

    should "have apache in the resources" do
      assert !@cloud.apaches.empty?
      assert_equal 8080, @cloud.apaches.first.port
      assert_equal "apache2", @cloud.apaches.first.packages.first.name
    end
  end
  
end