require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources
PoolParty::Resource.define_resource_methods

class ApacheTest < Test::Unit::TestCase
  def setup
    clear!
    @pool = pool :apache_test_pool do
      cloud :httpd do
        apache do
          port 8080
        end
      end
    end
    @cloud = clouds[clouds.keys.first]
    @base = DependencyResolvers::Chef
  end
  
  def teardown
    FileUtils.rm_rf test_dir
  end
  
  def test_have_apache_in_the_resources
    assert !@cloud.apaches.empty?
    assert_equal 8080, @cloud.apaches.first.port
    assert_equal "apache2", @cloud.apaches.first.packages.first.name      
    assert_equal ["restart-apache2", "reload-apache2", "force-reload-apache2"], @cloud.apaches.first.execs.map {|a| a.name }[0..2]
  end
  
  def test_compile
    @base.compile_to(@cloud, test_dir)
  end
  
  def test_resource_graph
    # @cloud.resources_graph.write_to_graphic_file("png", "/tmp/graph")
    assert !@cloud.resources_graph.cyclic?
  end
  
end