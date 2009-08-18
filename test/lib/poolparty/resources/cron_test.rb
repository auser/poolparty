require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class CronResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Cron.new "run root scripts", :command => "/bin/sh /usr/bin/root_scripts", :hour => [2,8,14], :weekday => [2,5]
      @base = DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the cron method denoted by has_method_name" do
      str = "cron \"run root scripts\" do
  command \"/bin/sh /usr/bin/root_scripts\"
  action :create
  minute \"*\"
  hour [ 2, 8, 14 ]
  day \"*\"
  month \"*\"
  weekday [ 2, 5 ]
  user \"root\"
end\n"

      assert_equal str, @base.compile(@res)
    end
    
  end
  
end