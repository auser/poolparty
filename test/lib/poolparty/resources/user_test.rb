require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class UserResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::User.new "random", :comment => "Random User", :uid => "1000", :gid => "users", :home => "/home/random", :shell => "/bin/zsh",  :password => "$1$JJsvHslV$szsCjVEroftprNn4JHtDi."
      @base = DependencyResolvers::Chef
      @base.compile_directory = test_dir
    end
    
    should "have the mount method denoted by has_method_name" do
str = <<-EOS
user "random" do
  action :create
  comment "Random User"
  uid \"1000\"
  gid "users"
  home "/home/random"
  shell "/bin/zsh"
  password "$1$JJsvHslV$szsCjVEroftprNn4JHtDi."
  supports :manage_home => false
end
EOS

      assert_equal str.chomp, @base.compile(@res)
    end
    
  end
  
end