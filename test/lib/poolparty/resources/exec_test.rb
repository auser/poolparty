require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class ExecResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do      
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Exec.new "/usr/sbin/mail -s 'hi' arilerner@mac.com"
      @base = DependencyResolvers::Chef
      @base.compile_directory = test_dir
      
      @str = <<-EOE
execute "/usr/sbin/mail -s 'hi' arilerner@mac.com" do
  command "/usr/sbin/mail -s 'hi' arilerner@mac.com"
  path [ "/usr/bin:/bin:/usr/local/bin:$PATH" ]
  action :run
EOE
    end
    
    should "have the method denoted by has_method_name turn into the string without any modifications" do      
      assert_equal "#{@str}end".chomp, @res.compile(:chef)
    end
    
    should "have the creates method when it's set" do
      @res.creates = "/etc/poolparty/created"
      @str << '  creates "/etc/poolparty/created"'
      assert_equal "#{@str}\nend", @res.compile(:chef)
    end
    
    should "have the cwd method when it's set" do
      @res.cwd = "/var/poolparty"
      @str << '  cwd "/var/poolparty"'
      assert_equal "#{@str}\nend", @res.compile(:chef)
    end
    
    should "have the environment method when it's set" do
      @res.environment = :development
      @str << '  environment :development'
      assert_equal "#{@str}\nend", @res.compile(:chef)
    end
    
    should "have the group method when it's set" do
      @res.group = 501
      @str << '  group 501'
      assert_equal "#{@str}\nend", @res.compile(:chef)
    end
    
    should "have the returns method when it's set" do
      @res.returns = 1
      @str << '  returns 1'
      assert_equal "#{@str}\nend", @res.compile(:chef)
    end
    
    should "have the user method when it's set" do
      @res.user = 502
      @str << '  user 502'
      assert_equal "#{@str}\nend", @res.compile(:chef)
    end
  end
  
end