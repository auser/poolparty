require "#{File.dirname(__FILE__)}/../../../test_helper"

include_fixture_resources

class ExecResourceTest < Test::Unit::TestCase
  context "Base" do
    setup do
      PoolParty::Resource.define_resource_methods
      @res = PoolParty::Resources::Exec.new "/usr/sbin/mail -s 'hi' arilerner@mac.com"
    end
    
    should "have the method denoted by has_method_name" do
      str = "execute \"/etc/poolparty\" do\n  path: ['/usr/bin:/bin:/usr/local/bin:$PATH']\nend\n"
      
      assert_match /execute "\/usr\/sbin\/mail -s 'hi' arilerner@mac\.com"/, @res.compile(:chef)
    end
    
  end
  
end