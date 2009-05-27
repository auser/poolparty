require "#{::File.dirname(__FILE__)}/../../test_helper"

class RemoterBaseTest < Test::Unit::TestCase
  context "Available Bases" do
    
    should "have global remote_bases" do
      assert !remote_bases.nil?
      assert remote_bases.size >1
      assert remote_bases.include? :ec2
      assert $remote_bases ==  remote_bases
    end
    
    should "have remote_bases class method " do
      assert ::PoolParty::Remote::RemoterBase.available_bases.include? :ec2
    end
  end
  
end