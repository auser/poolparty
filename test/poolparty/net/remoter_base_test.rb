require "#{::File.dirname(__FILE__)}/../../test_helper"

class RemoterBaseTest < Test::Unit::TestCase
  context "Available Bases" do
    
    should "have global remote_bases" do
      assert !PoolParty::Remote.available.empty?
      assert PoolParty::Remote.available.size >1
      assert PoolParty::Remote.available.include?(PoolParty::Remote::Ec2)
    end
    
    should "should return an array of top_level RemoterBase class symbols " do
      assert ::PoolParty::Remote::RemoterBase.available_bases.include?(:ec2)
    end
  end
  
end