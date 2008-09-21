require File.dirname(__FILE__) + '/../spec_helper'

include Remote

class TestClass
  include CloudResourcer
  include Remoter
  
  def keypair
    "fake_keypair"
  end
end
describe "Remoter" do
  before(:each) do
    @tc = TestClass.new
  end
  it "should have the ssh command" do
    @tc.ssh_string.should =~ /ssh -o StrictHostKeyChecking=no -l '#{Base.user}' -i/
  end
  it "should have the keypair in the ssh_string" do
    @tc.ssh_string.should =~ /#{@tc.keypair}/
  end
end