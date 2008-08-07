require File.dirname(__FILE__) + '/../../spec_helper'

describe "Configuer" do
  before(:each) do
    @conf = Object.new
  end
  it "should not be nil" do
    @conf.should_not be_nil
  end
  
  describe "with a spec file" do
    before(:each) do
      @pool = Script.new
      Script.stub!(:new).and_return(@pool)
      @basic = read_file(File.join(File.dirname(__FILE__), "files", "ruby_basic.rb"))
    end
    it "should load the basic example configure" do
      puts @pool.pools
      @pool.pools[:poolpartyrb].should_not be_nil
    end
    it "should contain a list of the clouds within the pool" 
    it "should store the cloud with a name"
    
    after do
      Script.inflate @basic
    end
  end
  
end