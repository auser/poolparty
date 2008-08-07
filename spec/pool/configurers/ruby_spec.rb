require File.dirname(__FILE__) + '/../../spec_helper'

describe "Configuer" do
  before(:each) do
    @conf = Object::Ruby.new
  end
  it "should not be nil" do
    @conf.should_not be_nil
  end
  
  describe "with a spec file" do
    before(:each) do
      @basic = read_file(File.join(File.dirname(__FILE__), "files", "ruby_basic.rb"))
    end
    it "should load the basic example configure"
    it "should contain a list of the clouds within the pool" 
    it "should store the cloud with a name"
  end
  
end