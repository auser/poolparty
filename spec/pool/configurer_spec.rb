require File.dirname(__FILE__) + '/../spec_helper'

describe "Configuer" do
  before(:each) do
    @conf = Configurer.new
  end
  it "should not be nil" do
    @conf.should_not be_nil
  end
  
  describe "files" do
    before(:each) do
      @basic = read_file(File.join(File.dirname(__FILE__), "configurers", "basic.rb"))
    end
    it "should load the basic example configure" do      

    end
  end
  
end