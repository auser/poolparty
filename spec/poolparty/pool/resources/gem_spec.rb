require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Gem" do
  before(:each) do
    reset!
    @cloud = cloud :gem_cloud do
      has_gempackage({:name => "rails"})
    end
    @gem = @cloud.resource(:gempackage)
  end
  it "should have a package" do
    @cloud.resource(:gempackage).should_not be_empty
  end
  it "should have the 'rails' gem in the packages" do
    @gem.to_string.should =~ /gem-package-rails/
  end
end