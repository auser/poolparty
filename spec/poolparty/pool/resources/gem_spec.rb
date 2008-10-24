require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Gem" do
  before(:each) do
    reset!
    @cloud = cloud :gem_cloud do
      has_gempackage(:name => "activesupport") do
        has_gempackage(:name => "rails")
      end
    end
    @gem = @cloud.get_resource(:gempackage, "activesupport")
  end
  it "should have a package" do
    @cloud.resource(:gempackage).should_not be_empty
  end
  it "should have the name set as activesupport" do
    @gem.name.should == "activesupport"
  end
  it "should have the 'rails' gem in the packages" do
    @cloud.get_resource(:gempackage, "activesupport").to_string.should =~ /activesupport/
  end
  describe "with parent options" do
    before(:each) do
      reset_resources!
    end
    describe "reset" do
      before(:each) do      
        @cloud1 = cloud :gem_version_cloud do
          has_gempackage(:name => "ParseTree", :version => "2.2.0") do
            has_gempackage(:name => "edge-rails")
          end
        end
        @gem = @cloud1.resource(:gempackage).first
        @gem2 = @gem.resource(:gempackage).first
      end
      it "should have the version set on the parent" do
        @gem.version.should == "2.2.0"
      end
      it "should not take the version of the parent on the child" do
        @gem.resource(:gempackage).first.version.should == nil
      end
    end
  end
end