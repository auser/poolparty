require File.dirname(__FILE__) + '/../../spec_helper'

include PoolParty::Resources

describe "Conditional" do
  before(:each) do
    reset_resources!
  end
  describe "wrapped" do
    before(:each) do
      @cloud = cloud :conditional_cloud_spec do
          execute_if("$hostname","'master'", {}, self) do
            has_file({:name => "/etc/apache2/puppetmaster2.conf"})
          end
      end
      @cond = @cloud.get_resource(:conditional, "$hostname 'master'")
    end
    it "should add the block of resources on the parent" do
      @cloud.resources.size.should == 1
    end
    it "should have a conditional in the resources" do
      @cond.name.should == "$hostname 'master'"
    end
    it "should push the resources onto the conditional resource" do
      @cond.resources.size.should == 1
    end
    it "should have a file resource on the conditional" do
      @cond.get_file("/etc/apache2/puppetmaster2.conf").name.should == "/etc/apache2/puppetmaster2.conf"
    end
    it "should have the parent as the cloud" do
      @cond.parent.should == @cloud
    end
    describe "helpers" do
      it "should have execute_on_master with the string $hostname == 'master'" do
        str = execute_on_master do
          has_file(:name => "/etc/vars")
        end.to_string.should =~ /\$hostnamemaster/
      end
      it "should have execute_on_node with the string $hostname != 'master'" do
        str = execute_on_node do
          has_file(:name => "/etc/vars")
        end.to_string.should =~ /\$hostnamemaster/
      end
    end
    describe "to_string" do
      before(:each) do
        @string = @cond.to_string
      end
      it "should have a case statement for the hostname" do
        @string.should =~ /case \$hostname/
      end
      describe "multiple" do
        before(:each) do
          reset!
          @cloud = cloud :multiple_conditionals do
            execute_on_master do
              has_file(:name => "/etc/frank.txt")
            end
            execute_on_master do
              has_exec(:name => "feed frank")
            end
          end
        end
        it "should have two conditional resources" do
          @cloud.resource(:conditional).size.should == 2
        end
      end
      describe "execute on node" do
        it "should place the node in the default section" do
          str = execute_on_node do
            has_file(:name => "/etc/vars")
          end.to_string.should =~ /default : \{/
        end
      end
    end
  end
end