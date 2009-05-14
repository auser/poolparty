require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Resources

describe "Script" do

  describe "wrapped" do
    it "should have inflate as a class method" do
      Script.respond_to?(:inflate).should == true
    end
    it "should have inflate_file as an instance method" do
      Script.respond_to?(:inflate_file).should == true
    end

    describe "with a script" do
      # describe "save!" do
      #   before(:each) do
      #     reset!
      # 
      #     pool :appdotcomcool do
      #       ami "ami-123456"
      #       
      #       cloud :app do
      #         expand_when "cpu > 90", "memory > 80"
      #         contract_when "cpu < 10", "memory < 10"
      #         
      #         has_file :name => "/etc/httpd/httpd.conf"
      #       end
      #     end
      #     @saved = Script.save!(false)
      #   end
      #   it "should save the full keypair" do
      #     @saved.should =~ /keypair ([\w -\/]+)+id_rsa'/
      #   end
      #   it "should save the ami" do
      #     @saved.should =~ /ami 'ami-123456'/
      #   end
      #   it "should save the expansions" do
      #     @saved.should =~ /expand_when 'cpu>90', 'memory>80'/
      #   end
      # end
      # NOT ENTIRELY CERTAIN THIS SHOULD WORK THE SAME WAY IT WORKED BEFORE
      # AL
    end
  end
end
