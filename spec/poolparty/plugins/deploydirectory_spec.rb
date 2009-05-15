require File.dirname(__FILE__) + '/../spec_helper'

class TestDeployDirectoryClass < PoolParty::Cloud::Cloud
end

describe "Remote Instance" do
  describe "wrapped" do
    before(:each) do
      reset!
      @tc = cloud :test_deploy_directory_class_cloud do
         has_deploy_directory("map-experiments", 
                              :from => "~/projects/westfield/map-experiments", 
                              :to => "/var/www/wifi-maps", 
                              :mode => "755",
                              :owner => "www-data")

         has_deploy_directory("map-experiments-two", 
                              :from => "~/projects/westfield/map-experiments", 
                              :to => "/var/www/wifi-maps-two", 
                              :mode => "755",
                              :owner => "www-data")

      end
      @compiled = ChefResolver.new(@tc.to_properties_hash).compile
    end

    describe "when being listed the first time" do
      it "should be a string" do
        @compiled.should =~ /execute/
      end

      it "should create the directory specified in 'to'" do
        @compiled.should =~ %r{directory "/var/www/wifi-maps"}
      end

      it "should copy from the deploy directory" do
        @compiled.should =~ %r{command "cp \-R /var/poolparty/dr_configure/user_directory/map-experiments/map-experiments/\* /var/www/wifi-maps"}
      end

      it "should chown if needed" do
        @compiled.should =~ %r{command "chown www-data \-R /var/www/wifi-maps"}
      end

      it "should chmod if needed" do
        @compiled.should =~ %r{command "chmod 755 /var/www/wifi-maps"}
      end
    end

    describe "when being listed a second time with the same source" do
      it "should create the directory specified in 'to'" do
        @compiled.should =~ %r{directory "/var/www/wifi-maps-two"}
      end

      it "should copy from the right deploy directory" do
        @compiled.should =~ %r{command "cp \-R /var/poolparty/dr_configure/user_directory/map-experiments-two/map-experiments/\* /var/www/wifi-maps-two"}
      end
    end
 
  end
end

# it should be able to have a different name and from