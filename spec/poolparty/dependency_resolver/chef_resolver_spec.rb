require File.dirname(__FILE__) + '/../spec_helper'

describe "ChefResolver" do
  before :all do
    reset!
    @cloud_reference_hash = {
      :options => {:name => "dog", :keypair => "bob", :users => ["ari", "michael"]},
      :resources => [
        {:name => "/etc/motd", :content => "Welcome to the cloud", :pp_type => "file"},
        {:name => "/etc/profile", :content => "profile info", :pp_type => "file"},
        {:name => "/var/www", :pp_type => "directory"}
      ],
      :services => {
        :apache => [{
          :options => {:listen => "8080"},
          :resources => [
            {:name => "/etc/apache2/apache2.conf", :pp_type => "file", :template => "/absolute/path/to/template", :content => "rendered template string"}
          ],
          :services => {}
        }]
      }
    }
  end
  
  it "throw an exception if not given a hash" do
    pending # odd error with should raise_error
    # lambda { PoolParty::ChefResolver.compile }.should raise_error
  end
  it "accept a hash" do
    lambda { PoolParty::ChefResolver.compile(@cloud_reference_hash)}.should_not raise_error
  end
  
  describe "when passed a valid cloud hash" do
    
    before(:all) do
      @dr = ChefResolver.new(@cloud_reference_hash)
      @compiled = @dr.compile
    end
    
    # describe "variables" do
    #   it "output options as Chef variables" do
    #     @compiled.should match(/bob/)
    #     @compiled.instance_of?(String).should == true
    #     @compiled.should match(/\$users = \[ \".* \]/)
    #   end
    # end
    
    describe "resources" do
      it "should print resources in the proper layout" do
        @compiled.should =~ /template "\/etc\/motd" do/
      end
    end
    
    describe "services" do
      it "should print apache into a class definition" do
        # puts "<pre>#{@compiled.to_yaml}</pre>"
        # @compiled.should =~ /class apache \{/
        pending
      end
    end
    
  end
  
  describe "with a cloud" do
    before(:each) do
      class ChefResolverSpec
        plugin :apache do
        end
      end
      @cloud = cloud :hope do
        keypair "bob"
        has_file :name => "/etc/motd", :content => "Welcome to the cloud"        
        has_file :name => "/etc/profile", :content => "profile info"        
        has_directory :name => "/var/www"
        has_file :name => "/var/www/index.html", :content => "profile info", :requires => get_directory("/var/www")
        # has_package :name => "bash"        
        # parent == cloud
        apache do
          # parent == apache
          listen "8080"
          has_file :name => "/etc/apache2/apache2.conf", :template => "#{::File.dirname(__FILE__)}/../fixtures/test_template.erb", :friends => "bob"
          has_exec :command => "ls /etc/apache2"
        end
      end
      @properties = @cloud.to_properties_hash
      
      # puts "<pre>#{@cloud_reference_hash.to_yaml}\n\n#{@properties.to_yaml}</pre>"
      @dr = ChefResolver.new(@properties)
      @compiled = @dr.compile
      # puts "---\n#{@compiled}"
    end
    it "should compile to a string" do
      # puts "<pre>#{@compiled.to_yaml}</pre>"
      @compiled.class.should == String
    end
    it "should include apache class" do
      @compiled.should =~ /# apache/
    end
    it "should require the file to have the directory (written as file)" do
      @compiled.should =~ /directory \"\/var\/www\" do/
    end
    after(:all) do
      # ::FileUtils.rm_rf "/tmp/poolparty/dr_configure/"
    end
  end
  
end