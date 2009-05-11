require File.dirname(__FILE__) + '/../spec_helper'

class DependencyResolverCloudExtensionsSpecBase < PoolParty::PoolPartyBaseClass
  include Dslify
end

# files, directories, etc...
class DependencyResolverSpecTestResource
  include Dslify
  include PoolParty::DependencyResolverResourceExtensions
  
  dsl_methods :name, :template, :content
end

# plugins, base_packages
class DependencyResolverSpecTestService < DependencyResolverCloudExtensionsSpecBase
  dsl_methods :listen
end

# clouds, duh
class DependencyResolverSpecTestCloud < DependencyResolverCloudExtensionsSpecBase
  dsl_methods :keypair, :name
end

class JunkClassForDefiningPlugin
  plugin :apache_plugin do
    default_options(
      :listen => 80
    )
    def loaded(o={},&block)
    end
  end  
end

describe "Resolution spec" do
  before(:each) do
    @apache_file = DependencyResolverSpecTestResource.new
    @apache_file.name = "/etc/apache2/apache2.conf"
    @apache_file.template = "/absolute/path/to/template"
    @apache_file.content = "rendered template string"
    
    @apache = DependencyResolverSpecTestService.new :apache_file
    @apache.listen = "8080"
    @apache.resources[:file] = []
    @apache.resources[:file] << @apache_file
        
    @cloud = DependencyResolverSpecTestCloud.new :cloud
    @cloud.keypair = "bob"
    @cloud.name = "dog"
    
    (@cloud.resources[:apache] ||= []) << @apache

    @cloud_file_motd = DependencyResolverSpecTestResource.new
    @cloud_file_motd.name = "/etc/motd"
    @cloud_file_motd.content = "Welcome to the cloud"
    
    @cloud_file_profile = DependencyResolverSpecTestResource.new
    @cloud_file_profile.name = "/etc/profile"
    @cloud_file_profile.content = "profile info"
        
    @cloud.resources[:file] = []
    @cloud.resources[:file] << @cloud_file_motd
    @cloud.resources[:file] << @cloud_file_profile
    
    @cloud_directory_var_www = DependencyResolverSpecTestResource.new
    @cloud_directory_var_www.name = "/var/www"
    
    @cloud.resources[:directory] = []
    @cloud.resources[:directory] << @cloud_directory_var_www    
  end
  it "be able to call to_properties_hash" do
    @cloud.respond_to?(:to_properties_hash).should == true
  end
  describe "to_properties_hash" do
    it "should output a hash" do
      @cloud.to_properties_hash.class.should == Hash
    end
    it "should have resources on the cloud as an array of hashes" do      
      @cloud.to_properties_hash[:resources].class.should == Array
    end
  end

  describe "defined cloud" do
    before(:each) do
      reset!
      ::File.stub!(:basename).and_return "template"
      @file = "Hello <%= friends %> on port <%= listen %>"
      @file.stub!(:read).and_return @file
      Template.stub!(:open).and_return @file

      cloud :dog_for_test do
        keypair "bob"
        has_file :name => "/etc/motd", :content => "Welcome to the cloud"
        has_file :name => "/etc/profile", :content => "profile info"
        has_directory :name => "/var/www"
        has_package :name => "bash"
        # parent == nil
        apache_plugin do
          # parent == TestClass
          # puts "<pre>#{parent}</pre> on <pre>#{context_stack.map {|a| a.class }.join(", ")} from #{self.class}</pre>"
          listen "8080"
          has_file :name => "/etc/apache2/apache2.conf", :template => "/absolute/path/to/template", :friends => "bob", :render_as => :erb
        end
      end
      @cloud = clouds[:dog_for_test]
      @properties = @cloud.to_properties_hash
      @apache_key = @properties[:resources].select {|hsh| hsh[:name] =~ /apache/ }.first
    end
    
    it "should have the method to_properties_hash on the cloud" do
      @cloud.respond_to?(:to_properties_hash).should == true
    end
    it "should have resources on the cloud as an array of hashes" do
      # puts "<pre>#{cloud(:dog).to_properties_hash.to_yaml}</pre>"
      @properties[:resources].class.should == Array
    end
    it "contain content in the template's hash" do
      @apache_key.resources.select_with_hash(:pp_type => "file").last[:content].should == "Hello bob on port 8080"
    end
    it "contain the files in a hash" do
      # puts "<pre>#{@properties.to_yaml}</pre>"
      @properties[:resources].select {|a| a[:name] == "/etc/motd" }.first.nil?.should == false
    end
    it "contain the directory named /var/www" do
      @properties[:resources].select {|a| a[:name] == "/var/www" }.first.nil?.should == false
    end
  end
end