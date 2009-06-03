require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestChefResolver < Test::Unit::TestCase
  context "from a hash" do
    setup do
      reset!
      @cloud_reference_hash = {
        :options => {:name => "dog", :keypair => "bob", :users => ["ari", "michael"]},
        :resources => [
          {:name => "/etc/motd", :content => "Welcome to the cloud", :pp_type => "file"},
          {:name => "/etc/profile", :content => "profile info", :pp_type => "file"},
          {:name => "/var/www", :pp_type => "directory"},
          {:name => "apache_class", :pp_type => "plugin", :resources => [
            {:name => "/etc/apache2/apache2.conf", :pp_type => "file", :template => "/absolute/path/to/template", :content => "rendered template string"}
          ]}
        ],
      }
    end

    should "throw an exception if not given a hash" do
      lambda { PoolParty::ChefResolver.compile }.should raise_error
    end
    should "accept a hash" do
      lambda { PoolParty::ChefResolver.compile(@cloud_reference_hash)}.should_not raise_error
    end
    context "compiled" do
      setup do
        reset!
        @dr = ChefResolver.new(@cloud_reference_hash)
        @compiled = @dr.compile
      end

      should "should print resources in the proper layout" do        
        @compiled.should =~ /template \"\/etc\/motd\" do\n\tsource \"\/etc\/motd\.erb\"/
      end

      should "should print apache into a class definition" do
        # puts "<pre>#{@compiled.to_yaml}</pre>"
        @compiled.should =~ /# apache/
      end
    end
  end
  
  context "with a cloud" do
    setup do
      reset!
      @cloud = cloud :hope do
        keypair "bob"
        has_file :name => "/etc/motd", :content => "Welcome to the cloud"        
        has_file :name => "/etc/profile", :content => "profile info"        

        has_directory :name => "/var/www"
        has_file :name => "/var/www/index.html", :content => "profile info", :requires => get_directory("/var/www")

        dependency_resolver "chef"
        # has_package :name => "bash"        
        # parent == cloud
        apache do
          # parent == apache
          listen "8080"
          has_file :name => "/etc/apache2/apache2.conf", :template => "#{::File.dirname(__FILE__)}/../../fixtures/test_template.erb", :friends => "bob"
          has_exec :command => "ls /etc/apache2"
        end
      end
      @properties = @cloud.to_properties_hash

      # puts "<pre>#{@cloud_reference_hash.to_yaml}\n\n#{@properties}</pre>"
      @dr = ChefResolver.new(@properties)
      @compiled = @dr.compile
    end
    should "should compile to a string" do
      # puts "<pre>#{@compiled.to_yaml}</pre>"
      @compiled.class.should == String
    end
    should "should include apache class" do
      @compiled.should =~ /# apache/
    end
    should "should require the file to have the directory (written as file)" do
      @compiled.should =~ /directory \"\/var\/www\"/
    end
  end
end
