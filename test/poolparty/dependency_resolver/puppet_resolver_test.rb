require "#{::File.dirname(__FILE__)}/../../test_helper"

class PuppetResolverSpec
  plugin :apache do
  end
end

class TestPuppetResolver < Test::Unit::TestCase
  context "from a hash" do
    setup do
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

    should "throw an exception if not given a hash" do
      lambda { PoolParty::PuppetResolver.compile }.should raise_error
    end
    should "accept a hash" do
      lambda { PoolParty::PuppetResolver.compile(@cloud_reference_hash)}.should_not raise_error
    end
    context "compiled" do
      setup do
        reset!
        @dr = PuppetResolver.new(@cloud_reference_hash)
        @compiled = @dr.compile
      end

      should "should print resources in the proper layout" do        
        @compiled.should =~ /file \{ "\/etc\/motd"/
      end

      should "should print apache into a class definition" do
        # puts "<pre>#{@compiled.to_yaml}</pre>"
        @compiled.should =~ /class apache \{/
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

        dependency_resolver "puppet"
        # has_package :name => "bash"        
        # parent == cloud
        apache do
          # parent == apache
          listen "8080"
          has_file :name => "/etc/apache2/apache2.conf", :template => "#{::File.dirname(__FILE__)}/../../fixtures/test_template.erb", :friends => "bob"
          has_exec :command => "ls /etc/apache2"
        end

        case_of "hostname" do
          when_is 'master' do            
            has_package :name=>'haproxy'
          end
        end
      end
      @properties = @cloud.to_properties_hash

      # puts "<pre>#{@cloud_reference_hash.to_yaml}\n\n#{@properties}</pre>"
      @dr = PuppetResolver.new(@properties)
      @compiled = @dr.compile
    end
    should "should compile to a string" do
      # puts "<pre>#{@compiled.to_yaml}</pre>"
      @compiled.class.should == String
    end
    should "should include apache class" do
      @compiled.should =~ /class apache \{/
    end
    should "should include the case statement" do
      @compiled.should =~ /case \$hostname \{/
      @compiled.should =~ /master : \{/
    end
    should "should require the file to have the directory (written as file)" do
      @compiled.should =~ /require => File\[\"\/var\/www\"\]/
    end
    should "should ensure file" do
      @compiled.should =~ /ensure => "present"/      
    end
  end
end
