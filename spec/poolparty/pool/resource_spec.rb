require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Resources

require File.dirname(__FILE__) + '/test_plugins/webserver'

class MyResource < PoolParty::Resources::Resource
  # Just to give some options for the test class
  def options(h={})
    @options ||= {:a => 1,:b => 2,:c => 3}
  end
end
describe "Resource" do
  before(:each) do
    setup
  end
  describe "wrapped" do
    before(:each) do
      @resource = MyResource.new({:a => 10}) do
        b "90"
      end
    end
    it "should set a from the hash" do
      @resource.a.should == 10
    end
    it "should set b from within the block" do
      @resource.b.should == "90"
    end
    it "should not wipe out the rest of the default options" do
      @resource.c.should == 3    
    end
    describe "to_s" do
      it "should be able to coalesce the instances" do
        @resource.to_string.should =~ /resource \{/
      end
    end
    describe "class methods" do
      it "should have an array of available resources" do
        PoolParty::Resources::Resource.available_resources.class.should == Array
      end
      it "should not be empty" do
        PoolParty::Resources::Resource.available_resources.should_not be_empty
      end
    end
    describe "instance methods" do
      before(:each) do
        @resource = MyResource.new
      end
      %w(requires ensures is_present is_absent ifnot).each do |method|
        eval <<-EOE
          it "should have the method #{method} available" do; @resource.respond_to?(:#{method}).should == true; end
        EOE
      end
      it "should be able to take requires method" do
        @resource.respond_to?(:requires).should == true
      end
      it "should push require onto the options" do
        @resource.options.has_key?(:require).should == false
        @resource.requires("nibbles")
        @resource.options.has_key?(:require).should == true
      end
      it "should be able to call ensures method on the resource" do
        @resource.respond_to?(:ensures).should == true
      end
      it "should push the option ensure onto the options" do
        @resource.options.has_key?(:ensure).should == false
        @resource.ensures("nibbles")
        @resource.options.has_key?(:ensure).should == true
      end
      it "should write the option ensures as present with is_present" do
        @resource.options.has_key?(:ensure).should == false
        @resource.is_present
        @resource.options[:ensure].should == "present"
      end
      it "should write the option ensures as absent with is_absent" do
        @resource.options.has_key?(:ensure).should == false
        @resource.is_absent
        @resource.options[:ensure].should == "absent"
      end
      it "should write the option unless for ifnot" do
        @resource.options.has_key?(:unless).should == false
        @resource.ifnot "str"
        @resource.options[:unless].should == "str"
      end
      describe "templating" do
        before(:each) do
          FileUtils.stub!(:cp).and_return true        
        end
        it "should have the method template" do
          @resource.respond_to?(:template).should == true
        end
        it "should raise an exception if no file is given" do
          lambda {
            @resource.template
          }.should raise_error
        end
        it "should raise an excepton if the file cannot be found" do
          lambda {
            @resource.template("radar")
          }.should raise_error
        end
        it "should not raise an exception if there is a file passed and the file is found" do
          File.should_receive(:file?).with("radar").and_return true
          lambda {
            @resource.template("radar")
          }.should_not raise_error
        end
        # it "should push the template option on to the options" do
        #   File.stub!(:file?).with("radar").and_return true
        #   @resource.options.has_key?(:template).should == false
        #   @resource.template("radar")
        #   @resource.options.has_key?(:template).should == true
        # end
      end
    end
    describe "command" do
      include PoolParty::Resources
      before(:each) do
        @cloud = cloud :command_cloud do; end
      end
      describe "add_resource" do
        it "should call add_resource when creating using the command: file" do
          @cloud.should_receive(:add_resource).with(:file, {:name => "frank"}, @cloud)
          @cloud.instance_eval do
            file(:name => "frank")
          end
        end
        it "should return a resource when the resource does not exist" do
          file(:name => "/etc/frank.txt").class.should == PoolParty::Resources::File
        end
        it "should return a resource when the resource does exist" do
          file(:name => "frank")
          file(:name => "frank").class.should == PoolParty::Resources::File
        end
      end
      it "should create the new 'resource' as a resource" do
        resource(:file).class.should == Array
      end
      it "should receive << when adding a new one" do
        resource(:file).should_receive(:<<).once
        has_file({:name => "pop"})
      end
      it "should contain 3 instances after calling resource 3 times" do
        file({:name => "red"})
        file({:name => "hot"})
        file({:name => "summer"})
        resource(:file).size.should == 3
      end
      describe "adding" do
        before(:each) do
          @a = file({:name => "red"})
          @b = file({:name => "hot"})
          @c = file({:name => "summer"})
        end
        it "should contain file named with 'red'" do
          get_resource(:file, "red").nil?.should == false
        end
      end
      describe "method_missing" do
        before(:each) do
          file({:name => "red"})
          file({:name => "hot"})
          file({:name => "summer"})
        end
        it "should be able to pick out methods with the phrase has_" do
          lambda {
            has_file
          }.should_not raise_error
        end
        it "should not have a method prepended with crabs_" do
          lambda {
            crabs_file
          }.should raise_error
        end
        it "should pick out methods with the phrase does_not_" do
          lambda {
            does_not_have_file({:name => "red"})
          }.should_not raise_error
        end
        it "should set the has_file to present ensure" do
          has_file({:name => "redface"})
          resource(:file).get_named("redface").first.options[:ensure].should == "present"
        end
        it "should set the does_not_have_file to absent ensure" do
          does_not_have_file({:name => "net"})
          resource(:file).get_named("net").first.options[:ensure].should == "absent"
        end
        it "should be able to have_service as well" do
          has_service({:name => "apache"})
          resource(:service).get_named("apache").first.options[:ensure].should == "running"
        end      
      end
      describe "get_resource" do
        before(:each) do
          reset_resources!
          @cloud_get_resource = cloud :get_resource do
            file(:name => "red")
            file(:name => "hot")
            file(:name => "tamales")
          end
        end
        it "should return a type of resource when looking for a resource that exists" do
          @cloud_get_resource.get_resource(:file, "hot").class.should == PoolParty::Resources::File
        end
        it "should return the resource of the name requested" do
          @cloud_get_resource.get_resource(:file, "hot").name.should == "hot"
        end
        it "should return nil if the resource requested is not there" do
          @cloud_get_resource.get_resource(:file, "smarties").should be_nil
        end
        it "should not have created any more resources" do
          @cloud_get_resource.resource(:file).size.should == 3
        end
      end
      describe "parent" do
        before(:each) do
          @cloud = cloud :orange do
            tangerine "orange"
            file(:name => "file.txt")
          end
          @file = @cloud.resources[:file].first
        end
        it "should take the options of the parent" do        
          @file.parent.tangerine.should_not == nil
        end
        it "should set the option as the same from the parent" do
          @file.parent.tangerine.should == "orange"
        end
      end
      describe "appending to resource" do
        before(:each) do
          @cloud1 = cloud :apples do
            directory(:name => "/var/www") do
              file(:name => "/var/www/file.html")
            end
          end
          @dir = @cloud1.get_resource(:directory, "/var/www")
          @file = @dir.get_resource(:file, "/var/www/file.html")
        end
        it "should say there is 1 resource because the lower-level resources should be contained on the parenting resource" do
          @cloud1.resources.size.should == 1
        end
        it "should say there is one resource on the outer resource" do
          @dir.resources.size.should == 1
        end
        it "should contain the file as a resource" do
          @dir.resource(:file)[0].to_s.should == @file.to_s
        end
        it "should set the parent as the parenting resource" do
          @file.parent.to_s.should == @dir.to_s
        end
      end
      describe "fetching" do
        before(:each) do
          @file = file(:name => "pancakes")
        end
        it "should not create a new resource if the same resource exists" do
          PoolParty::Resources::File.should_not_receive(:new)
          file(:name => "pancakes")
        end
        it "should return the file preiously created" do
          get_resource(:file, "pancakes").should == @file
        end
        it "should be able to use the helper to grab the file" do
          PoolParty::Resources::File.should_not_receive(:new)
          file(:name => "pancakes").should == @file
        end
        it "should turn the resource into a string" do
          @file.to_s.should == "File['pancakes']"
        end
        describe "cancelled" do
          it "should make a resource not cancelled by default" do
            @file.cancelled?.should == false
          end
          it "should say cancelled? is true if it has been cancelled" do
            @file.cancel
            @file.cancelled?.should == true
          end
          it "should be able to get the file from the helper" do
            get_file("pancakes").should == @file
          end
        end
        describe "virtual_resources" do
          before(:each) do
            @virtual_resource = git(:name => "tank", :source => "git://github.com/auser/testgit.git")
          end
          it "should fetch a virtual resource the same" do
            git(:name => "tank", :source => "git://github.com/auser/testgit.git").to_s.should == @virtual_resource.to_s
          end
        end
      end
      describe "global resources" do
        describe "setting" do
          before(:each) do        
            @cloud2 = cloud :applepie do          
              directory(:name => "/var/www") do
                file(:name => "/var/www/file.html")
              end
            end
            @cloud2 = cloud(:applepie)
          end
          it "should have a global_resources_store available as an Array" do
            @cloud2.global_resources_store.class.should == Array
          end
          it "should have stored the two resources from the cloud into the global_resources_store" do
            @cloud2.global_resources_store.size.should >= 2
          end
          it "should be an array of resources" do
            @cloud2.global_resources_store.first.is_a?(PoolParty::Resources::Resource).should == true
          end
          it "should say that the type directory of name /var/www is in the global_resources_store" do
            @cloud2.in_global_resource_store?(:directory, "/var/www").should == true
          end
          it "should say that the type file of the name /var/www/file.html is in the global_resources_store" do
            @cloud2.in_global_resource_store?(:file, "/var/www/file.html").should == true
          end
          it "should say tht the type exec of the name echo 'hello' is not in the global_resources_store" do
            @cloud2.in_global_resource_store?(:exec, "echo 'hello'").should == false
          end
          it "should say that the resource is in the global_resources_store when put in the global_resources_store" do
            @file1 = PoolParty::Resources::File.new(:name => "/var/tmp.txt")
            @cloud2.store_into_global_resource_store(@file1)
            @cloud2.in_global_resource_store?(:file, "/var/tmp.txt")
          end
          it "should put the resource into the global_resources_store when calling store_into_global_resource_store" do
            @file2 = PoolParty::Resources::File.new({:name => "/var/bunk.txt"})
            @cloud2.global_resources_store.should_receive(:<<).with(@file2)
            @cloud2.store_into_global_resource_store(@file2)
          end
          it "should not put a resource into the global_resources_store if it's already there" do
            @file3 = PoolParty::Resources::File.new({:name => "/var/www/file.html"})
            @cloud2.global_resources_store.should_not_receive(:<<).with(@file3)
            @cloud2.store_into_global_resource_store(@file3)
          end
          it "should be able to get the resource from the global_resources_store by the name and type" do
            @cloud2.get_from_global_resource_store(:directory, "/var/www").key.should == "/var/www"
          end
          describe "adding to global resource store" do
            before(:each) do
              @cloud3 = cloud :pumpkinpie do          
                file(:name => "/var/www/pumpkinfile.html")
                file(:name => "/var/www/pumpkinfile.html")
                apache do
                  file(:name => "/var/www/pumpkinfile.html")
                end
              end
              @cloud3 = cloud(:pumpkinpie)
            end
            it "should not have 2 of the same resources" do
              @cloud3.resource(:file).size.should == 1
            end
          end
          describe "grabbing after already instantiated" do
            before(:each) do
              @cloud2.instance_eval do
                has_package(:name => "apache2") do
                  has_exec(:name => "Add apache2 module") do
                    command "a2enmod mpm_worker"
                  end
                end
                has_package(:name => "boxers") do
                  has_file(:name => "/var/list_of_boxers.txt", :requires => (get_package("apache2")) )
                end
              end
            end
            describe "same_resources_of" do
              it "should say that two resources are the same" do
                has_file(:name => "tanks")
                has_file(:name => "tanks").same_resources_of(:file, "tanks").should == true
              end
              it "should say that two resources are the same regardless of their context" do
                @cloud2.instance_eval do
                  has_file(:name => "mighty_mighty_bosstones")
                end
                @cloud2.resource(:file).first.same_resources_of(:file, "mighty_mighty_bosstones").should == true
              end
              it "should say that two resources are not the same if they are not the same" do
                has_file(:name => "trees").same_resources_of(:file, "timber").should == false
              end
            end
            it "should grab the resource when called in in a block"# do
            #   @cloud2.resources_string_from_resources(@cloud2.resources).should =~ /\[ Package\['apache2'\], Package\['boxers'\] \]/
            # end
          end
        end
      end
    end    
  end
end
