require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestBaseClass < PoolParty::PoolPartyBaseClass
end

class TestBaseClassTest < Test::Unit::TestCase
  context "services" do
    setup do
      reset!
      @tbc = TestBaseClass.new do
        has_git_repo :name => "git://pop.git", :to => "/var/www/google"
      end
    end
    should "should add a service when a service is called" do
      @tbc.ordered_resources.size.should > 0
    end
  end
  context "context_stack" do
    setup do
      ::PoolParty.reset!
      @a = TestBaseClass.new
    end
    should "should have a context stack that is empty" do
      @a.context_stack.empty?.should == true
    end
    should "should have a context_stack that is not empty when being evaluated" do
      TestBaseClass.new do
        context_stack.empty?.should == false
      end
    end
    should "should have self in context_stack" do
      TestBaseClass.new do
        context_stack.last.should == self
      end
    end
    should "should have the parent of self set in the context stack in the current_context" do
      TestBaseClass.new do
        current_context.last.should == parent
      end
    end
    context "depth" do
      setup do
        reset!
        Proc.new do 
          @a = $a =TestBaseClass.new do
            @@b = $b = TestBaseClass.new do
              @@c = $c =TestBaseClass.new do
              end
            end
          end
        end.call
      end
      should "should set the correct depth" do
        @a.depth.should == 0
        @@b.depth.should == 1
        @@c.depth.should == 2
      end
      should "should have the parent set properly" do
        # @a.parent.should == nil
        @@c.parent.should == @@b
        @@b.parent.should == @a
      end
      should "should have proper self" do
        @a.this.should == $a.this
      end
    end
  end
  should "should have the parent set properly" do
    @a = TestBaseClass.new do
      @@b = TestBaseClass.new do
        @@c = TestBaseClass.new do
        end
      end
    end

    @a.parent.should == nil
    @@c.parent.should == @@b
    @@b.parent.should == @a

    @a.parent.should == nil
    @@c.parent.should == @@b
    @@b.parent.should == @a
  end
end

class TestLookupMethod < Test::Unit::TestCase
  context "look up methods" do
    setup do
      reset!
      @tbc = TestBaseClass.new do
        has_file "/var/note.txt"
        has_directory "/var"
        has_package :name => "apache2"
        has_git_repo "git://git.git", :repos => "git://git.git", :to => "/var/www"
      end
    end
    should "have the method gems" do
      assert @tbc.respond_to?(:files)
    end
    should "have 1 file" do
      assert_equal @tbc.files.size, 1
    end
    should "have the file name in the files lookup method" do
      assert_equal @tbc.files.first.name, "/var/note.txt"
    end
    should "have the lookup method packages" do
      assert @tbc.respond_to?(:packages)
      assert_equal @tbc.packages.size, 1
      assert_equal @tbc.packages.first.name, "apache2"
    end
    should "have lookup method directorys" do
      assert @tbc.respond_to?(:directorys)
      assert_equal @tbc.directorys.size, 1
      assert_equal @tbc.directorys.first.name, "/var"
    end
    should "have plugin lookup methods" do
      assert @tbc.respond_to?(:git_repos)
      assert_equal @tbc.git_repos.size, 1
      assert_equal @tbc.git_repos.first.name, "git://git.git"
    end
  end  
end
