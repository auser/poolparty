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
      @tbc.ordered_resources.size.should > 1
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