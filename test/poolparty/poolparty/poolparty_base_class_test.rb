require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestBaseClass < PoolParty::PoolPartyBaseClass
end

class TestBaseClassTest < Test::Unit::TestCase
  context "services" do
    before(:each) do
      reset!
      @tbc = TestBaseClass.new do
        has_git_repos :name => "test git", :at => "/var/www/google", :source => "git://pop.git"
      end
    end
    it "should add a service when a service is called" do
      @tbc.services.size.should == 1
    end
  end
  context "context_stack" do
    before(:each) do
      ::PoolParty.reset!
      @a = TestBaseClass.new
    end
    it "should have a context stack that is empty" do
      @a.context_stack.empty?.should == true
    end
    it "should have a context_stack that is not empty when being evaluated" do
      TestBaseClass.new do
        context_stack.empty?.should == false
      end
    end
    it "should have self in context_stack" do
      TestBaseClass.new do
        context_stack.last.should == self
      end
    end
    it "should have the parent of self set in the context stack in the current_context" do
      TestBaseClass.new do
        current_context.last.should == parent
      end
    end
    context "depth" do
      before(:each) do
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
      it "should set the correct depth" do
        @a.depth.should == 0
        @@b.depth.should == 1
        @@c.depth.should == 2
      end
      it "should have the parent set properly" do
        # @a.parent.should == nil
        @@c.parent.should == @@b
        @@b.parent.should == @a
      end
      it "should have proper self" do
        @a.this.should == $a.this
      end
    end
  end
  it "should have the parent set properly" do
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