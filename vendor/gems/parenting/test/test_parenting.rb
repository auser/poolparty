require File.dirname(__FILE__) + '/test_helper.rb'

class Quickie
  include Parenting
  attr_accessor :my_name
  def initialize(nm="Default", &block)
    @my_name ||= nm
    run_in_context(&block) if block
  end
end

class QuickieTest < Test::Unit::TestCase
  context "setting" do
    before do
      @a = Quickie.new do
        $b = Quickie.new("franke") do
          $c = Quickie.new("bob") do
          end
        end
      end
    end
    it "should set the parents properly" do
      assert_equal $c.parent, $b
      assert_equal $b.parent, @a
      assert_equal @a.parent, nil
    end
    it "should have proper depth" do
      assert_equal @a.depth, 0
      assert_equal $b.depth, 1
      assert_equal $c.depth, 2
    end
    it "should have current_context" do
      assert_equal $context_stack, []
      assert_equal @a.current_context, []
      assert_equal $b.current_context,[@a]
      assert_equal $c.current_context,[@a, $b]
    end
    it "should set my_name on $c to frank" do
      assert_equal $c.my_name, "bob"
    end
    it "should set my_name on $b to bob" do
      assert_equal $b.my_name, "franke"
    end
    it "should not set my_name to is_frank on @a" do
      assert_equal @a.my_name, "Default"
    end
  end
  context "from within a module_eval" do
    before(:all) do
      instance_eval <<-EOE
        @a = Quickie.new do
          self.class.send :attr_reader, :b
          @b = Quickie.new do
            self.class.send :attr_reader, :c
            @c = Quickie.new do
              self.class.send :attr_reader, :d
              $d = @d = Quickie.new do
              end
            end
          end
        end
      EOE
    end
    it "should set the parent's properly" do
      assert_equal @a.parent, nil
      assert_equal @a.b.parent, @a
      assert_equal @a.b.c.parent, @a.b
      assert_equal @a.b.c.d.parent, @a.b.c
    end
    it "should set the depth" do
      assert_equal @a.depth, 0
      assert_equal @a.b.depth, 1
      assert_equal @a.b.c.depth, 2
      assert_equal @a.b.c.d.depth, 3
    end
    it "should have a current context" do
      assert_equal @a.context_stack.size, 0
      assert_equal @a.b.current_context, [@a]
      assert_equal @a.b.c.current_context, [@a,@a.b]
      assert_equal @a.b.c.d.current_context, [@a, @a.b, @a.b.c]
    end
    it "should no be weird" do
      assert_equal $d, @a.b.c.d
      assert_equal $d.parent, @a.b.c
      assert_equal $d.parent.parent.parent, @a
    end
  end
  context "calling a method on parent that doesn't exist on self" do
    setup do
      class Tiny
        include Parenting
        attr_reader :message
        def initialize(&block)
          run_in_context(&block) if block
        end
      end
      instance_eval <<-EOE
        @a = Quickie.new :outside do
          self.class.send :attr_reader, :b          
          @b = Tiny.new do
            @message = my_name
          end
        end
      EOE
    end

    should "setup the parents properly" do
      assert_equal @a.parent, nil
      assert_equal @a.b.parent, @a
    end
    should "call my_name on the parent" do
      assert_equal @a.b.message, :outside
    end
  end
  

end