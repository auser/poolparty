require "#{File.dirname(__FILE__)}/test_helper"

class Quickie
  include Dslify
  def initialize(&block)
    instance_eval &block if block
  end
end

class QuickieTest < Test::Unit::TestCase
  context "setting" do
    before do
      Quickie.class_eval do
        dsl_methods :bank, :snobs, :author, :gilligans_island
      end
      @q = Quickie.new
    end
    it "should be able to set methods on self" do
      assert_nothing_raised do
        @q.bank "bobs"
      end
    end
    it "should set and then retrieve the same value back" do
      @q.snobs "are mean"
      assert_equal @q.snobs, "are mean"
    end
    it "should set and retrieve values back with an = sign" do
      @q.author = ["Ari Lerner"]
      @q.snobs = "Michael"
      assert_equal @q.author, ["Ari Lerner"]
      assert_equal @q.snobs, "Michael"
    end
    it "should set these values in the h Hash on the object" do
      assert_raise NoMethodError do
        @q.movies "can be fun"
      end
    end
    it "should set multiple keys with set_vars_from_options" do
      @q.set_vars_from_options({:a => "a", :b => "b"})
      assert_equal @q.a, "a"
      assert_equal @q.b, "b"
    end
    it "should set methods even when they are called with a block" do
      @q.author Quickie.new do
      end
      assert_equal @q.author.class, Quickie
    end
  end
  
  context "calling methods on an instance" do
    setup do
      class Detective
        include Dslify
        attr_reader :snooped
        def snoop(*n)
          @snooped = "done!"
        end
      end
      @d= Detective.new
    end
    should "Call the method snoop with set_vars_from_options" do
      @d.set_vars_from_options(:snoop => true)
      assert @d.snooped
    end
  end
  
  context "default options" do
    setup do
      class Bang
        include Dslify
        default_options(
          :says => 'vmrun'
        )
        def initialize(opts={}, &block)
          instance_eval &block if block
        end
      end
      @bang = Bang.new
    end

    should "overwrite the default dsl option in instance_eval" do
      assert_equal @bang.says, "vmrun"
      @bang = Bang.new do
        says "snake"
      end
      assert_equal @bang.says, "snake"
    end
  end
  
  
  context "with inheritance and classes" do
    before do
      class Pop
        include Dslify
        default_options :name => "pop", :flavor=>'cherry'
        def initialize(o={})
          set_vars_from_options(o)
        end
        
        def real_method
          "the real deal, no magic"
        end
      end
      
      class Foo < Pop
        default_options :name=>'fooey'
      end
      
      class Bar < Pop
        default_options :name=>'pangy', :taste => "spicy"
      end
      
      class Dad < Pop
      end
      
      class Grandad < Dad
      end
      
      class Defaults < Pop
        default_options(
          :global_default => "red_rum"
        )
      end
      
      @pop = Pop.new      
      @foo = Foo.new
      @bar = Bar.new
    end
    it "should take the default options set on the class" do
      assert_equal @pop.dsl_options[:name], "pop"
      assert_equal @pop.name, "pop"
    end
    it "should allow us to add defaults on the instance by calling dsl_options" do      
      # QuickieTest::Pop.name == "Cinnamon"
      @poptart = Pop.new :name => "Cinnamon"
      assert_equal @poptart.name, "Cinnamon"
    end
    it "should take the default options on a second class that inherits from the base" do
      assert_equal @foo.name, "fooey"
    end
    it "should take the default options on a third inheriting class" do
      assert_equal @bar.name, "pangy"
    end
    it "should not add a method not in the default_options" do
      assert_equal @bar.respond_to?(:boat), false      
    end
    it "should return the original default options test" do
      assert_equal @bar.dsl_options[:taste], "spicy"
      assert_equal @bar.dsl_options[:name], "pangy"
    end
    it "should set the default options of the child to the superclass's if it doesn't exist" do
      # QuickieTest::Dad => QuickieTest::Pop
      d = Dad.new
      assert Pop.new.name == 'pop'
      assert_equal "pop", d.name
      d.name "Frankenstein"
      assert_equal d.name, "Frankenstein"
    end
    it "should raise if the method isn't found on itself, the parent or in the rest of the method missing chain" do
      assert_raise NoMethodError do
        Class.new.sanitorium
      end
    end
    it "should be able to reach the grandparent through the chain of dsify-ed classes" do
      # QuickieTest::Grandad => QuickieTest::Dad => QuickieTest::Pop
      assert Dad.method_defined?(:name)
      assert Dad.new.flavor == 'cherry'
      assert Grandad.new.name, "pop"
    end
    it "should grab the default options from the dsl options (instance method)" do
      d = Dad.new(:star => "bucks")
      assert_equal d.default_options.keys.map {|k| k.to_s }.sort, %w(flavor name)
    end
  end
  context "methods" do
    setup do
      class MrDanger
        include Dslify
        default_options :where_to => "The Grand Canyon"
        def initialize(o={})
          set_vars_from_options(o)
        end
        def where_to(*a)
          "New York City"
        end
      end
    end

    should "not override the method where_to" do
      assert_equal MrDanger.new.where_to, "New York City"
    end
    should "not override the method where_to when called with set_vars_from_options" do
      assert_equal MrDanger.new(:where_to => "Bank of America").where_to, "New York City"
    end
  end
  
  context "when calling with a block" do
    setup do
      class ToddTheSquare
        include Dslify
        default_options :provider => :vmrun, :t => :nothing
        
        def provider(&block)
          instance_eval &block if block
        end
      end
    end
    
    should "should not evaluate the block" do
      tts = ToddTheSquare.new
      assert_equal tts.t, :nothing
    end

    should "should evaluate the block" do
      tts = ToddTheSquare.new
      tts.provider do
        self.t = :something
      end
      assert_equal tts.t, :something
    end
  end
  
  context "set_vars_from_options" do
    setup do
      class VarrrrrrrrMatey
        include Dslify
        default_options :say => "hello", :to => "world"
        
        def initialize(o={}, &block)
          set_vars_from_options(o)
          instance_eval &block if block
        end
        
        def to_s
          say + " " + to
        end
      end
    end

    should "set the vars on the options with no options" do
      assert_equal VarrrrrrrrMatey.new.to_s, "hello world"
    end
    should "update the options if called with options" do
      assert_equal VarrrrrrrrMatey.new({:say => "goodbye"}).to_s, "goodbye world"
    end
    should "update the options if called with block" do
      @v = VarrrrrrrrMatey.new do
        to "me"
      end
      assert_equal @v.to_s, "hello me"
    end
  end
  
  context "dsl_methods and methods on the object" do
    setup do
      class PluginMoopieMoop
        include Dslify
        default_options :stars => to_s
        
        def self.inherited(s)
          # stuff
          super
        end
      end
      class DoopyDoop < PluginMoopieMoop        
        def stars
          "right now"
        end
      end
      @obj = DoopyDoop.new
    end

    should "have the method :stars" do
      assert @obj.respond_to?(:stars)
    end
    should "use the method on the instance" do
      assert_equal @obj.stars, "right now"
    end
  end
  
  
  
end