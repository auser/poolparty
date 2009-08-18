require File.dirname(__FILE__) + '/test_helper.rb'

class TestTest < Test::Unit::TestCase
  def test_shared_aliases
    %w(shared_behavior share_as share_behavior_as shared_examples_for).each do |method_alias|
      assert self.class.respond_to?(method_alias.to_sym)
    end
  end
  
  def test_use_aliases
    %w(uses it_should_behave_like behaves_like uses_examples_from).each do |method_alias|
      assert self.class.respond_to?(method_alias.to_sym)
    end
  end
  
  context "A shared group" do
    context "creates a module" do
      test "based on a string name" do
        self.class.shared "things and fun" do
        end

        assert Object.const_get(:ThingsAndFun)
        assert_equal Context::SharedBehavior, Object.const_get(:ThingsAndFun).class
      end
    
      it "based on a symbol name" do
        self.class.shared :fun_and_games do
        end
      
        assert Object.const_get(:FunAndGames)
        assert_equal Context::SharedBehavior, Object.const_get(:FunAndGames).class
      end
    
      it "unless the name is not a String or Symbol" do
        assert_raise ArgumentError do
          self.class.shared StandardError do
          end
        end
      end
    end
  
    context "should be locatable" do      
      shared "hello sir" do
        def amazing!
          puts "back off!"
        end
      end
    
      it "by a symbol" do
        assert_nothing_raised do
          self.class.use :hello_sir
        end
      end
    
      shared "hello madam" do
        def fantastic!
          puts "you know me!"
        end
      end
    
      it "by a string" do
        assert_nothing_raised do
          self.class.use "hello madam"
        end
      end
    
      shared "hi dog" do
        def stupendous!
          puts "hoo hah!"
        end
      end
    
      it "by direct reference" do
        assert_nothing_raised do
          self.class.use HiDog
        end
      end
    end
  
    context "should include its shared behavior" do
      shared "Athos" do
        it "en_garde" do
          true
        end
      end
    
      it "by a symbol" do
        self.class.use :athos

        assert send("test: A shared group should include its shared behavior en_garde")
      end
    
      shared "Porthos" do
        def parry!
          true
        end
      end
    
      it "by a string" do
        self.class.use "Porthos"
        assert parry!
      end
    
      shared "Aramis" do
        def lunge!
          true
        end
      end
    
      it "by direct reference" do
        self.class.use Aramis
        assert lunge!
      end
    end
  end
end
