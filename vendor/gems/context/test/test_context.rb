require File.dirname(__FILE__) + '/test_helper.rb'

class TestContext < Test::Unit::TestCase
  def test_can_write_tests_without_context
    assert true
  end
  
  def test_context_aliases
    [:context, :contexts, :describe, :describes, :group, :specify, :specifies].each do |method_alias|
      assert self.class.respond_to?(method_alias)
    end
  end
  
  context "A new context" do
    context "when not nested" do
      before do
        @context = Class.new(Test::Unit::TestCase).context("When testing") do
                    def test_this_thing
                      true
                    end
                  end
      end

      it "should set the context name" do
        assert_equal "When testing", @context.context_name
      end
      
      it "should be a Test::Unit::TestCase" do
        assert @context.ancestors.include?(Test::Unit::TestCase)
      end
    end
  
    context "when nested" do
      before do
        @context = self.class.context("and we're testing") do
          def self.nested
            @nested
          end
          
          @nested = context "should be nested" do
            def test_this_thing
              true
            end
          end
        end
      end
      
      it "should set a nested context's name" do
        assert_equal "A new context when nested and we're testing should be nested", @context.nested.context_name
      end
      
      it "should also be a Test::Unit::TestCase" do
        assert @context.nested.ancestors.include?(Test::Unit::TestCase)
      end
    end
  end
end
