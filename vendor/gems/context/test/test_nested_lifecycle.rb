require File.dirname(__FILE__) + '/test_helper.rb'

class TestNestedLifecycle < Test::Unit::TestCase
  before :all  do
    @var = 0
  end
  
  before do
    @var += 1
  end
  context "A new context" do
    before do
      @var += 1
    end
    
    before :all do
      @var = 0
    end
    
    context "A nested context" do
      before do
        @var += 1
      end
      
      before :all do
        @var += 1
      end

      context "A second, nested context" do
        before do
          @var += 1
        end

        before :all do
          @var += 1
        end

        it "should set var" do
          assert_equal 6, @var
        end
      end
    end
  end
end
