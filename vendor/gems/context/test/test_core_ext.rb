require File.dirname(__FILE__) + '/test_helper.rb'

class TestCoreExt < Test::Unit::TestCase
  context "A string" do
    it "should be converted to method name" do
      assert :this_is_fun, "this is fun".to_method_name
    end
    
    it "should be downcased when converted" do
      assert :this_is_a_blast, "THIS is A BlASt".to_method_name
    end
    
    it "should change spaces to _" do
      assert :this_has_been_great, "This has been great".to_method_name
    end

    it "should change dangerous punctuation to _" do
      assert :no__really__this_was__good_, "No, really; this was #good!".to_method_name
    end
  end
end
