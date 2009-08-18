require "#{File.dirname(__FILE__)}/../../test_helper"

class StringTest < Test::Unit::TestCase
  context "string" do
    
    should "replace with ^" do
      out = ":god bless :country" ^ {:god => "Budda", :country => "India"}
      assert_equal "Budda bless India", out
    end
    
    should "not explode with non-existant data in the hash (replace with whitespace)" do
      out = ":person has a girlfriend" ^ {:boss => "Budda"}
      assert_equal " has a girlfriend", out
    end
    
    should "camelcase properly" do
      assert_equal "DocRiver", "doc_river".camelcase
      assert_equal "AWholeLottaHotdogs", "a_whole_lotta_hotdogs".camelcase
      assert_equal "One", "one".camelcase
    end
    
    should "snake_case properly" do
      assert_equal "one", "one".snake_case
      assert_equal "plenty_of_moons", "PlentyOfMoons".snake_case
      assert_equal "girls_make_boys_make_girls", "GirlsMakeBoysMakeGirls".snake_case
    end
    
    should "dasherize properly" do
      assert_equal "mini-coopers-rock", "MiniCoopersRock".dasherize
      assert_equal "only-the-best", "OnlyTheBest".dasherize
      assert_equal "one", "one".dasherize
    end
    
    should "classify properly" do
      assert_equal "ABird", "a_bird".classify
      assert_equal "Macguyver", "macguyver".classify
      assert_equal "RiceAndBeans", "rice_and_beans".classify
      assert_equal "Rice::And::Beans", "rice::and::beans".classify
      assert_equal "Pepper", "dr.pepper".classify
      assert_equal "Dr::Pepper", "dr::pepper".classify
    end
    
    should "to_hash from a string of = signs" do
      str =<<-EOE
        a = b
        b = c
        c = a
      EOE
      hsh = str.to_hash
      assert_equal Hash, hsh.class
      assert_equal hsh[:a], "b"
      assert_equal hsh[:b], "c"
      assert_equal hsh[:c], "a"
    end
    
    should "get the top level class" do
      assert_equal "pepper", "Dr::Pepper".top_level_class
      assert_equal "pepper_and_chives", "Dr::Pepper-And_chives".top_level_class
      assert_equal "rig", "Rig".top_level_class
    end
    
    should "have the / for filepaths" do
      assert_equal "/root/home/stuff", "/root"/"home"/"stuff"
      assert_equal "/root/box", "/root" / "box"
    end
    
    should "have pluralize (dumb)" do
      assert_equal "ferry".pluralize(2), "ferrys"
      assert_equal "boat".pluralize(2), "boats"
    end
  end
  
end