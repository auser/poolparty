require "#{File.dirname(__FILE__)}/../test_helper"

class JsonerTest < Test::Unit::TestCase
  context "Jsoner" do

    should "set the hash to the attributes of the Jsoner object" do
      j = Baker::Jsoner.new :name => "frankovitz", :occupation => "car salesman"
      assert_equal j.attributes[:name], "frankovitz"
      assert_equal j.name, "frankovitz"
      assert_equal j.attributes[:occupation], "car salesman"
      assert_equal j.occupation, "car salesman"
    end
    
    should "set attributes when set in a block" do
      j = Baker::Jsoner.new do
        first_name "Bob"
        last_name "Schwartz"
      end
      assert_equal j.first_name, "Bob"
      assert_equal j.last_name, "Schwartz"      
    end
    
    should "set attributes when using set" do
      j = Baker::Jsoner.new do
        set :first_name, "Randy"
        set :last_name, "Pinz"
      end
      
      assert_equal j.first_name, "Randy"
      assert_equal j.last_name, "Pinz"
    end
    
    context "compile" do
      setup do
        @cookbook_directory = "#{File.dirname(__FILE__)}/../test_dir"
        @json = Baker::Jsoner.new :name => "Samantha", :occupation => "Customer Care", :cookbook_directory => @cookbook_directory do
          set :boyfriend, "Ari"
          cute = :yes
        end
      end
      
      teardown do
        FileUtils.rm_rf @cookbook_directory if File.directory?(@cookbook_directory)
      end

      should "create the directory" do        
        assert !File.directory?("#{@cookbook_directory}/json")
        @json.compile
        assert File.directory?("#{@cookbook_directory}/json")
      end
      
      should "create the content in the json directory" do
        @json.compile
        assert_equal @json.attributes.to_json, open("#{@cookbook_directory}/json/dna.json").read
      end
      
      should "create the file with the json name if given" do
        @json.compile("frank")
        assert_equal @json.attributes.to_json, open("#{@cookbook_directory}/json/frank.json").read
      end
    end
    
  end
  
end