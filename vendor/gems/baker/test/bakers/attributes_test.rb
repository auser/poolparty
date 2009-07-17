require "#{File.dirname(__FILE__)}/../test_helper"

class MealTest < Test::Unit::TestCase
  context "template" do
    setup do
      @attr = Baker::Attributes.new :cookbook_directory => "#{File.dirname(__FILE__)}/../test_dir",
                                    :basename => "poolparty"
    end
    
    should "accept a hash of attributes" do
      swallow_output do
        assert_nothing_raised do
          @attr.variables :pig => "n`whistle"
        end
      end
    end
        
    should "store the attributes in the @attributes instance variable" do
      @attr.variables :plane => "Delta"
      assert_equal @attr.attributes, {:plane => "Delta"}
    end
    
    context "printing the variables" do
      setup do
        @cookbook_directory = "#{File.dirname(__FILE__)}/../test_dir"
        @attr.variables :box => "inside a box", :airline => "Delta", :quality => ["not so good", "fairly good"]
        @attr.send :print_variable, "carriage", @attr.attributes
      end
      
      teardown do
        FileUtils.rm_rf @cookbook_directory if File.directory?(@cookbook_directory)
      end

      should "create the template directory (since it doesn't exist)" do
        assert File.directory?("#{@cookbook_directory}/attributes")
        assert File.file?("#{@cookbook_directory}/attributes/carriage.rb")
      end
      
      should "set the content as attributes accessible by chef" do
        str = open("#{@cookbook_directory}/attributes/carriage.rb").read
        assert_match /poolparty Mash\.new unless attribute\?\('poolparty'\)/, str
        assert_match /poolparty\['carriage'\]\['quality'\] = \[ "not so good", "fairly good" \]/, str
        assert_match /poolparty\['carriage'\]\['airline'\] = "Delta"/, str
      end
      
    end
  end
  
end