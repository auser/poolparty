require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestChef < Test::Unit::TestCase
  context "json generation" do
    setup do
      reset!
      cloud :json_this_yo do
        chef do
          json
        end
      end
      @cloud = clouds[:json_this_yo]
    end
    
    # TODO: Solidify json generation
    should "include the json file" do
      assert ::Suitcase::Zipper.items.has_key?(:"__p__stringdna.json_chef")
      assert_equal ::Suitcase::Zipper.items[:"__p__stringdna.json_chef"][:content], '{"recipes":["poolparty"]}'
      ::Suitcase::Zipper.build_dir!("#{@cloud.tmp_path}/test_dependencies")
      assert_equal '{"recipes":["poolparty"]}', open("#{@cloud.tmp_path}/test_dependencies/chef/dna.json").read
    end
  end
end