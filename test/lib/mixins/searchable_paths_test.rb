require "#{File.dirname(__FILE__)}/../../test_helper"

PATH_TEST_ROOT = "/tmp/poolparty" / "path_test"
PATH_ONE       = PATH_TEST_ROOT / "one"
PATH_TWO       = PATH_TEST_ROOT / "two"

class TestFile
  include SearchablePaths
  has_searchable_paths(:dir => "templates", :paths => [PATH_ONE, PATH_TWO])
end

class TestFile2
  include SearchablePaths
  has_searchable_paths(:dirs => ["clouds", "/"], :prepend_paths => [PATH_ONE, PATH_TWO])
end

def File.write_to_file(name, content="")
  open(name, "w") {|f| f.print content}
end

class SearchablePathsTest < Test::Unit::TestCase
  context "searchable_paths" do
    setup do
      # create path one
      [PATH_ONE/:clouds, PATH_ONE/:templates, PATH_ONE/:foo].each {|dir| FileUtils.mkdir_p(dir) }
      [PATH_ONE/'clouds.rb', PATH_ONE/:clouds/'special.rb', PATH_ONE/:templates/'apache.conf'].each do |f|
        File.write_to_file(f)
      end

      # create path two
      [PATH_TWO/:clouds, PATH_TWO/:templates, PATH_TWO/:foo].each {|dir| FileUtils.mkdir_p(dir) }
      [PATH_TWO/'swing.rb', PATH_TWO/'clouds.rb', PATH_TWO/:clouds/'common.rb', PATH_TWO/:templates/'mysql.conf'].each do |f|
        File.write_to_file(f)
      end

      @template = TestFile.new
      @cloud    = TestFile2.new
    end
    
    teardown do
      FileUtils.rm_rf(PATH_TEST_ROOT)
    end

    should "should have class options set" do
      assert_equal "templates", TestFile.searchable_paths_dir
      assert_equal ["templates"], TestFile.searchable_paths_dirs
      assert_equal ["clouds", "/"], TestFile2.searchable_paths_dirs
    end
    
    should "should be able to find a template" do
      assert_not_nil @template.find_file("apache.conf")
      assert_equal PATH_ONE/:templates/'apache.conf', @template.find_file("apache.conf")
      assert_equal PATH_TWO/:templates/'mysql.conf', @template.find_file("mysql.conf")
    end

    should "shouldn't find a template that doesn't exist" do
      assert_nil @template.find_file("post-office.conf")
    end

    should "should find something with prepended paths" do
      assert_equal PATH_ONE/'clouds.rb', @cloud.find_file("clouds.rb")
      assert_equal PATH_TWO/'swing.rb', @cloud.find_file("swing.rb")
    end

    should "should find things in the right order" do
      assert_equal PATH_ONE/'clouds.rb', @cloud.find_file("clouds.rb")
    end
    
    context "search_in_known_locations" do
      setup do
        FileUtils.mkdir_p(PATH_ONE/'extra')                     
        File.write_to_file(PATH_ONE/'extra'/'clouds.rb')
      end

      should "should look in the additional search paths first" do
        @cloud.find_file("clouds.rb", [PATH_ONE/'extra']).should == PATH_ONE/'extra'/'clouds.rb'
      end
    end
    

  end
  
end