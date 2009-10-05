require "test_helper"

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
  def setup
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
    
  def teardown
    FileUtils.rm_rf(PATH_TEST_ROOT)
  end

  def test_should_have_class_options_set
    assert_equal "templates", TestFile.searchable_paths_dir
    assert_equal ["templates"], TestFile.searchable_paths_dirs
    assert_equal ["clouds", "/"], TestFile2.searchable_paths_dirs
  end
    
  def test_should_be_able_to_find_a_template
    assert_not_nil @template.find_file("apache.conf")
    assert_equal PATH_ONE/:templates/'apache.conf', @template.find_file("apache.conf")
    assert_equal PATH_TWO/:templates/'mysql.conf', @template.find_file("mysql.conf")
  end

  def test_shouldnt_find_a_template_that_doesnt_exist
    assert_nil @template.find_file("post-office.conf")
  end

  def test_should_find_something_with_prepended_paths
    assert_equal PATH_ONE/'clouds.rb', @cloud.find_file("clouds.rb")
    assert_equal PATH_TWO/'swing.rb', @cloud.find_file("swing.rb")
  end

  def test_should_find_things_in_the_right_order
    assert_equal PATH_ONE/'clouds.rb', @cloud.find_file("clouds.rb")
  end

  def test_should_look_in_the_additional_search_paths_first
    FileUtils.mkdir_p(PATH_ONE/'extra')                     
    File.write_to_file(PATH_ONE/'extra'/'clouds.rb')
    assert_equal @cloud.find_file("clouds.rb", [PATH_ONE/'extra']), PATH_ONE/'extra'/'clouds.rb'
  end
  
end