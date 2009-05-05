require File.dirname(__FILE__) + '/../spec_helper'

PATH_TEST_ROOT = Default.storage_directory / "path_test"
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

describe "FileWriter" do
  before(:each) do
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

  describe "class methods" do
    it "should have class options set" do
      TestFile.searchable_paths_dir.should   == "templates"
      TestFile.searchable_paths_dirs.should  == ["templates"]
      TestFile2.searchable_paths_dirs.should == ["clouds", "/"]
    end
  end

  describe "#find_file" do
    it "should be able to find a template" do
      @template.find_file("apache.conf").should_not be_nil
      @template.find_file("apache.conf").should == PATH_ONE/:templates/'apache.conf'
      @template.find_file("mysql.conf").should == PATH_TWO/:templates/'mysql.conf'
    end

    it "shouldn't find a template that doesn't exist" do
      @template.find_file("post-office.conf").should be_nil
    end

    it "should find something with prepended paths" do
      @cloud.find_file("clouds.rb").should == PATH_ONE/'clouds.rb'
      @cloud.find_file("swing.rb").should == PATH_TWO/'swing.rb'
    end

    it "should find things in the right order" do
      @cloud.find_file("clouds.rb").should == PATH_ONE/'clouds.rb'
    end





  end

  after(:each) do
    FileUtils.rm_rf(PATH_TEST_ROOT)
  end
end
