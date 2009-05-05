require File.dirname(__FILE__) + '/../spec_helper'

PATH_TEST = Default.storage_directory / "path_test"

class TestFile
  include SearchablePaths
  has_searchable_paths(:dir => "templates")
end

class TestFile2
  include SearchablePaths
  has_searchable_paths(:dirs => ["clouds", "/"])
end

def File.write_to_file(name, content="")
  open(name, "w") {|f| f.print content}
end

describe "FileWriter" do
  before(:each) do
    [PATH_TEST/:clouds, PATH_TEST/:templates, PATH_TEST/:foo].each {|dir| FileUtils.mkdir_p(dir) }
    [PATH_TEST/'clouds.rb', PATH_TEST/:clouds/'special.rb', PATH_TEST/:templates/'apache.conf'].each do |f|
      File.write_to_file(f)
    end
  end

  describe "class methods" do
    it "should have class options set" do
      TestFile.searchable_paths_dir.should   == "templates"
      TestFile.searchable_paths_dirs.should  == ["templates"]
      TestFile2.searchable_paths_dirs.should == ["clouds", "/"]
    end
  end

  after(:each) do
    FileUtils.rm_rf(PATH_TEST)
  end
end
