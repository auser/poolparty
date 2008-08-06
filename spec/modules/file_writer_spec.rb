require File.dirname(__FILE__) + '/../spec_helper'

class TestClass
  include FileWriter
end
describe "FileWriter" do
  before(:each) do
    # Application.reset!
    @instance = Class.new
    @instance.stub!(:name).and_return "node0"
    @instance.stub!(:ip).and_return "127.0.0.1"
    @test = TestClass.new
    @test.stub!(:base_tmp_dir).and_return("tmp")
  end
  describe "writing to the temp directory with a string" do
    before(:each) do
      @test.write_to_file_for("haproxy", @instance, "topical")
      @outfile = "#{@test.base_tmp_dir}/node0-haproxy"
    end
    after(:each) do
      FileUtils.rm @outfile
    end
    it "should be able to write a file to the tmp directory with a string" do      
      File.file?(@outfile).should == true
    end
    it "should be able to write to the file with the string" do
      open(@outfile).read.should == "topical"
    end
  end
  
  describe "writing to the temp directory with a block" do
    before(:each) do
      @test.write_to_file_for("haproxy", @instance) do
        "Hello topical"
      end
      @outfile = "#{@test.base_tmp_dir}/node0-haproxy"
    end
    after(:each) do
      FileUtils.rm @outfile
    end
    it "should be able to write a file to the tmp directory with a string" do      
      File.file?(@outfile).should == true
    end
    it "should be able to write to the file with the string" do
      open(@outfile).read.should == "Hello topical"
    end
    describe "for output of block" do
      before(:each) do
        @test.write_to_file_for("haproxy", @instance) do
          "Fix and me"
          "Hello topical"
        end
      end
      it "should just write the final output of the block to the file" do
        open(@outfile).read.should == "Hello topical"
      end
    end
  end
  
  describe "without a node" do
    before(:each) do
      @test.write_to_file_for("haproxy") do
        "Hello topical"
      end
      @outfile = "#{@test.base_tmp_dir}/haproxy"
    end
    after(:each) do
      FileUtils.rm @outfile
    end
    it "should write to the master file without a node name" do
      File.file?(@outfile).should == true
    end
  end
  
  after(:all) do
    FileUtils.rm_r "tmp"
  end
end