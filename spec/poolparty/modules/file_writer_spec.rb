require File.dirname(__FILE__) + '/../spec_helper'

class TestClass
  include FileWriter
end
describe "FileWriter" do
  before(:each) do
    @test = TestClass.new
    @filepath = File.join("nilly.rb")
    @path = File.join(Base.storage_directory, @filepath)
  end
  %w(write_to_file_in_storage_directory copy_file_to_storage_directory write_to_temp_file).each do |method|
    eval <<-EOE
    it "should have a #{method} method" do
      @test.respond_to?(:#{method}).should == true
    end
    EOE
  end
  it "should copy the file to the Base.storage_directory when calling copy_file_to_storage_directory" do
    FileUtils.should_receive(:cp).with("ranger", Base.storage_directory+"/ranger").and_return true
    @test.copy_file_to_storage_directory("ranger")
  end
  describe "write to file in storage directory" do
    it "should try to create the directory if it doesn't exist" do
      FileTest.stub!(:directory?).and_return false
      ::File.stub!(:open).and_return true
      FileUtils.should_receive(:mkdir_p).with(::File.dirname(@path))
    end
    it "should call File.open on the file" do
      ::File.should_receive(:open).with(@path, "w+").and_return true
    end
    it "should call the block if it is given with a block" do
      block = Proc.new do |a|
        "meee: #{a.class}"
      end
      @test.write_to_file_in_storage_directory(@filepath, "STRING TO WRITE", &block)
    end
    it "should write the string in the file" do
      @test.write_to_file_in_storage_directory(@filepath, "STRING TO WRITE")
      open(::File.join( Base.storage_directory, @filepath)).read.should == "STRING TO WRITE"
    end
    after do
      @test.write_to_file_in_storage_directory(@filepath, "STRING TO WRITE")
    end
  end
  describe "trying to copy the same file" do
    before(:each) do
      @filepath = @path
    end
    it "should not try to copy a file if they are the same file" do
      FileUtils.should_not_receive(:cp)
      @test.copy_file_to_storage_directory(@filepath)
    end
  end
  after(:all) do
    ::File.unlink @path if ::File.file? @path
  end
end