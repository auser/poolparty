require File.dirname(__FILE__) + '/../spec_helper'

class TestClass
  include FileWriter
end
describe "FileWriter" do
  before(:each) do
    @test = TestClass.new
  end
  %w(write_to_file_in_storage_directory copy_file_to_storage_directory write_to_temp_file).each do |method|
    eval <<-EOE
    it "should have a #{method} method" do
      @test.respond_to?(:#{method}).should == true
    end
    EOE
  end
  it "should copy the file to the Base.storage_directory when calling copy_file_to_storage_directory" do
    FileUtils.should_receive(:cp).with("ranger", Base.storage_directory).and_return true
    @test.copy_file_to_storage_directory("ranger")
  end
  describe "write to file in storage directory" do
    before(:each) do      
      @filepath = File.join("willy", "nilly.rb")
      @path = File.join(Base.storage_directory, @filepath)
    end
    it "should try to create the directory if it doesn't exist" do
      ::File.stub!(:open).and_return true
      File.stub!(:directory?).with(::File.dirname(@path)).and_return false
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
    after do
      @test.write_to_file_in_storage_directory(@filepath, "STRING TO WRITE")
    end
  end
end