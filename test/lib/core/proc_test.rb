require "#{File.dirname(__FILE__)}/../../test_helper"

class ProcTest < Test::Unit::TestCase
  context "Proc" do
    setup do
      @this_proc = Proc.new do
        puts "hello world"
      end
    end
    should "have a proc_info" do
      assert_equal [File.expand_path(__FILE__), 6], @this_proc.proc_info
    end
    should "have the source available for the proc" do
      assert_equal open(__FILE__).read.split("\n"), @this_proc.source(__FILE__, 0)
    end
    should "be able to grab the source of the block" do
      assert_equal '        puts "hello world"', @this_proc.code
    end
    should "be able to grab the source of a block in an external file" do
      f = fixtures_dir/"resources"/"random_proc_file.rb"
      instance_eval open(f).read, f
      assert_equal "  @str = \"I have a bee\"", @b.code
    end
  end
  
end