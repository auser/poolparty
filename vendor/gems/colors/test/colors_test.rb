require File.dirname(__FILE__)+'/test_helper'

class ColorsTest < Test::Unit::TestCase
  context "colors" do
    setup do
      @data = [
        "<red bg=blue>Red text on blue background</red>",
        "<red>Red text</red>",
        "<blue bg=yellow>Blue text on yellow bg</blue>",
        "<b>BOLD</b>",
        "<blue>Blue</blue>, <red>red</red> and <yellow>BOLD</yellow>",
        "<purple bg=red>Purple is covered</purple> <blue>too!</blue>"
      ]      
    end

    should "substitute tags for each line" do
      assert_equal "\e[1m\e[44m\e[1m\e[31mRed text on blue background\e[0m\e[37m\e[40m\e[0m\e[37m\e[40m", Colors.process(@data[0])
      assert_equal "\e[1m\e[31mRed text\e[0m\e[37m\e[40m", Colors.process(@data[1])
      assert_equal "\e[1m\e[43m\e[1m\e[34mBlue text on yellow bg\e[0m\e[37m\e[40m\e[0m\e[37m\e[40m", Colors.process(@data[2])
      assert_equal "\e[1mBOLD\e[0m\e[37m\e[40m", Colors.process(@data[3])
      assert_equal "\e[1m\e[34mBlue\e[0m\e[37m\e[40m, \e[1m\e[31mred\e[0m\e[37m\e[40m and \e[1m\e[33mBOLD\e[0m\e[37m\e[40m", Colors.process(@data[4])
      assert_equal "\e[1m\e[41m\e[1m\e[35mPurple is covered\e[0m\e[37m\e[40m\e[0m\e[37m\e[40m \e[1m\e[34mtoo!\e[0m\e[37m\e[40m", Colors.process(@data[5])
    end
  end
  
end
