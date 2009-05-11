require File.dirname(__FILE__) + '/../spec_helper'
require File.dirname(__FILE__) + '/../../../lib/poolparty/helpers/optioner'

describe "Option Parser" do
  describe "options" do
    before(:each) do
      @op = PoolParty::Optioner.new([], {:abstract => true})
      @op.parse_options
    end
    it "should set the options as an Hash" do
      @op.options.class.should == Hash
    end
    it "should have the verbose option set to false by default" do
      @op.verbose?.should == false
    end
    it "should call a method called on it that is not defined on the options if they exist" do
      @op.dsl_options.should_receive(:[]).with(:verbose).at_least(1).and_return true
      @op.verbose
    end
    it "should exit after displaying the help message" do
      hide_output do
        lambda {
          @o = PoolParty::Optioner.new(["-h"], {:parse_options => false})
          @o.should_receive(:process_options).once
          @o.parse_options
        }
      end
    end
  end
  
  describe "parse_args" do
    before(:each) do
      @op = PoolParty::Optioner.new(%w( -v -i 1 five six -x), {:abstract => true, :parse_options => false})
    end
    it "should have an array of default boolean args" do
      @op.boolean_args.sort.should == ['--debug', '-V', '-h', '-t', '-v']
    end
    it "should split ARGV into flagged and unflagged arg arrays" do
      @op.unflagged_args.sort.sort.should ==  ["five", "six"]
      @op.flagged_args.should == ["-v", "-i", "1", "-x"]
    end
  end
  
  it "should be able to take a block and set some options on the block" do
    PoolParty::Optioner.new(["-w"], {:abstract => false, :load_pools => false}) do |opts, optioner|
      optioner.add_args :wee, :tea
      opts.on('-w', '--wee')    { optioner.wee = "wee" }
      opts.on('-t t', '--teatime tea')    { optioner.tea = "time" }
    end.wee.should == "wee"
  end
end