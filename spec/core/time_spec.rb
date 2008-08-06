require File.dirname(__FILE__) + '/../spec_helper'

describe "Time" do
  describe "parsing" do
    it "should be able to parse seconds" do
      10.seconds.should == 10
    end
    it "should be able to parse minutes" do
      80.minutes.should == 4800
    end
    it "should be able to parse hours" do
      2.hours.should == 7200
    end
    it "should be able to parse days" do
      1.days.should == 86400
    end
    it "should be able to parse weeks" do
      2.weeks.should == 1209600
    end
  end
  describe "from" do
    it "should be able to find minutes ago" do
      10.minutes.ago.to_s.should == (Time.new - 10.minutes).to_s
    end
    it "should be able to find from now" do
      10.minutes.from_now.to_s.should == (Time.now + 10.minutes).to_s
    end
  end
end