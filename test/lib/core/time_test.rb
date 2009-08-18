require "#{File.dirname(__FILE__)}/../../test_helper"

class TimeTest < Test::Unit::TestCase
  context "Time" do
    context "parsing" do
      should "should be able to parse seconds" do
        10.seconds.should == 10
      end
      should "should be able to parse minutes" do
        80.minutes.should == 4800
      end
      should "should be able to parse hours" do
        2.hours.should == 7200
      end
      should "should be able to parse days" do
        1.days.should == 86400
      end
      should "should be able to parse weeks" do
        2.weeks.should == 1209600
      end
    end
    context "from" do
      should "should be able to find minutes ago" do
        10.minutes.ago.to_s.should == (Time.new - 10.minutes).to_s
      end
      should "should be able to find from now" do
        10.minutes.from_now.to_s.should == (Time.now + 10.minutes).to_s
      end
    end
    context "time_ago string" do
      should "should be able to turn 10.minutes.ago into a string" do
        10.minutes.time_ago.should == "10 minutes ago"
      end
      should "should be able to turn 30.seconds into Less than a minute ago" do
        30.seconds.time_ago.should == "Less than a minute ago"
      end
      should "should turn 1.year.ago into 1 year ago" do
        2.years.time_ago.should == "2 years ago"
      end
      should "should turn 35.days into 35 days ago" do
        29.days.time_ago.should == "4 weeks ago"
      end
      should "should turn 45.days into 1 month ago" do
        45.days.time_ago.should == "1 month ago"
      end
      should "should turn 70.days into 2 months ago" do
        70.days.time_ago.should == "2 months ago"
      end
    end
  end
end