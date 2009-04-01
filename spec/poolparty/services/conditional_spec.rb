require File.dirname(__FILE__) + '/../spec_helper'

# STUB!
def add_service(arg)  
end

describe "Conditional" do
  it "not fail when calling case_of" do
    lambda {
      case_of "b" do
      end
    }.should_not raise_error
  end
  it "should put the case object on the case stack as a Conditional object" do
    c = case_of "b" do
    end
    c.attribute.should == "b"
  end
  it "should populate the when_statements when a when_is is called" do
    c = case_of "b" do
      when_is "b" do
      end
    end
    c.when_statements.size.should == 1
  end
  it "freeze the case statement when calling end_of" do
    c = case_of "b" do
      when_is "b" do
      end
    end
    c.options.frozen?.should == true
  end
  it "create a new service for the when_is block" do
    c = case_of "b" do
      when_is "b" do
        "I'm TOTALLY B!"
      end
    end
    c.when_statements[:b].is_a?(Service).should == true
  end
  it "create a new service for the when_is block" do
    c = case_of "b" do
      when_is "b" do
        "I'm TOTALLY B!"
      end
      when_is "c" do
        "You are not 'c', don't lie!"
      end
    end
    c.when_statements[:b].is_a?(Service).should == true
  end
end