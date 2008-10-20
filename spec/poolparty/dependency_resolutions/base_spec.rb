require File.dirname(__FILE__) + '/../spec_helper'

describe "Resolution spec" do
  before(:each) do
    @base = Class.new
    @base.extend(DependencyResolutions::Base)
  end
  %w(to_s to_string).each do |meth|
    eval "it 'should have the method #{meth} defined' do; @base.respond_to?(:#{meth}).should == true; end"
  end
end
