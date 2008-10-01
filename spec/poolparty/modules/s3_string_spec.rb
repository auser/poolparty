require File.dirname(__FILE__) + '/../spec_helper'

describe "S3 String" do
  before(:each) do
    @str = "testbucketstring"
  end
  %w(bucket_objects bucket_object bucket_object_exists? 
      store_bucket_value delete_bucket_value bucket_exists? delete_bucket).each do |meth|
    eval <<-EOE
      it "should have the method #{meth}" do
        @str.respond_to?(:#{meth}).should == true
      end
    EOE
  end
end