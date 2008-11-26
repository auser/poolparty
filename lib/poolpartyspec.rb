require "rubygems"
require "spec"

@basestr = open("#{::File.dirname(__FILE__)}/poolparty/spec/have_base.rb").read

PoolParty::Resources::Resource.available_resources.each do |ty|
  ty.downcase!
  str = @basestr ^ {:classname => "Have#{ty.capitalize}", 
                    :type => ty,
                    :includer => "SpecExtensions::Have#{ty.capitalize}.new(name, extra)"}
  eval str
end

module PoolPartySpecHelper    
end

class TestCloudClass < PoolParty::Cloud::Cloud
  def initialize(name,opts={}, &block)
    reset_resources!
    super
  end
  def build_test_manifest
    realize_plugins!(true)
    
    returning Array.new do |arr|
      
      services.each do |service|
        service.options.merge!(:name => service.name)
        classpackage_with_self(service)
      end
      
      global_classpackages.each do |cls|
        arr << cls.to_string
      end
      
    end.join("\n")
  end
end

def new_test_cloud(&block)  
  TestCloudClass.new(:test_cloud, &block)
end