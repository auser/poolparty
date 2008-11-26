$:.unshift(File.join(File.dirname(__FILE__), "poolparty", "spec"))
require "rubygems"
require "spec"
require "ensure_matchers_exist"

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

def load_test_cloud(name, poolfile=nil)  
  PoolParty::Script.inflate_file poolfile
  blk = cloud(name).stored_block.dup
  PoolParty::Pool.reset!
  new_test_cloud &blk
end