require "rubygems"
require "spec"
["core", "spec"].each do |dir|
  Dir[File.join(File.dirname(__FILE__), "poolparty", "spec", dir, "*.rb")].each {|f| require f}
end

class TestCloudClass < PoolParty::Cloud::Cloud  
  def build_test_manifest
    reset_resources!
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