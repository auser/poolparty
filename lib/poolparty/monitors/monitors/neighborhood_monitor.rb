require ::File.dirname(__FILE__)+"/../../core/hash.rb"
require ::File.dirname(__FILE__)+"/../../core/array.rb"
require "#{::File.dirname(__FILE__)}/../../poolparty/neighborhoods"

module Monitors
  
  class Neighborhood
    attr_reader :stats, :request
    attr_accessor :response
    
    def get(data=nil)
      neighborhood
    end
    
    def put(data)
      @neighborhood = merge_array_of_hashes_with_key(neighborhood, JSON.parse(data), 'ip')
      save
    end
    
    def post(data)
      received = JSON.parse(data)
      @neighborhood = JSON.parse(data)
      save
    end
    
    private
    def neighborhood
      @neighborhood ||= ::PoolParty::Neighborhoods.load_default.instances  #rescue [{"instance_id"=>"/Users/mfM.vmx", "ip"=>"172.16.68.128"}, {"instance_id"=>"/Usvm/Ubuntu64bitVM.vmx", "ip"=>"172.16.68.130"}]
    end
    
    def save(filepath='/etc/poolparty/neighborhood.json')
      puts "\n---\n#{neighborhood.to_json}\n---\n"
      ::File.open(filepath, "w") {|f| f << neighborhood.to_json }
      neighborhood
    end
    
    # Take two arrays of hashes and merge them based on a common key.  
    # For example:
    # a1=[{:a=>'a'}, {'tt'=>5}]
    # a2= [{:a=>'a', :new=>'stuff'}, {'c'=>'C'}]
    # will merge to: [{:a=>"a", :new=>"stuff"}, {"tt"=>5}, {"c"=>"C"}]
    def merge_array_of_hashes_with_key(arr1, arr2, key)
      ar1=arr1.dup
      ar2= arr2.dup
      first_pass = ar1.collect{|h1|
        found = ar2.detect{|hsh|
           hsh.has_key?(key) && hsh[key]==h1[key] && ar2.delete_at(ar2.index(hsh))
        }
        found ? found : h1
      }
      first_pass + ar2
    end
    
  end
end