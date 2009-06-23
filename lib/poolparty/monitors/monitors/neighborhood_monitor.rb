require ::File.dirname(__FILE__)+"/../../core/hash.rb"
require ::File.dirname(__FILE__)+"/../../core/array.rb"
require "#{::File.dirname(__FILE__)}/../../poolparty/neighborhoods"
require 'rest_client'

module Monitors
  
  class Neighborhood < BaseMonitor
    
    def get(data=nil)
      reset!
      neighborhood      
    end
    
    def put(data, from=nil)
      @neighborhood_instances = merge_array_of_hashes_with_key(neighborhood.instances, JSON.parse(data), 'ip')
      @neighborhood = {@neighborhood[:instances] => @neighborhood_instances}
      # after_close do
      #   if @neighborhood.instances.size>1
      #     # TODO: Add logger here
      #     puts "Pinging #{"#{@neighborhood.instances.rand.ip}/neighborhood"}"
      #     RestClient.put "#{@neighborhood.instances.rand.ip}/neighborhood", @neighborhood, :content_type => 'text/x-json'
      #   end
      # end
      save
    end
    
    def post(data)
      received = JSON.parse(data)
      @neighborhood = JSON.parse(data)
      save
    end
    
    private
    def myself
      @myself ||= @neighborhood.instance.select_with_hash('ip'=>@env['REQUEST_IP'])
    end
    
    def neighborhood
      @neighborhood ||= {        
        :instances => instances,
        :stats => stats
      } #rescue [{"instance_id"=>"1000", "ip"=>"172.16.68.128"}, {"instance_id"=>"456", "ip"=>"172.16.68.130"}]
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
    
    # HELPERS
    def instances(_n=nil)
      @instances ||= ::PoolParty::Neighborhoods.load_default.instances #.to_hash #[:instances]
    end
    def stats(_n=nil)
      @stats ||= ::PoolParty::Neighborhoods.load_default.stats rescue Stats.new(@env).get
    end
    def reset!
      @neighborhood = @stats = nil
    end
  end
end