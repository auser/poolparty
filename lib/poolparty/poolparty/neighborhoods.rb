=begin rdoc
  Neighborhood
  
  Neighborhoods describes the neighborhood that an instance is in.
  Neighborhoods are instances that are "near" other instances, either by 
  association of instantiation or through monitor_rack.   
=end
require "#{::File.dirname(__FILE__)}/../modules/pinger"
require "#{::File.dirname(__FILE__)}/../schema"

module PoolParty
  class Neighborhoods
    include ::PoolParty::Pinger
    attr_reader :schema
    
    # Create a neighborhood from a string, array or hash given.
    def initialize(data)
      raise Exception.new("You must pass a string or a hash to Neighborhoods") unless data
      parsed_data = case data
      when Array
        {:instances => data.map {|entry| disect(entry) }}
      when String
        {:instances => JSON.parse(data)}#.map "#{inst["instance_id"]}\t#{inst["ip"]}"}}
      when Hash
        data
      end
      @schema = PoolParty::Schema.new(parsed_data)
      raise Exception.new("No instances found in the Neighborhoods schema") unless @schema.instances
    end
    
    # Get the known instances from the neighborhood.json file on the server
    def instances
      @instances ||= @schema.to_hash[:instances] rescue @schema.instances.collect {|line| disect(line) }
    end
    
    # Returns empty if the neighborhood has no instances
    def empty?
      instances.empty?
    end
    
    # Get the next node in the hash
    def next_node(node_hash)
      return nil if empty?
      sort.wrapping_next(node_hash)
    end
    
    # Sort the instances by ip string, a very basic sort
    def sort
      instances.sort {|a, b| a.ip <=> b.ip}
    end
    
    # Get the instances at the specific index of the neighborhood
    def [](at)
      instances[at] if at >= 0 && at < instances.size
    end
    
    def disect(line)
      case line
      when String
        arr = line.split("\t")
        {:instance_id => arr[0], :ip => arr[1]}
      else
        line
      end
    end
    
    # Run through an enumeration of the instances
    def each(&block)
      instances.each &block
    end
    
    # TODO: Make this into something useful
    def clump(filepath=nil)
      out = instances.to_json
      ::File.open(filepath, "w") {|f| f << out } if filepath
      out
    end
    
    def self.clump(json, filepath=nil)
      new(json).clump(filepath)
    end
    
    # Load the default neighborhood.json file
    # If the neighborhood.json file exists in 
    #   /etc/poolparty/neighborhood.json
    # then load the neighborhood from the file, otherwise
    # if there is a butterfly server running locally,
    # query it for the current neighborhood.
    # Finally, return an empty set of instances
    def self.load_default
      if ::File.file?("/etc/poolparty/neighborhood.json")
        new( open("/etc/poolparty/neighborhood.json").read )
      elsif ping_port("127.0.0.1", Default.butterfly_port, 1)# butterfly responding?
        require "open-uri"
        begin
          timeout(2) do
            new( open("http://127.0.0.1:8642/neighborhood").read )
          end
        rescue TimeoutError => e
          require "#{::File.dirname(__FILE__)}/../../poolparty"
          cld = ::PoolParty::Cloud::Cloud.load_from_json(open("/etc/poolparty/clouds.json").read)          
          nodes = cld.nodes({:status => "running"}, false)
          data = nodes.map {|hsh| hsh.reject {|k,v| v.nil? }}.map {|a| a.merge(:launching_time => a[:launching_time].to_s) }
          ::File.open("/etc/poolparty/neighborhood.json", "w") {|f| f << "{\"instances\":#{data.to_json}}" }
          new(data)
        end
      else
        new("[]")
      end
    end
    
  end
end