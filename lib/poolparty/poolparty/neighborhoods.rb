require "#{::File.dirname(__FILE__)}/../modules/pinger"
require "#{::File.dirname(__FILE__)}/../schema"

module PoolParty
  class Neighborhoods
    include ::PoolParty::Pinger
    
    attr_reader :schema
    
    def initialize(data)
      raise Exception.new("You must pass a string or a hash to Neighborhoods") unless data
      parsed_data = case data
      when Array
        {:instances => data.map {|entry| disect(entry) }}
      when String
        {:instances => JSON.parse(data)}#.map "#{inst["instance_id"]}\t#{inst["ip"]}"}}
      end
      @schema = PoolParty::Schema.new(parsed_data)
      raise Exception.new("No instances found in the Neighborhoods schema") unless @schema.instances
    end
    
    def instances
      # puts "schema isntances #{@schema.instances.class} #{@schema.instances }"
      @instances ||= @schema.instances.map {|line| disect(line) }
    end
    
    def empty?
      instances.empty?
    end
    
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
    
    def self.load_default
      if ::File.file?("/etc/poolparty/neighborhood.json")
        new( open("/etc/poolparty/neighborhood.json").read )
      elsif ping_port("127.0.0.1", Default.butterfly_port, 1)# butterfly responding?
        require "open-uri"
        new( open("http://127.0.0.1:8642/neighborhood").read )
      else
        new("[]")
      end
    end
    
  end
end