require "#{::File.dirname(__FILE__)}/../schema"

module PoolParty
  class Neighborhoods
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
      def_file = [
        Dir.pwd,
        Default.base_config_directory,
        Default.remote_storage_path,
        Default.poolparty_home_path
      ].select do |dir|
        filepath = ::File.expand_path("#{dir}/neighborhood.json")
        filepath if ::File.file?(filepath)
      end.first || nil
      def_file ? new( open(::File.expand_path("#{def_file}/neighborhood.json")).read ) : new('[]')
    end
    
  end
end