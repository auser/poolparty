=begin rdoc
  This is responsible for creating the dna.json 
  for chef
    
  Usage:
    Baker::Json.new :name => "joe" do
      set :occupation, "bookie"
      wife = "Joe Anne"
    end
=end
module Baker
  class Jsoner < Base
    
    attr_reader :attributes
    
    def initialize(hsh_of_attributes={}, &block)
      attributes.merge! hsh_of_attributes
      instance_eval &block if block
    end
    
    def compile(json_name="dna")
      json_output = attributes.to_json
      dir = "#{cookbook_directory}/json"
      ::FileUtils.mkdir_p dir unless ::File.directory?(dir)
      File.open("#{dir}/#{json_name}.json", "w") {|f| f << json_output}
    end
    
    def set(key, value)
      attributes[key] = value
    end
    
    def attributes
      @attributes ||= {}
    end
    
    def cookbook_directory
      attributes.has_key?(:cookbook_directory) ? attributes[:cookbook_directory] : super
    end
    
    def method_missing(m,*a,&block)
      key = m.to_s.gsub(/\=/, '').to_sym
      if a.size > 0
        attributes[key] = a.size == 1 ? a[0] : a
      elsif attributes.has_key?(key)
        attributes[key]
      else
        super
      end
    end
    
  end
end