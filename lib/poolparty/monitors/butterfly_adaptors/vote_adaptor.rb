require "#{::File.dirname(__FILE__)}/../../aska/aska"

module Butterfly
  class VoteAdaptor < AdaptorBase
    
    attr_reader :data
    def initialize(o={})
      super
      @cloud = JSON.parse( open(o[:clouds_json_file]).read ) rescue {}
      # Our cloud.options.rules looks like
      #  {"expand_when" => "load > 0.9", "contract_when" => "load < 0.4"}
      # We set these as rules on ourselves so we can use aska to parse the rules
      # So later, we can call vote_rules on ourself and we'll get back Aska::Rule(s)
      # which we'll call valid_rule? for each Rule and return the result
      @cloud["options"]["rules"].each do |name,rule|
        r = Aska::Rule.new(rule)
        (rules[name] ||= []) << r
      end
    end
    def rules
      @rules ||= {}
    end
    def get(req, resp)
      begin
        nominations
      rescue Exception => e
        resp.fail!
        "Error: #{e}"
      end
    end
    
    private
    
    def nominations
      load = data[:load] ||= ServerMonitor.send(:load)
      data[:nominations] ||= rules.collect do |k,v|
        t = v.map do |r|
          ServerMonitor.send(r.key.to_sym).to_f.send(r.comparison, r.var.to_f) == true ? k : nil
        end.compact
        k unless t.empty?
      end
    end

    def data
      @data ||= reload_data!
    end
    def reload_data!
      super 
      @data = {}
    end
  end
end