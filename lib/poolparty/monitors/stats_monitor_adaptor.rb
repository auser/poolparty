require ::File.dirname(__FILE__)+"/../aska/aska.rb"

module Butterfly
  class StatsMonitorAdaptor < AdaptorBase
    attr_reader :stats
    
    puts "self = #{self}"
    
    def initialize(o={})
      super
      o[:clouds_json_file]='/Users/mfairchild/Code/poolparty/spec/poolparty/fixtures/clouds.json'
      
      @cloud = JSON.parse( open(o[:clouds_json_file]).read ) rescue {"options" => 
                                                                      {"rules" => {"expand" => PoolParty::Default.expand_when, 
                                                                                    "contract" => PoolParty::Default.contract_when
                                                                                  }}}
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
    
    def get(req, resp)
      begin
        if !req.params || req.params.empty?
          default_stats.to_json
        else
          stats[req.params[0]] ||= self.send(req.params[0])
          stats[req.params[0]].to_json
        end
      rescue Exception => e
        resp.fail!
        "Error: #{e}"
      end 
    end
    
    def rules
      @rules ||= {}
    end
    
    def default_stats
      %w(load).each do |var|
        stats["#{var}"] ||= self.send(var.to_sym)
      end
      puts "default stats =  #{stats.inspect}"
      stats
    end

    def stats
      @stats ||= {}
    end
    
    def load
      %x{"uptime"}.split[-3].to_f
    end
    
    def instances
      res = %x{"server-list-active names"}.split(" ") rescue 1
      res
    end
    
    def can_expand?
      instances.size < max_instances
    end
    
    def can_contract?
      instances.size > min_instances
    end
    
    def min_instances
      (@cloud["options"]["minimum_instances"] || PoolParty::Default.minimum_instances).to_i
    end
    
    def max_instances
      (@cloud["options"]["maximum_instances"] || PoolParty::Default.maximum_instances).to_i
    end
    
    def nominations
      load = stats[:load] ||= self.send(:load)
      stats[:nominations] ||= rules.collect do |k,cld_rules|
        t = cld_rules.collect do |r|
          self.send(r.key.to_sym).to_f.send(r.comparison, r.var.to_f) == true ? k : nil
        end.compact
        nil unless t.empty?
      end
    end
  
  end
end