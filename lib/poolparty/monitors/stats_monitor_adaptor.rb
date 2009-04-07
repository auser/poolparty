require ::File.dirname(__FILE__)+"/../aska/aska.rb"
require ::File.dirname(__FILE__)+"/../../poolparty/lite.rb"

module Butterfly
  class StatsMonitorAdaptor < AdaptorBase
    attr_reader :stats
    
    def initialize(o={})
      super
      @cloud = JSON.parse( open(o[:clouds_json_file]).read ) rescue {"options" => 
                                                                      {"rules" => {"expand" => PoolParty::Default.expand_when, 
                                                                                    "contract" => PoolParty::Default.contract_when
                                                                                  }}}
      # Our cloud.options.rules looks like
      #  {"expand_when" => "load > 0.9", "contract_when" => "load < 0.4"}
      # We set these as rules on ourselves so we can use aska to parse the rules
      # So later, we can call vote_rules on ourself and we'll get back Aska::Rule(s)
      # which we'll call valid_rule? for each Rule and return the result
      @cloud["options"]["rules"].each do |name,rul|
        r = Aska::Rule.new(rul)
        rule(name) << r
      end
    end
    
    def get(req, resp)
      begin
        if !req.params || req.params.empty?
          default_stats
        else
          stats[req.params[0].to_sym] ||= self.send(req.params[0])
          stats[req.params[0].to_sym]
        end
      rescue Exception => e
        resp.fail!
        "Error: #{e}"
      end 
    end
    
    def put(req, resp)
      if d = JSON.parse(req.post_content)
        stats.merge!(d)
        handle_election
      else
        "boom"
      end
    end
    
    # Handle the elections
    def handle_election
      # Ballots look like:
      # host => ["contract"]
      candidates = {:expand => 0, :contract => 0}
      candidates.each do |action, ballots|
        stats.each do |ip, node_hsh|
          candidates[action]+=1 if node_hsh["nominations"] && node_hsh["nominations"].include?(action.to_s)
        end
      end
      # TODO: Move?
      # Expand the cloud if 50+% of the votes are for expansion
      # Contract the cloud if 51+% of the votes are for contraction
      %x["server-expand-cloud"] if (candidates[:expand] - candidates[:contract])/stats.keys.size > 0.5
      %x["server-contract-cloud"] if (candidates[:contract] - candidates[:expand])/stats.keys.size > 0.5
    end
    
    def rules
      @rules ||= {}
    end
    
    def rule(name)
      rules[name] ||= []
    end
    
    def default_stats
      %w(load nominations).each do |var|
        stats[my_ip][var] ||= self.send(var.to_sym)
      end
      stats
    end

    def stats
      @stats ||= {my_ip  => {}}
    end
    
    def load
      %x{"uptime"}.split[-3].to_f
    end
    
    def instances
      # res = PoolParty::Neighborhoods.load_default.instances
      res ||= %x{"server-list-active name"}.split(" ")
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
      load = stats[my_ip]["load"] ||= self.send(:load)
      stats[my_ip]["nominations"] ||= rules.collect do |k,cld_rules|
        t = cld_rules.collect do |r|
          # If the comparison works
          if self.send(r.key.to_sym).to_f.send(r.comparison, r.var.to_f)
            # if we are facing an expansion rule
            if k =~ /expand/
              k if can_expand?
            # if we are facing a contraction rule
            elsif k =~ /contract/
              k if can_contract?
            else
              k
            end
          end
        end.compact
      end.flatten.compact
    end
    
    def my_ip
      @my_ip ||= ohai["ipaddress"]
    end
    
    def ohai
      @ohai ||= JSON.parse(%x["ohai"])
    end
  
    def reload_data!
      super
      @stats = nil
    end
  end
end