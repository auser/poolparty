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
      @cloud["options"]["rules"].each do |name, rul|
        r = Aska::Rule.new(rul)
        rule(name) << r
      end
      fork_and_put
    end
    
    #TODO: first packet should be a post
    def first_put(time_to_wait=60)
      puts " waiting #{time_to_wait} seconds for a put, otherwise initiating. #{stats.inspect}"
      sleep time_to_wait  #lets see if we receive a stats update before puting a new one
      if stats=={my_ip  => {}}
        puts "Initiating first put"
        touch ''
        fork_and_put
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
        hsh = d.reject {|ip, _node| ip == my_ip }
        stats.merge!(hsh)
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
      if (candidates[:expand] - candidates[:contract])/stats.keys.size > 0.5
        %x[/usr/bin/server-cloud-elections expand] unless elected_action == "expand"
        @elected_action = "expand"
      elsif (candidates[:contract] - candidates[:expand])/stats.keys.size > 0.5
        %x[/usr/bin/server-cloud-elections contract] unless elected_action == "contract"
        @elected_action = "contract"
      end      
      
      reload_data!
      stats[my_ip]["elected_action"] = @elected_action if @elected_action
      
      fork_and_put
      "ok"
    end
    
    def fork_and_put
      fork do
        # put to next node
        # TODO: Fix mysterious return of the nil (HASH next_sorted_key(my_ip))
        # next_node = stats.next_sorted_key(my_ip)        
        idx = (stats.size - stats.keys.sort.index(my_ip))
        next_node = stats.keys.sort[idx - 1]
        sleep(10)
        Net::HTTP.start(next_node, PoolParty::Default.butterfly_port) do |http|
          http.send_request('PUT', '/stats_monitor.json', stats.to_json)
        end
      end
    end
    
    def elected_action
      @elected_action ||= nil
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
      res ||= %x[/usr/bin/server-list-active internal_ip].split("\t")
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
      @ohai ||= JSON.parse(%x[ohai])
    end
  
    def reload_data!
      super
      @stats[my_ip] = {}
      instances.each {|inst| @stats[inst] ||= {} }
    end
  end
end