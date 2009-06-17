module Monitors
  class Elections < BaseMonitor
    
    def get(data=nil)
      'hello'
    end
    
    def count_ballots(candidates={:expand => 0, :contract => 0}, ballots={})
      # Ballots look like:
      # {host => ["contract"]}
      # Count the number of nominations for each candidate action
      candidates.each do |action, ballots|
         stats.each do |ip, node_hsh|
           if node_hsh["nominations"] && node_hsh["nominations"].include?(action.to_s)
             candidates[action]+=1
           end
         end
       end
      candidates
    end
    
    # Handle the elections
    def handle_election(ballots={})
      count_votes(ballots)
      # Expand the cloud if 50+% of the votes are for expansion
      # Contract the cloud if 51+% of the votes are for contraction
      # Check to make sure an elected action is not already in progress
      if (candidates[:expand] - candidates[:contract])/stats.keys.size > 0.5
        %x[server-cloud-elections expand] unless elected_action == "expand"
        @elected_action = "expand"
      elsif (candidates[:contract] - candidates[:expand])/stats.keys.size > 0.5
        %x[server-cloud-elections contract] unless elected_action == "contract"
        @elected_action = "contract"
      end
      
      reload_data!
      stats[my_ip]["elected_action"] = @elected_action if @elected_action
      log << "#{Time.now.strftime("%Y-%m-%d-%H-%M")}, #{stats.to_json}\n"
      stats.to_json
    end

    def  my_cloud
      require '/etc/poolparty/clouds.rb'
      name = open("/etc/poolparty/cloudname").read
      clouds[name.to_sym]
    end
  
  end
end