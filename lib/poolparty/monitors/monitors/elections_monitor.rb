module Monitors
  class Elections < BaseMonitor
    
    # these are the rules that define the 
    # elected actions
    def self.candidates
      {
        :expand => :contract,
        :contract => :expand,
        :configure => nil,
        :none => nil
      }
    end
    
    def get(data=nil)
      'hello'
    end
    
    def put(data=nil)
      elections = JSON.parse(data)
      log "Received #{elections.histogram.inspect} in Elections Monitor"
      handle_election(elections.histogram)
    end
        
    def count_ballots(ballots={}, candidates={:expand => 0, :contract => 0})
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
    # ballots: {"contract" => 1, "expand" => 4}
    def handle_election(ballots={})      
      # Expand the cloud if 50+% of the votes are for expansion
      # Contract the cloud if 51+% of the votes are for contraction
      # Check to make sure an elected action is not already in progress      
      log "handle_election: #{ballots.inspect}"
      ballots.symbolize_keys!
      total_votes = ballots.inject(0) {|total, arr| total += arr[1] }
            
      ballots.each do |ballot, pro_votes|        
        contra = self.class.candidates[ballot]
        con_votes = ballots[contra] || 0
        
        if (pro_votes - con_votes)/total_votes > 0.5
          return run_elected_action(ballot)
        end
      end
    end
    
    def run_elected_action(ballot)
      log "Electing #{ballot}"
      case ballot
      when :expand
        my_cloud.running_action = :expanding
        my_cloud.launch_instance!(:cloud_name => my_cloud.name)
        my_cloud.running_action = nil
      when :contract        
        my_cloud.running_action = :contracting
        my_cloud.terminate_youngest_instance!
        my_cloud.running_action = nil
      else
        "none"
      end
    end
  
  end
end