module Monitors
  class Elections < BaseMonitor
    
    # these are the rules that define the 
    # elected actions
    def self.candidates
      {
        :expand => :contract,
        :contract => :expand,
        :configure => nil
      }
    end
    
    def get(data=nil)
      'hello'
    end
    
    def put(data=nil)      
      handle_election(data.json_parse.histogram)
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
      ballots.each do |ballot, pro|
        con = self.class.candidates[ballot.to_sym]
        if (pro - con)/ballot.keys.size > 0.5
          return elect(ballot)
        end
      end
    end
    
    def elect(ballot)
      if %w(expand contract).include?(ballot)
        %x[server-cloud-elections #{ballot}]
      end
    end
  
  end
end