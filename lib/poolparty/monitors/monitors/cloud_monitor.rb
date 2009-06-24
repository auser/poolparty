=begin rdoc
  Cloud monitor just is a test monitor to hold cloud info
=end

module Monitors
  
  class Cloud < BaseMonitor
    
    def get(_unused=nil)
      {   :name => my_cloud.name, 
          :minimum_instances => my_cloud.minimum_instances, 
          :maximum_instances => my_cloud.maximum_instances,
          :remoter_base => my_cloud.remoter_base,
          :keypair => my_cloud.keypair
      }
    end
    
    def nominations(_unused=nil)
      running_nodes = my_cloud.nodes(:status => "running")
      nominations = []
      running_nodes.each do |node|
        timeout(10) do
          log "Checking with #{node.internal_ip} for nominations: #{open("http://#{node.internal_ip}:8642/stats/get_nominations").read}"
          nominations << begin
            JSON.parse(open("http://#{node.internal_ip}:8642/stats/nominations").read) || "none"
          rescue
            log "Error when connecting to #{node.internal_ip}: #{e.inspect}"
            "none"
          end
        end
      end.histogram
    end
    
  end
  
end