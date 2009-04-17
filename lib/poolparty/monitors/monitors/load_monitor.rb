# module PoolParty
#   module Monitors
#     
#     class LoadMonitor < BaseMonitor
#       
#       def run
#         stdout = %x[uptime]
#         stdout.split(/\s+/)[-1].to_f rescue 0.0
#       end
#             
#     end
#     
#     register_monitor :cpu
#   end
# end