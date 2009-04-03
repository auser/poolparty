# =begin rdoc
#   Web monitor.
# =end
# module PoolParty
#   module Monitors
#     
#     class WebMonitor < BaseMonitor
#       
#       def run
#         str = %x[httperf]
#         str.split(/\s+/)[-1].to_f rescue 0.0
#       end
#             
#     end
#     
#     register_monitor :cpu
#   end
# end