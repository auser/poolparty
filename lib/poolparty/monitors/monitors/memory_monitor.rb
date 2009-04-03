# module PoolParty
#   module Monitors
#     
#     class MemoryMonitor < BaseMonitor
#       
#       def run
#         uname = %x[uname]
#         case uname.chomp
#         when "Darwin"
#           darwin_memory_usage
#         else
#           nix_memory_usage
#         end                
#       end
#       
#       def darwin_memory_usage
#         str = %x[vm_stat]
#         keep_array = []
#         array_of_strings = str
#         begin
#           free_memory = array_of_strings[/free:(\W*)+([0-9]+)/, 2].to_f
#           active_memory = array_of_strings[/active:(\W*)([0-9]+)/, 2].to_f
#           inactive_memory = array_of_strings[/inactive:(\W*)([0-9]+)/, 2].to_f
# 
#           used_memory = (active_memory + inactive_memory)
#           total_memory = (free_memory + active_memory + inactive_memory)
# 
#           used_memory / total_memory
#         rescue Exception => e
#           0.0
#         end                 
#       end
#       
#       def nix_memory_usage
#         str = %x[free -m | grep -i mem]
#         begin
#           total_memory = str.split[1].to_f
#           used_memory = str.split[2].to_f
# 
#           used_memory / total_memory
#         rescue Exception => e
#           0.0
#         end
#       end
#             
#     end
#     
#     register_monitor :memory
#   end
# end