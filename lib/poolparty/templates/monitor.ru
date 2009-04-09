require 'rubygems'
require 'butterfly'
# require 'poolparty'
require 'poolparty/monitors/stats_monitor_adaptor.rb'
require 'rack'

opts ={ :adaptor_opts => {
           :file => 'poolparty/monitors/stats_monitor_adaptor.rb'},
           :port => PoolParty::Default.butterfly_port
      }
      
use Rack::CommonLogger
run Butterfly::Server.new(opts)