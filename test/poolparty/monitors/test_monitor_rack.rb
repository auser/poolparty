require "#{::File.dirname(__FILE__)}/../../test_helper"
require 'tempfile'
require 'rack/test'


class MonitorRackTest < Test::Unit::TestCase
  include Rack::Test::Methods
  context "MonitorRack" do
    setup do
      @rackup = ::File.dirname(__FILE__)+"/../../../lib/poolparty/templates/monitor.ru"
    end
    
    should "be able to start" do
      tfile = Tempfile.new('monitor_rack.pid')
      output = IO.popen("thin -R #{@rackup} start", 'r')
      # output = IO.popen("uptime", 'r')
      p output.status
      assert $?.success?
      
      `thin -R #{tfile.path} stop`
      assert $?.success?
      tfile.unlink
    end
  end
  
end