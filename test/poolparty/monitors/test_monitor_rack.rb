require "#{::File.dirname(__FILE__)}/../../test_helper"
require 'tempfile'

class MonitorRackTest < Test::Unit::TestCase
  context "MonitorRack" do
    setup do
      @rackup=<<EOS
      #!/usr/bin/env ruby
      require 'rubygems'
      require 'rack'
      require 'json'
      require 'thin'
      require 'rest_client'
      require "#{::File.dirname(__FILE__)}/../../../lib/poolparty/monitors/monitor_rack" 

      app = Rack::Builder.new do
        # use Rack::Reloader, 2
        use Rack::ShowExceptions
        # use Rack::PostBodyContentTypeParser  #parses json requests to params hash
        run Monitors::MonitorRack.new()
      end

      run app
EOS
    end

    should "be able to start" do
      tfile = Tempfile.new('monitor_rackup.ru'){|f| f.write @rackup}
      
      output = `thin -R #{tfile.path} start --daemon`
      
      assert $?.success?
      # `thin -R #{tfile.path} stop`
      assert $?.success?
      tfile.unlink
    end
  end
  
end