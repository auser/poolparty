$:.unshift(File.dirname(__FILE__) + '/../../lib')
require 'poolparty'

%w(context matchy).each do |library|
  begin
    require library
  rescue
    STDERR.puts "== Cannot run test without #{library}"
  end
end

class TestBaseClass < PoolParty::PoolPartyBaseClass  
end