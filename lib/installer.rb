$LOAD_PATH.unshift(File.dirname(__FILE__))
$LOAD_PATH.unshift(File.dirname(__FILE__) + "/poolparty")

require "poolparty"

%w(base_installer vmware).each do |lib|
  require "installers/#{lib}"
end

PoolParty::Installers::Vmware.new