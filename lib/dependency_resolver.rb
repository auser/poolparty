$LOAD_PATH.unshift(File.dirname(__FILE__))
$LOAD_PATH.unshift(File.dirname(__FILE__) + "/poolparty")

require "poolparty"

%w(base).each do |lib|
  require "dependency_resolvers/#{lib}"
end