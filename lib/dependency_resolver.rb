$LOAD_PATH.unshift(File.dirname(__FILE__))

%w(proxy_object base chef).each do |lib|
  require "dependency_resolvers/#{lib}"
end