$LOAD_PATH.unshift(File.dirname(__FILE__))

%w(base).each do |lib|
  require "dependency_resolvers/#{lib}"
end