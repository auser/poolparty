$LOAD_PATH.unshift(File.dirname(__FILE__))

module PoolParty
  module DependencyResolvers
    def self.all
      PoolParty::DependencyResolvers.constants
    end
  end
end

%w(proxy_object base chef).each do |lib|
  require "dependency_resolvers/#{lib}"
end