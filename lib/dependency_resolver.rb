$LOAD_PATH.unshift(File.dirname(__FILE__))

module DependencyResolvers
  def self.all
    @all ||= []
  end
end

%w(proxy_object base chef).each do |lib|
  require "dependency_resolvers/#{lib}"
end