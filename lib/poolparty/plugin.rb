=begin rdoc
  Simply a stub class for documentation purposes
  Plugins are all resources
=end

%w(apache git rails collectd hermes).each do |plugin|
  require "plugins/#{plugin}"
end