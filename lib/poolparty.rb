$LOAD_PATH.unshift(File.dirname(__FILE__))

t=Time.now

# Gems
%w(rubygems json logger erb net/ssh).each do |lib|
  require lib
end

%w(dslify parenting).each do |dep|
  $LOAD_PATH.unshift(File.join(File.dirname(__FILE__),'..', 'vendor/gems', dep, 'lib'))
  require "#{dep}"
end

# Require the poolparty error so we can use it ubiquitously
require "poolparty/pool_party_error"

# Core object overloads
%w( object string integer
    array hash symbol
    time).each do |lib|
  require "core/#{lib}"
end

# Features
%w(callbacks pinger searchable_paths).each do |lib|
  require "mixins/#{lib}"
end

# dependency_resolvers
require "dependency_resolver"

# PoolParty core
%w( default pool_party_log base dsl_base cloud 
    keypair plugin
    pool resource ).each do |lib|
  require "poolparty/#{lib}"
end

vputs "PoolParty core loadtime: #{Time.now-t}"