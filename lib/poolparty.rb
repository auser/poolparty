$LOAD_PATH.unshift(File.dirname(__FILE__))

$TESTING = true # Just for now

t=Time.now

# Gems
%w(rubygems json).each do |lib|
  require lib
end

%w(dslify parenting).each do |dep|
  $LOAD_PATH.unshift(File.join(File.dirname(__FILE__),'..', 'vendor/gems', dep, 'lib'))
  require "#{dep}"
end

# Core object overloads
%w( object string integer
    array hash symbol
    time).each do |lib|
  require "ruby/#{lib}"
end

# Features
%w(callbacks pinger searchable_paths).each do |lib|
  require "mixins/#{lib}"
end

# PoolParty core
%w( default base cloud keypair pool_party_error
    plugin pool resource dependency_resolver).each do |lib|
  require "poolparty/#{lib}"
end

vputs "PoolParty core loadtime: #{Time.now-t}"