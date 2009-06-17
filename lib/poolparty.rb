$LOAD_PATH.unshift(File.dirname(__FILE__))
$LOAD_PATH.unshift(File.dirname(__FILE__) + "/poolparty")

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
%w(object string).each do |lib|
  require "ruby/#{lib}"
end

# Features
%w(callbacks pinger searchable_paths).each do |lib|
  require "features/#{lib}"
end

# PoolParty core
%w( default base cloud key plugin 
    pool resource dependency_resolver).each do |lib|
  require "core/#{lib}"
end

vputs "PoolParty core loadtime: #{Time.now-t}"