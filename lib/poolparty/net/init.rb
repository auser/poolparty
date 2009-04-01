# Load the core net libraries.  These are neccessary for any of the remoter_bases to function.
%w(remote_instance messenger remote_bases remoter_base).each do |file|
  require File.join(::File.dirname(__FILE__),file+'.rb')
end

# Register available remoter_bases
Dir["#{::File.dirname(__FILE__)}/remoter_bases/*/*.rb"].each do |base| 
  name = File.join(::File.basename(base, ::File.extname(base)))
  require base
  register_remote_base name
end