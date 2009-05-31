class Object
  #TODO: deprecate. use RemoterBase.available_bases instead
  def remote_bases
    $remote_bases ||= []
  end
end

# Load the core net libraries.
# These are neccessary for any of the remoter_bases to function.
require ::File.join(::File.dirname(__FILE__),'remoter_base.rb')
require ::File.join(::File.dirname(__FILE__),'remote_instance.rb')

Dir["#{::File.dirname(__FILE__)}/remoter_bases/*/*.rb"].each do |base|
  name = ::File.basename(base, ::File.extname(base))
  require base
end