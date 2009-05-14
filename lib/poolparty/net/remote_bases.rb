require File.dirname(__FILE__) + "/remoter_base"

class Object
  def remote_bases
    $remote_bases ||= []
  end
  
  #TODO: deprecate, this is taken care of with inherited hook
  # Register the remoter base in the remote_bases global store
  def register_remote_base(*args)
    args.each do |arg|
      base_name = "#{arg}".downcase.to_sym
      (remote_bases << base_name) unless remote_bases.include?(base_name)
    end
  end
  alias :available_bases :remote_bases
end

Dir["#{File.dirname(__FILE__)}/remote_bases/*.rb"].each do |base| 
  name = ::File.basename(base, ::File.extname(base))
  require base
end
