=begin rdoc
  Schema
  
  Schemas are basically stub clouds without the PoolParty stack
=end
module PoolParty
  class Schema
    attr_accessor :hsh
    def initialize(h={})
      @hsh = {}
      case h
      when Hash
        h.each {|k,v| self[k] = v}
      when Array
        h.each {|el| self[el['ip']]=el}
      when String        
        JSON.parse(h).each {|k,v| self[k.to_sym] = v}
      end
    end

    def [](k)
      hsh[k]
    end
    
    def []=(k,v)
      if v.is_a?(Hash)
        hsh[k.to_sym] = self.class.new(v)
      else
        hsh[k.to_sym] = v
      end      
    end

    def to_hash
      @hsh
    end
    
    def save!
      ::File.open("#{Default.base_config_directory}/#{Default.properties_hash_filename}", "w") {|f| f << self.to_json }
    end
    
    def method_missing(sym, *args, &block)
      if @hsh.has_key?(sym.to_sym)
        @hsh.fetch(sym)
      elsif @hsh.has_key?(sym.to_s)
        @hsh.fetch(sym.to_s)
      else
        @hsh.send(sym, *args, &block)
      end
    end
    
    def to_cloud(node={})
      require "ostruct"
      
      $pool_specfile = "/etc/poolparty/clouds.rb"
      
      remoter_base = PoolParty::Remote.module_eval(options.remote_base.split('::')[-1].camelcase)
      # TODO: Seriously, make this sexier
      
      cld = OpenStruct.new(options)
      cld.keypair = ::PoolParty::Key.new("/etc/poolparty/#{node[:keypair]}")
      cld.remoter_base = remoter_base
      cld.build_and_store_new_config_file = "/etc/poolparty/clouds.json"
      cld.dependency_resolver = PoolParty.module_eval(options.dependency_resolver.split("::")[-1].camelcase).send(:new)
      
      cld
    end
  end
end
class Hash
  def method_missing(sym, *args, &block)
    if has_key?(sym.to_sym)
      fetch(sym)
    elsif has_key?(sym.to_s)
      fetch(sym.to_s)
    else
      super
    end
  end  
end