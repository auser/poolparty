module PoolParty
  class CapistranoConfigurer
    def cap_config
      @config ||= ::Capistrano::Configuration.new
    end
    def cloud
      #{@cloud}
    end
    def provisioner
      #{self}
    end
    def method_missing(sym, *args, &block)
      if cloud.methods.include?(sym)
        cloud.send(sym, *args, &block)
      elsif provisioner.methods.include?(sym)
        provisioner.send(sym, *args, &block)
      else
        cap_config.send(sym, *args, &block)
      end
    end
    
  end
end