=begin rdoc
  Remote instances
=end
module CloudProviders
  class RemoteInstance
    include Dslify, Connections
    
    attr_reader :name, :init_opts
    
    def initialize(init_opts={}, &block)
      @init_opts = init_opts
      set_vars_from_options(init_opts)
      instance_eval &block if block
      after_initialized
    end
    
    def keypair(n=nil)
      @keypair ||= n.nil? ? nil : Keypair.new(n)
    end
    
    def after_initialized
    end
    
    def rsync_dir(dir)
      rsync :source => dir, :destination => "/"
    end
    
    def run
      warn "#{self.class} does not implement run. Something is wrong"
    end
    
    def default_keypair_path
      self.class.default_keypair_path
    end
    
    private
    
    def cloud
      init_opts.has_key?(:cloud) ? init_opts[:cloud] : nil
    end
        
  end
end