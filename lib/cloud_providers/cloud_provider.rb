=begin rdoc
  CloudProvider is the base class for cloud computing services such as Ec2, Eucalyptus - where your servers run.
=end
require "#{File.dirname(__FILE__)}/connections"
module CloudProviders
  class CloudProvider
    include Dslify, Connections
    
    attr_reader :name, :init_opts
    
    def initialize(name, init_opts={}, &block)
      @name = name
      @init_opts = init_opts
      set_vars_from_options(init_opts)
      instance_eval &block if block
      after_initialized
    end
    
    def after_initialized
    end
    
    def run
      warn "#{self.class} does not implement run. Something is wrong"
    end
    
    def default_keypair_path
      self.class.default_keypair_path
    end
    
    def self.default_keypair_path
      ENV["EC2_CONFIG_DIR"] || "#{ENV["HOME"]}/.ssh"
    end
    
    private
    
    def cloud
      init_opts.has_key?(:cloud) ? init_opts[:cloud] : nil
    end
        
  end
end