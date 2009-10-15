=begin rdoc
  CloudProvider is the base class for cloud computing services such as Ec2, Eucalyptus - where your servers run.
=end
module CloudProviders
  class CloudProvider
    include Dslify
    
    attr_reader :name, :init_opts
    
    def initialize(name, init_opts={}, &block)
      @name = name
      if name.is_a?(Hash) && init_opts.empty?
        @init_opts = name
      else
        @init_opts = init_opts
      end
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
    
    def bootstrap_nodes!
    end
    
    def method_missing(m,*a,&block)
      if cloud.respond_to?(m)
        cloud.send(m,*a,&block)
      else
        super
      end
    end
    
    private
    
    def cloud
      init_opts.has_key?(:cloud) ? init_opts[:cloud] : nil
    end
    
    def progress_bar_until(msg=nil, &block)
      puts "#{msg}"
      loop do
        if block.call
          break
        else
          $stdout.print "."
          $stdout.flush
          sleep 1
        end
      end
      puts "OK"
    end
        
  end
end