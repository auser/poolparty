=begin rdoc
  Remote instances
=end
module CloudProviders
  class RemoteInstance
    include Dslify, Connections
    
    attr_reader :name, :init_opts, :raw_response
    attr_accessor :cloud_provider
    
    default_options(
      :instance_id => nil,
      :image_id => nil,
      :status => nil
    )
    
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
    
    def accessible?
      ping_port(public_ip, 22, 40)
    end
    
    def rsync_dir(dir)
      rsync :source => dir/"*", :destination => "/"
    end
    
    def chef_bootstrapped?
      @chef_bootstrapped ||= !ssh(["gem list | grep chef"]).empty?
    end
    
    def bootstrap_chef!
      unless chef_bootstrapped?
        ssh([
          'apt-get update',
          'apt-get autoremove -y',
          'apt-get install -y ruby ruby-dev rubygems git-core',
          'gem sources -a http://gems.opscode.com',
          'gem install chef ohai --no-rdoc --no-ri'
        ])
      end
    end
    
    def run_chef!
      chef_solo_cmd = <<-CMD
        GEM_BIN=$(gem env | grep "EXECUTABLE DIRECTORY" | awk "{print \\$4}") \
        && $GEM_BIN/chef-solo -j /etc/chef/dna.json -c /etc/chef/solo.rb
      CMD
      ssh([chef_solo_cmd.strip.squeeze(' ')])
    end
        
    def run
      warn "#{self.class} does not implement run. Something is wrong"
    end
    
    def default_keypair_path
      self.class.default_keypair_path
    end
    
    ## provide hash like methods to access and iterate over node attributes
    def each
      dsl_options.each{ |k,v| yield k,v }
    end
    
    def [](k)
      if dsl_options.has_key? k
        dsl_options[k]
      else
        nil
      end
    end
    
    def []=(k,v)
      dsl_options[k] = v
    end
    
    def has_key?(key)
      dsl_options.has_key?(key)
    end
    
    def keys
      dsl_options.keys
    end
       
    def values
      dsl_options.values
    end
    
    def to_hash
      dsl_options
    end
    ##end of hash like methods
    
    # Is this instance running?
    def running?
      !(status =~ /running/).nil?
    end
    # Is this instance pending?
    def pending?
      !(status =~ /pending/).nil?
    end
    # Is this instance terminating?
    def terminating?
      !(status =~ /shutting/).nil?
    end
    # Has this instance been terminated?
    def terminated?
      !(status =~ /terminated/).nil?
    end
    
    # elapsed seconds since node launch time
    def elapsed_runtime
      Time.now - Time.parse(launch_time)
    end
    
    # def  to_s
    #  (cloud ? to_hash.merge(:cloud=>cloud.name) : to_hash)
    # end
    
    private
    def cloud
      init_opts.has_key?(:cloud) ? init_opts[:cloud] : nil
    end
    
    def cloud_provider
      cloud.cloud_provider
    end
    
  end
end