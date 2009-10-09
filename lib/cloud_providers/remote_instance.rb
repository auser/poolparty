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
      ssh([
        "/var/lib/gems/1.8/bin/chef-solo -j /etc/chef/dna.json -c /etc/chef/solo.rb"
      ])
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
    
    def cloud_provider
      cloud.cloud_provider
    end
        
  end
end