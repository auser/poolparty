=begin rdoc
  BootStrapper is contains the basic files that need to be uploaded to a new instance and the first commands that are run to set up a base PoolParty enviornment.  This includes things like ensuring that ruby, rubygems, and poolparty are installed, and then start the monitor.
=end

require "#{::File.dirname(__FILE__)}/../net/remoter/connections"

# Provide a very simple provisioner with as few dependencies as possible
module PoolParty
  module Provision
  
    class BootStrapper
      include ::PoolParty::Remote
      
      # List of gems that are default to install
      def self.gem_list
        # auser-dslify
        # auser-parenting
        @gem_list ||= %w( logging
                          rake
                          xml-simple
                          net-ssh
                          net-sftp
                          net-scp
                          net-ssh-gateway
                          highline
                          json
                          activesupport
                          grempe-amazon-ec2
                          ohai
                          chef
                          adamwiggins-rest-client
                          rack
                          thin
                        )
      end

      # Default options for the boot_strapper
      @defaults = ::PoolParty::Default.default_options.merge({
        :full_keypair_path   => "#{ENV["AWS_KEYPAIR_NAME"]}" || "~/.ssh/id_rsa",
        :installer           => 'apt-get',
        :dependency_resolver => 'chef'
      })
      class <<self; attr_reader :defaults; end
      
      # In case the method is being called on ourself, let's check the 
      # defaults hash to see if it's available there
      def method_missing(m,*a,&block)
        if self.class.defaults.has_key?(m) 
          self.class.defaults[m]
        elsif @cloud
          @cloud.send m, *a, &block
        else
          super
        end
      end
      
      def initialize(host, opts={}, &block)        
        self.class.defaults.merge(opts).to_instance_variables(self)
        @target_host = host
        @cloud = opts[:cloud]
                
        instance_eval &block if block
        @cloud.call_before_bootstrap_callbacks if @cloud
        
        default_commands
        dputs "Starting bootstrapping process on #{host}"
        execute!
        
        @cloud.call_after_bootstrap_callbacks if @cloud
        after_bootstrap
        dputs "Bootstrapping complete on #{host}"
      end
      
      def self.class_commands
        @class_commands ||= []
      end
        
      # Collect all the bootstrap files that will be uploaded to the remote instances
      def pack_the_dependencies
        # Add the keypair to the instance... shudder
        ::Suitcase::Zipper.add(keypair, "keys")
        
        # Use the locally built poolparty gem if it is availabl
        if edge_pp_gem = Dir["#{Default.vendor_path}/../pkg/*poolparty*gem"].pop
          puts "using edge poolparty: #{::File.expand_path(edge_pp_gem)}"
          ::Suitcase::Zipper.add(edge_pp_gem, 'gems')
        else
          vputs "using gem auser-poolparty. use rake build to use edge"
          self.class.gem_list << 'auser-poolparty'
        end
        # Add the gems to the suitcase
        puts "Adding default gem dependencies"
        ::Suitcase::Zipper.gems self.class.gem_list, "#{Default.tmp_path}/trash/dependencies"

        ::Suitcase::Zipper.packages( "http://rubyforge.org/frs/download.php/45905/rubygems-1.3.1.tgz",
                 "#{Default.tmp_path}/trash/dependencies/packages")
        # ::Suitcase::Zipper.add("templates/")
        
        ::Suitcase::Zipper.add("#{::File.dirname(__FILE__)}/../templates/monitor.ru", "/etc/poolparty/")
        ::Suitcase::Zipper.add("#{::File.dirname(__FILE__)}/../templates/monitor.god", "/etc/poolparty/")
                
        ::Suitcase::Zipper.add("#{Default.tmp_path}/trash/dependencies/cache", "gems")        
        
        ::Suitcase::Zipper.add("#{::File.join(File.dirname(__FILE__), '..', 'templates', 'gemrc_template' )}", "etc/poolparty")
        
        instances = @cloud.nodes(:status => "running") + [@cloud.started_instance]
        ::Suitcase::Zipper.add_content_as(
          {:instances => instances.flatten.compact}.to_json, 
          "neighborhood.json", "/etc/poolparty")
        
        ::Suitcase::Zipper.build_dir!("#{Default.tmp_path}/dependencies")
        
        ::Suitcase::Zipper.flush!
        
        # ::FileUtils.rm_rf "#{Default.tmp_path}/trash"
      end
  
      # The commands to setup a PoolParty enviornment
      def default_commands
        pack_the_dependencies
        ::FileUtils.rm_rf "#{Default.tmp_path}/dependencies/gems/cache"
        rsync "#{Default.tmp_path}/dependencies", '/var/poolparty'
        
        commands << [
          "mkdir -p /etc/poolparty",
          "mkdir -p /var/log/poolparty",
          "groupadd -f poolparty",
          # "useradd poolparty  --home-dir /var/poolparty  --groups poolparty  --create-home",
          'cd /var/poolparty/dependencies',
          "cp /var/poolparty/dependencies/etc/poolparty/gemrc_template /etc/poolparty",          
          "#{installer} update",
          "#{installer} install -y ruby ruby1.8-dev libopenssl-ruby1.8 build-essential wget",  #optional, but nice to have
          "tar -zxvf packages/rubygems-1.3.1.tgz",
          "cd rubygems-1.3.1",
          "ruby setup.rb --no-ri --no-rdoc",
          "ln -sfv /usr/bin/gem1.8 /usr/bin/gem", #TODO: check if this is really needed
          "cd ../ && rm -rf rubygems-1.3.1*",
          "gem source --add http://gems.github.com",
          "cd /var/poolparty/dependencies/gems/",
          "gem install --no-rdoc --no-ri -y *.gem",
          "cd /var/poolparty/dependencies",
          "cp /var/poolparty/dependencies/etc/poolparty/* /etc/poolparty/",
          'touch /var/poolparty/POOLPARTY.PROGRESS',
          "mkdir -p /root/.ssh",
          "cp /var/poolparty/dependencies/keys/* /root/.ssh/",
          "chmod 600 /root/.ssh/#{::File.basename(@cloud.keypair.full_filepath)}",
          # "god -c /etc/poolparty/monitor.god",
          "mkdir -p /var/log/poolparty/",
          "thin -R /etc/poolparty/monitor.ru -p 8642 --pid /var/run/stats_monitor.pid --daemon -l /var/log/poolparty/monitor.log start 2>/dev/null",
          'echo "bootstrap" >> /var/poolparty/POOLPARTY.PROGRESS']
        commands << self.class.class_commands unless self.class.class_commands.empty?
      end
      
      def after_bootstrap
        # thin_cmd = "thin -R /etc/poolparty/monitor.ru start -p 8642 --daemonize --pid /var/run/poolparty/monitor.pid --log /var/log/poolparty/monitor.log --environment production --chdir /var/poolparty" #TODO --user poolparty --group poolparty
        # vputs "thin_cmd = #{thin_cmd}"
        # curl_put = "curl -i -XPUT -d'{}' http://localhost:8642/stats_monitor"
        # execute! [ thin_cmd, "sleep 10", curl_put  ] 
        # execute! ["sleep 5", curl_put]
      end
    end
    
  end
end