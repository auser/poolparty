require "#{::File.dirname(__FILE__)}/../net/remoter/connections"

#provide a very simple provisioner with as few dependencies as possible
module PoolParty
  module Provision
 
    class BootStrapper
      include ::PoolParty::Remote
      
      def self.gem_list
        @gem_list ||= %w(  flexmock
                        lockfile
                        logging
                        ZenTest
                        rake
                        xml-simple
                        sexp_processor
                        net-ssh
                        net-sftp
                        net-scp
                        net-ssh-gateway
                        echoe
                        highline
                        json
                        capistrano
                        ParseTree
                        ruby2ruby
                        activesupport
                        grempe-amazon-ec2
                        RubyInline
                        archive-tar-minitar
                        )
      end
  
      @defaults = ::PoolParty::Default.default_options.merge({
        :full_keypair_path   => "#{ENV["AWS_KEYPAIR_NAME"]}" || "~/.ssh/id_rsa",
        :installer           => 'apt-get',
        :dependency_resolver => 'puppet'
      })
      class <<self; attr_reader :defaults; end
      
      def initialize(host, opts={}, &block)
        self.class.defaults.merge(opts).to_instance_variables(self)
        @target_host = host
        @cloud = opts[:cloud]
                
        instance_eval &block if block        
        @cloud.call_before_bootstrap_callbacks if @cloud
        
        default_commands
        execute!
      end
      
      def self.class_commands
        @class_commands ||= []
      end
        
      def pack_the_dependencies
        # Use the locally built poolparty gem if it is availabl
        if edge_pp_gem = Dir["#{Default.vendor_path}/../pkg/*poolparty*gem"].pop
          puts "using edge poolparty: #{::File.expand_path(edge_pp_gem)}"
          ::Suitcase::Zipper.add(edge_pp_gem, 'gems')        
        else
          self.class.gem_list << 'auser-poolparty'
        end
        # Add the gems to the suitcase
        puts "Adding default gem depdendencies"
        ::Suitcase::Zipper.gems self.class.gem_list, "#{Default.vendor_path}/dependencies"

        ::Suitcase::Zipper.packages "http://rubyforge.org/frs/download.php/45905/rubygems-1.3.1.tgz", "#{Default.vendor_path}/dependencies/packages"

        ::Suitcase::Zipper.add("#{Default.vendor_path}/dependencies/cache", "gems")
        ::Suitcase::Zipper.build_dir!("#{Default.tmp_path}/dependencies")
      end
  
      def default_commands
        pack_the_dependencies
        rsync "#{Default.tmp_path}/dependencies", '/var/poolparty'
        rsync "#{::File.join(File.dirname(__FILE__), '..', 'templates', 'gemrc' )}", '/etc/gemrc'
        
        commands << [
          "mkdir -p /etc/poolparty",
          'cd /var/poolparty/dependencies',
          "#{installer} update",
          "#{installer} install -y ruby1.8 ruby1.8-dev libopenssl-ruby1.8 build-essential wget",  #optional, but nice to have
          "tar -zxvf packages/rubygems-1.3.1.tgz",
          "cd rubygems-1.3.1",
          "ruby setup.rb --no-ri --no-rdoc",
          "ln -sfv /usr/bin/gem1.8 /usr/bin/gem", #TODO: check if this is really needed
          "cd ../ && rm -rf rubygems-1.3.1*",
          "cd /var/poolparty/dependencies/gems/",
          "gem install --no-rdoc --no-ri -y *.gem",
          'touch /var/poolparty/POOLPARTY.PROGRESS',
          'echo "bootstrap" >> /var/poolparty/POOLPARTY.PROGRESS']
        commands << self.class.class_commands unless self.class.class_commands.empty?
      end
    end
    
  end
end