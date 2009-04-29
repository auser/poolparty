require "#{::File.dirname(__FILE__)}/../net/remoter/connections"
require "#{::File.dirname(__FILE__)}/../core/string"
require 'fileutils'
Dir["#{::File.dirname(__FILE__)}/configurations/*"].each {|lib| require lib}

#provide a very simple provisioner with as few dependencies as possible
module PoolParty
  module Provision
   
    class DrConfigure
     include ::PoolParty::Remote
     
     def self.defaults
       ::PoolParty::Default.default_options.merge({
         :full_keypair_path   => "#{ENV["AWS_KEYPAIR_NAME"]}" || "~/.ssh/id_rsa",
         :installer           => 'apt-get install -y',
         :dependency_resolver => 'chef',
         :invalid_run_count => 2
       })
     end
     
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
    
     attr_reader :cloud, :keypair, :run_count
     
     def initialize(host, opts={}, &block)
       self.class.defaults.merge(opts).to_instance_variables(self)
       @target_host = host
       @configurator = "::PoolParty::Provision::#{dependency_resolver.capitalize}".constantize
       @cloud = opts[:cloud]
       @keypair = @cloud.keypair
       @run_count = 0
       
       @cloud.call_before_configure_callbacks if @cloud
       prescribe_configuration
       execute!
     end
     
     private
     
     def prescribe_configuration
       ::FileUtils.mkdir_p "#{Default.tmp_path}/dr_configure" unless ::File.directory?("#{Default.tmp_path}/dr_configure")
      ::File.cp $pool_specfile, "#{Default.tmp_path}/dr_configure/clouds.rb"
      ::File.open "#{Default.tmp_path}/dr_configure/clouds.json", "w" do |f|
        f << cloud.to_properties_hash.to_json
      end
      
      setup_configurator
      # write_erlang_cookie
      @configurator.files_to_upload.each {|f| ::FileUtils.cp f, "#{Default.tmp_path}/dr_configure/#{::File.basename(f)}" if ::File.file?(f) }
      
      pack_up_and_ship_off_suitcase
      run_commands          
     end
     
     def execute!
      super
      add_run_count!
      begin
        if @cloud.verifiers.size > 0
          @cloud.passing?
          @cloud.vputs "Cloud passed verification"
        end
      rescue Exception => e
        if run_count < invalid_run_count
          @cloud.vputs <<-EOM

          Verification failed: #{e}"
            Running configure again to try to solve the problem

          EOM
          execute!
        else
          @cloud.vputs <<-EOM

          Verification failed: #{e}"
            Please check your clouds.rb for any errors

          EOM
        end        
      end
     end
     
     def run_commands
       commands << [
         'chmod 644 /var/poolparty/dr_configure/clouds.json',
         'chmod 644 /var/poolparty/dr_configure/clouds.rb',
         'cp /var/poolparty/dr_configure/clouds.json /etc/poolparty',
         'cp /var/poolparty/dr_configure/clouds.rb /etc/poolparty',
         "touch /var/poolparty/POOLPARTY.PROGRESS",
         'echo "configure" >> /var/poolparty/POOLPARTY.PROGRESS'
         ]
       commands << self.class.class_commands unless self.class.class_commands.empty?
       commands << @configurator.commands
     end
     
     def pack_up_and_ship_off_suitcase
       ::Suitcase::Zipper.build_dir!("#{Default.tmp_path}/dr_configure")
       rsync "#{Default.tmp_path}/dr_configure/", "/var/poolparty/dr_configure/", ['-a --stats']
     end
     
     # Pack up  monitors verifiers plugins directories in the same direcotry as your clouds.rb and send to nodes.
     def setup_configurator
       # @cloud.write_properties_hash("#{Default.tmp_path}/properties_hash.rb")
       #TODO: move to puppet class       
       #TODO: remove or conditionalize this puppet specific task
       @cloud.build_and_store_new_config_file("#{Default.tmp_path}/dr_configure/poolparty.pp")
       
       %w(monitors verifiers plugins).each do |dir|
        @cloud.pack_user_directory dir
       end
     end
     
     def add_run_count!
      @run_count += 1
     end
     
     def write_erlang_cookie
       # cookie = (1..16).collect { chars[rand(chars.size)] }.pack("C*")
       cookie =  (1..65).collect {rand(9)}.join()
       cookie_file = ::File.open("#{Default.tmp_path}/dr_configure/erlang.cookie", 'w+'){|f| f << cookie }
       ::File.cp "#{::File.dirname(__FILE__)}/../templates/erlang_cookie_maker", "#{Default.tmp_path}/dr_configure/"
     end
     
     def self.class_commands
       @class_commands ||= []
     end
    
   end 
  end
end