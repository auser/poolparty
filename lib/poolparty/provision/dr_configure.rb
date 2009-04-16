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
         :dependency_resolver => 'chef'
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
    
     attr_reader :cloud
     attr_accessor :full_keypair_path
     def initialize(host, opts={}, &block)
       self.class.defaults.merge(opts).to_instance_variables(self)
       @target_host = host
       @configurator = "::PoolParty::Provision::#{dependency_resolver.capitalize}".constantize
       @cloud = opts[:cloud]
       
       @cloud.call_before_configure_callbacks if @cloud
       prescribe_configuration
       execute!
     end
     
     def prescribe_configuration
       ::FileUtils.mkdir_p "#{Default.tmp_path}/dr_configure" unless ::File.directory?("#{Default.tmp_path}/dr_configure")
      ::File.cp $pool_specfile, "#{Default.tmp_path}/dr_configure/clouds.rb"
      ::File.open "#{Default.tmp_path}/dr_configure/clouds.json", "w" do |f|
        f << cloud.to_properties_hash.to_json
      end
            
      setup_configurator
      write_erlang_cookie
      @configurator.files_to_upload.each {|f| ::FileUtils.cp f, "#{Default.tmp_path}/dr_configure/#{::File.basename(f)}" if ::File.file?(f) }
      
      pack_up_and_ship_off_suitcase
                  
      commands << [
        'chmod 600 /var/poolparty/dr_configure/clouds.json',
        'chmod 600 /var/poolparty/dr_configure/clouds.rb',
        'cp -f /var/poolparty/dr_configure/clouds.json /etc/poolparty',
        'cp /var/poolparty/dr_configure/clouds.rb /etc/poolparty',
        'cp /var/poolparty/dr_configure/erlang.cookie /root/.erlang.cookie',        
        'ruby /var/poolparty/dr_configure/erlang_cookie_maker',
        "touch /var/poolparty/POOLPARTY.PROGRESS",
        'echo "configure" >> /var/poolparty/POOLPARTY.PROGRESS'
        ]
      commands << self.class.class_commands unless self.class.class_commands.empty?
      commands << @configurator.commands      
     end
     
     def pack_up_and_ship_off_suitcase
       ::Suitcase::Zipper.build_dir!("#{Default.tmp_path}/dr_configure")
       
       rsync "#{Default.tmp_path}/dr_configure/", "/var/poolparty/dr_configure/"
     end
     
     def setup_configurator
       # @cloud.write_properties_hash("#{Default.tmp_path}/properties_hash.rb")
       #TODO: move to puppet class
       puts "writting new config file"
       @cloud.build_and_store_new_config_file("#{Default.tmp_path}/dr_configure/poolparty.pp") 
       # Neighborhoods.clump(@cloud.remote_instances_list, "#{Default.tmp_path}/neighborhood.json")
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