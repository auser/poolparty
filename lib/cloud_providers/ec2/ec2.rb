=begin rdoc
  EC2 CloudProvider
  This serves as the basis for running PoolParty on Amazon's ec2 cloud.
=end
begin
  require 'AWS'
rescue LoadError
  puts <<-EOM
  There was an error requiring AWS
EOM
end

module CloudProviders
  class Ec2 < CloudProvider
    # Set the aws keys from the environment, or load from /etc/poolparty/env.yml if the environment variable is not set
    def self.default_access_key
      ENV['EC2_ACCESS_KEY'] || load_keys_from_file[:access_key]
    end
    
    def self.default_secret_access_key
      ENV['EC2_SECRET_KEY'] || load_keys_from_file[:secret_access_key]
    end
    
    def self.default_private_key
      ENV['EC2_PRIVATE_KEY'] || load_keys_from_file[:private_key]
    end
    
    def self.default_cert
      ENV['EC2_CERT'] || load_keys_from_file[:cert]
    end
    
    def self.default_user_id
      ENV['EC2_USER_ID'] || load_keys_from_file[:user_id]
    end
    
    def self.default_ec2_url
      ENV['EC2_URL'] || load_keys_from_file[:ec2_url]
    end
    
    def self.default_s3_url
      ENV['S3_URL'] || load_keys_from_file[:s3_url]
    end
    
    def self.default_cloud_cert
     ENV['CLOUD_CERT'] || ENV['EUCALYPTUS_CERT'] || load_keys_from_file[:cloud_cert]
    end
    
    # Load the yaml file containing keys.  If the file does not exist, return an empty hash
    def self.load_keys_from_file(filename="#{ENV["HOME"]}/.poolparty/aws", caching=true)
      return @aws_yml if @aws_yml && caching==true
      return {} unless File.exists?(filename)
      puts("Reading keys from file: #{filename}")
      @aws_yml = YAML::load( open(filename).read ) || {}
    end
    
    default_options(
      :image_id               => 'ami-ed46a784',
      :instance_type          => 'm1.small',
      :addressing_type        => "public",
      :availability_zones     => ["us-east-1a"],
      :user_id                => default_user_id,
      :private_key            => default_private_key,
      :cert                   => default_cert,
      :cloud_cert             => default_cloud_cert,
      :access_key             => default_access_key,
      :secret_access_key      => default_secret_access_key,
      :ec2_url                => default_ec2_url,
      :s3_url                 => default_s3_url,
      :min_count              => 1,
      :max_count              => 1,
      :user_data              => '',
      :addressing_type        => nil,
      :kernel_id              => nil,
      :ramdisk_id             => nil,
      :block_device_mappings  => nil,
      :elastic_ips            => [],  # An array of the elastic ips
      :ebs_volumes            => []   # The volume id of an ebs volume # TODO: ensure this is consistent with :block_device_mappings
    )
    
    # Called when the create command is called on the cloud
    def create!
      [:security_groups, :load_balancers].each do |type|
        self.send(type).each {|ele| ele.create! }
      end
    end
    
    def run
      puts "  for cloud: #{cloud.name}"
      puts "  minimum_instances: #{minimum_instances}"
      puts "  maximum_instances: #{maximum_instances}"
      puts "  security_groups: #{security_group_names.join(", ")}"
      puts "  running on keypair: #{keypair}"
            
      security_groups.each do |sg|
        sg.run
      end
      
      unless load_balancers.empty?
        load_balancers.each do |lb|
          puts "    load balancer: #{lb.name}"
          lb.run
        end
      end     
      if autoscalers.empty?
        puts "---- live, running instances (#{nodes.size}) ----"
        if nodes.size < minimum_instances
          expansion_count = minimum_instances - nodes.size
          puts "-----> expanding the cloud because the minimum_instances is not satisified: #{expansion_count}"
          expand_by(expansion_count)
        elsif nodes.size > maximum_instances
          contraction_count = nodes.size - maximum_instances
          puts "-----> contracting the cloud because the instances count exceeds the maximum_instances by #{contraction_count}"
          contract_by(contraction_count)
        end
        
        progress_bar_until("Waiting for the instances to be launched") do
          reset!
          running_nodes = nodes.select {|n| n.running? }
          running_nodes.size >= minimum_instances
        end
        reset!
        # ELASTIC IPS
        unless _elastic_ips.empty?
          unused_elastic_ip_addresses = ElasticIp.unused_elastic_ips(self).map {|i| i.public_ip }
          used_elastic_ip_addresses = ElasticIp.elastic_ips(self).map {|i| i.public_ip }

          elastic_ip_objects = ElasticIp.unused_elastic_ips(self).select {|ip_obj| _elastic_ips.include?(ip_obj.public_ip) }

          assignee_nodes = nodes.select {|n| !ElasticIp.elastic_ips(self).include?(n.public_ip) }

          elastic_ip_objects.each_with_index do |eip, idx|
            # Only get the nodes that do not have elastic ips associated with them
            begin
              if assignee_nodes[idx]
                puts "Assigning elastic ip: #{eip.public_ip} to node: #{assignee_nodes[idx].instance_id}"
                ec2.associate_address(:instance_id => assignee_nodes[idx].instance_id, :public_ip => eip.public_ip)
              end
            rescue Exception => e
              p [:error, e.inspect]
            end
            reset!
          end
        end
      else
        autoscalers.each do |a|
          puts "    autoscaler: #{a.name}"
          puts "-----> The autoscaling groups will launch the instances"
          a.run
          
          progress_bar_until("Waiting for autoscaler to launch instances") do
            reset!
            running_nodes = nodes.select {|n| n.running? }
            minimum_instances >= running_nodes.size
          end
          reset!
        end
      end
      
      reset!
      from_ports = security_groups.map {|a| a.authorizes.map {|t| t.from_port.to_i }.flatten }.flatten
      
      if from_ports.include?(22)
        progress_bar_until("Waiting for the instances to be accessible by ssh") do
          running_nodes = nodes.select {|n| n.running? }
          accessible_count = running_nodes.map do |node|
            node.accessible?
          end.size
          accessible_count == running_nodes.size
        end
      end
      
    end
    
    def teardown
      puts "------ Tearing down and cleaning up #{cloud.name} cloud"
      unless autoscalers.empty?
        puts "Tearing down autoscalers"
      end
    end
    
    def expand_by(num=1)
      e = Ec2Instance.run!({
        :image_id => image_id,
        :min_count => num,
        :max_count => num,
        :key_name => keypair.basename,
        :security_groups => security_groups,
        :user_data => user_data,
        :instance_type => instance_type,
        :availability_zone => availability_zones.first,
        :base64_encoded => true,
        :cloud => cloud
      })
      reset!
      e
    end
    
    def contract_by(num=1)
      num.times do |i|
        id = nodes[-num].instance_id
        Ec2Instance.terminate!(:instance_id => id, :cloud => cloud)
      end
      reset!
    end
    
    def bootstrap_nodes!(tmp_path=nil)
      tmp_path ||= cloud.tmp_path
      nodes.each do |node|
        next unless node.in_service?
        node.cloud_provider = self
        node.rsync_dir(tmp_path)
        node.bootstrap_chef!
        node.run_chef!
      end
    end
    
    def configure_nodes!(tmp_path=nil)
      tmp_path ||= cloud.tmp_path
      nodes.each do |node|
        next unless node.in_service?
        node.cloud_provider = self
        node.rsync_dir(tmp_path) if tmp_path
        node.run_chef!
      end
    end
    
    def nodes
      all_nodes.select {|i| i.in_service? }#describe_instances.select {|i| i.in_service? && security_groups.include?(i.security_groups) }
    end
    
    def all_nodes
      #TODO: need to sort by launch time
      # 
      @nodes ||= describe_instances.select {|i| security_groups.include?(i.security_groups) }
    end
    
    # Describe instances
    # Describe the instances that are available on this cloud
    # @params id (optional) if present, details about the instance
    #   with the id given will be returned
    #   if not given, details for all instances will be returned
    def describe_instances(id=nil)
      @describe_instances = ec2.describe_instances.reservationSet.item.map do |r|
        r.instancesSet.item.map do |i|
          inst_options = i.merge(r.merge(:cloud => cloud)).merge(cloud.cloud_provider.dsl_options)
          Ec2Instance.new(inst_options)
        end
      end.flatten
      # id.nil? ? @describe_instances : @describe_instances.select {|a| a.instance_id == id }.first
    end
    
    # Extras!
    
    def load_balancer(name=cloud.proper_name, o={}, &block)
      load_balancers << ElasticLoadBalancer.new(name, sub_opts.merge(o || {}), &block)
    end
    def autoscale(name=cloud.proper_name, o={}, &block)
      autoscalers << ElasticAutoScaler.new(name, sub_opts.merge(o || {}), &block)
    end
    def security_group(name=cloud.proper_name, o={}, &block)
      security_groups << SecurityGroup.new(name, sub_opts.merge(o || {}), &block)
    end
    def elastic_ip(*ips)
      ips.each {|ip| _elastic_ips << ip}
    end
        
    # Proxy to the raw Grempe amazon-aws @ec2 instance
    def ec2
      @ec2 ||= AWS::EC2::Base.new( :access_key_id => access_key, :secret_access_key => secret_access_key )
    end
    
    # Proxy to the raw Grempe amazon-aws autoscaling instance
    def as
      @as = AWS::Autoscaling::Base.new( :access_key_id => access_key, :secret_access_key => secret_access_key )
    end
    
    # Proxy to the raw Grempe amazon-aws elastic_load_balancing instance
    def elb
      @elb ||= AWS::ELB::Base.new( :access_key_id => access_key, :secret_access_key => secret_access_key )
    end
    def security_group_names
      security_groups.map {|a| a.to_s }
    end
    def security_groups
      @security_groups ||= []
    end
    def load_balancers
      @load_balancers ||= []
    end
    def autoscalers
      @autoscalers ||= []
    end
    
    # Clear the cache
    def reset!
      @nodes = @describe_instances = nil
    end
    
    private
    # Helper to get the options with self as parent
    def sub_opts
      dsl_options.merge(:parent => self, :cloud => cloud)
    end
    def _elastic_ips
      @_elastic_ips ||= []
    end
    def generate_keypair(n=nil)
      puts "[EC2] generate_keypair is called with #{default_keypair_path/n}"
      begin
        hsh = ec2.create_keypair(:key_name => n)
        string = hsh.keyMaterial
        FileUtils.mkdir_p default_keypair_path unless File.directory?(default_keypair_path)
        puts "[EC2] Generated keypair #{default_keypair_path/n}"
        puts "[EC2] #{string}"
        File.open(default_keypair_path/n, "w") {|f| f << string }
        File.chmod 0600, default_keypair_path/n
      rescue Exception => e
        puts "[EC2] The keypair exists in EC2, but we cannot find the keypair locally: #{n} (#{e.inspect})"
      end
      keypair n
    end
    
  end
end

require "#{File.dirname(__FILE__)}/ec2_instance"
require "#{File.dirname(__FILE__)}/helpers/ec2_helper"
%w( security_group 
    authorize 
    elastic_auto_scaler 
    elastic_block_store 
    elastic_load_balancer 
    elastic_ip
    revoke).each do |lib|
  require "#{File.dirname(__FILE__)}/helpers/#{lib}"
end