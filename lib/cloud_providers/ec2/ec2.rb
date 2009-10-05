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
      :minimum_instances      => 1,
      :maximum_instances      => 3,
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
    def run
      puts "  for cloud: #{cloud.name}"
      puts "  minimum_instances: #{minimum_instances}"
      puts "  maximum_instances: #{maximum_instances}"
      puts "  security_groups: #{security_groups}"
      
      unless _security_groups.empty?
        _security_groups.each do |sg|
          sg.run
        end
      end
      
      unless load_balancers.empty?
        load_balancers.each do |lb|
          puts "    load balancer: #{lb.name}"
          lb.run
        end
      end
      
      if autoscales.empty?
        puts "---- live, running instances (#{instances.size}) ----"
        if instances.size < minimum_instances
          expansion_count = minimum_instances - instances.size
          puts "-----> expanding the cloud because the minimum_instances is not satisified: #{expansion_count} (TODO)"
        elsif instances.size > maximum_instances
          contraction_count = instances.size - maximum_instances
          puts "-----> contracting the cloud because the instances count exceeds the maximum_instances by #{contraction_count} (TODO)"
        end
      else
        autoscales.each do |a|
          puts "    autoscaler: #{a.name}"
          puts "-----> The autoscaling groups will launch the instances"
          a.run
        end
      end
    end
    def instances
      @instances ||= describe_instances
    end
    def describe_instances
      @describe_instances ||= ec2.describe_instances.reservationSet.item.map do |r|
        r.instancesSet.item.map do |i|
          {
            :instance_id => i["instanceId"],
            :security_groups => r.groupSet.item[0].groupId,
            :image_id => i["imageId"],
            :private_ip => i["privateIpAddress"],
            :dns_name => i["dnsName"],
            :instance_type => i["instanceType"],
            :public_ip => i["ipAddress"],
            :keypair => i["keyName"],
            :launch_time => i["launchTime"],
            :availability_zones => i["placement"]["availabilityZone"],
            :status => i["instanceState"]["name"]
          }
        end
      end
    end
    
    # Extras!
    
    def load_balancer(*arr)
      name, o, block = *arr
      load_balancers << ElasticLoadBalancer.new(name, dsl_options.merge(o), &block)
    end
    def autoscale(*arr)
      name, o, block = *arr
      autoscales << ElasticAutoScaler.new(name, dsl_options.merge(o), &block)
    end
    def security_group(name, o={}, &block)
      _security_groups << SecurityGroup.new(name, dsl_options.merge(o), &block)
    end
    def method_missing(m,*a,&block)
      if cloud.respond_to?(m)
        cloud.send(m,*a,&block)
      else
        super
      end
    end
    protected
    def ec2
      @ec2 ||= AWS::EC2::Base.new( :access_key_id => access_key, :secret_access_key => secret_access_key )
    end
    def as
      @as = AWS::Autoscaling::Base.new( :access_key_id => access_key, :secret_access_key => secret_access_key )
    end
    def elb
      @elb ||= AWS::ELB::Base.new( :access_key_id => access_key, :secret_access_key => secret_access_key )
    end
    def security_groups
      _security_groups.map {|a| a.to_s }
    end
    def _security_groups
      @security_groups ||= [SecurityGroup.new("default", {:parent => self, :cloud => cloud})]
    end
    private
    def load_balancers
      @load_balancers ||= []
    end
    def autoscales
      @autoscales ||= []
    end
    
    def generate_keypair(n=nil)
      puts "[EC2] generate_keypair is called with #{n}"
      begin
        hsh = ec2.create_key_pair(n)
        string = hsh[:aws_material]
        FileUtils.mkdir_p default_keypair_path unless File.directory?(default_keypair_path)
        puts "[EC2] Generated keypair #{default_keypair_path/n}"
        vputs "[EC2] #{string}"
        File.open(default_keypair_path/n, "w") {|f| f << string }
        File.chmod 0600, default_keypair_path/n
      rescue RightAws::AwsError => e
        puts "[EC2] The keypair exists in EC2, but we cannot find the keypair locally: #{n}"
      end
      keypair n
    end

  end
end

%w(security_group authorize elastic_auto_scaler elastic_block_store elastic_load_balancer revoke).each do |lib|
  require "#{File.dirname(__FILE__)}/helpers/#{lib}"
end