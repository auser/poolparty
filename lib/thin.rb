require "rubygems"
require "#{File.dirname(__FILE__)}/../vendor/gems/amazon-ec2/lib/AWS"
class Array
  # Example  nodes.select_with_hash(:status=>'running')
  def select_with_hash(conditions={})
    return self if conditions.empty?
    select do |node|
      conditions.any? do |k,v|
        ( node.has_key?(k) && node[k]==v ) or ( node.respond_to?(k) && node.send(k)==v )
      end
    end
  end
end
class Hash
  def diff(other, *hsh)
    keys.map do |k|
      if hsh.include?(k)
        other[k] == self[k] ? nil : {k => other[k]}
      end
    end.reject {|b| b.nil? }
  end
end
class Object
  def pool(name=nil, &block)
    @pool ||= PoolParty::Pool.new(name, &block)
  end
end
class String
  # Turn a downcased string and capitalize it
  # so that it can be a class
  # doc_river #=> DocRiver
  def camelcase
    gsub(/(^|_|-)(.)/) { $2.upcase }
  end
  
  # "FooBar".snake_case #=> "foo_bar"
  def snake_case
   gsub(/\B[A-Z]+/, '_\&').downcase
  end
 
  # "FooBar".dasherize #=> "foo-bar"
  def dasherize
    gsub(/\B[A-Z]+/, '-\&').downcase
  end
    
  # Turn a string from lowercased with a .
  # to a classified classname
  # rice_and_beans #=> "RiceAndBeans"
  # handles subclassed and namespaced classes as well
  # for instance
  #   rice::and::beans #=> Rice::And::Beans
  def classify
    self.sub(/.*\./, '').split("::").map {|ele| ele.camelcase }.join("::")
  end
  # Constantize tries to find a declared constant with the name specified
  # in the string. It raises a NameError when the name is not in CamelCase
  # or is not initialized.
  #
  # Examples
  #   "Module".constantize #=> Module
  #   "Class".constantize #=> Class
  def constantize(mod=Object)
    camelcased_word = classify
    begin
      mod.module_eval(camelcased_word, __FILE__, __LINE__)
    rescue NameError
      nil
    end
  end
end
module Dslify
  def self.included(base)
    base.send     :include, InstanceMethods
    base.extend(ClassMethods)
  end
  
  module ClassMethods    
    def default_options(hsh={})
      (@_dsl_options ||= {}).merge! hsh
      set_default_options(@_dsl_options)
    end
    
    def dsl_options
      @_dsl_options ||= {}
    end
    def options
      dsl_options
    end
    
    def dsl_methods(*syms)
      syms.each {|sym| set_default_options({sym => nil}) }
    end
    
    def set_default_options(new_options)
      new_options.each do |k,v|
        dsl_options[k] = v
        class_eval define_dsl_method_str(k)
      end
    end
    
    def define_dsl_method_str(k)
      <<-EOE
        def #{k}(n=nil)
          if n.nil?
            fetch(:#{k})
          else
            self.#{k}=n
          end          
        end
        def #{k}=(n)
          dsl_options[:#{k}] = n
        end
        def fetch(k)
          dsl_options[k]                    
        end
      EOE
    end
    
    def inherited(subclass)
      subclass.set_default_options(dsl_options)
    end
  end
  module InstanceMethods
    def dsl_options
      @dsl_options ||= self.class.dsl_options.clone
    end
    def default_options
      Hash[*dsl_options.select{|k,v| self.class.default_options.has_key?(k) }.inject([]){|res,(k,v)| res << k << v }]
    end
    def set_vars_from_options(hsh={})
      hsh.each do |k,v| 
        instance_eval self.class.define_dsl_method_str(k) unless self.respond_to?(k)
        self.send k, v
      end
    end
    
    def set_default_options(hsh={})
      self.class.set_default_options(hsh)
    end
    
    def method_missing(m,*a,&block)
      if m.to_s[-1..-1] == '?'
        t = m.to_s.gsub(/\?/, '').to_sym
        warn "DEPRECATED: Dslify will no longer support ? methods. Fix yo code.: #{m}"
        respond_to?(t) && !self.send(t, *a, &block).nil?
      else
        super
      end
    end
  end
end

module CloudProviders
  class CloudProvider
    include Dslify
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
    private
    def cloud
      init_opts.has_key?(:cloud) ? init_opts[:cloud] : nil
    end
  end
  class Base < CloudProvider
  end
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
          puts "-----> expanding the cloud because the minimum_instances is not satisified: #{expansion_count}"
        elsif instances.size > maximum_instances
          contraction_count = instances.size - maximum_instances
          puts "-----> contracting the cloud because the instances count exceeds the maximum_instances by #{contraction_count}"
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
    def load_balancer(*arr)
      name, o, block = *arr
      load_balancers << ElasticLoadBalancer.new(name, o.merge(:parent => self, :cloud => cloud), &block)
    end
    def autoscale(*arr)
      name, o, block = *arr
      autoscales << ElasticAutoScaler.new(name, o.merge(:parent => self, :cloud => cloud), &block)
    end
    def security_group(name, o={}, &block)
      _security_groups << SecurityGroup.new(name, o.merge(:parent => self, :cloud => cloud), &block)
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
  end
  class Authorize < Ec2
    default_options({
                :protocol => "tcp",
                :from_port => "22",
                :to_port => "22",
                :cidr_ip => "0.0.0.0/0"})
    def run
      puts "Authorizing: #{cloud.proper_name} for #{protocol} to #{from_port}:#{to_port} #{cidr_ip}"
      options = { :group_name => cloud.proper_name,
                  :ip_protocol => protocol,
                  :from_port => from_port,
                  :to_port => to_port,
                  :cidr_ip => cidr_ip}
                  
      ec2.authorize_security_group_ingress(options) rescue nil
    end
  end
  class Revoke < Ec2
    default_options({
                :protocol => "tcp",
                :from_port => "22",
                :to_port => "22",
                :cidr_ip => "0.0.0.0/0"})
    def run
      puts "Revoking: #{cloud.proper_name} for #{protocol} to #{from_port}:#{to_port} #{cidr_ip}"
      options = { :group_name => cloud.proper_name,
                  :ip_protocol => protocol,
                  :from_port => from_port,
                  :to_port => to_port,
                  :cidr_ip => cidr_ip}
                  
      ec2.revoke_security_group_ingress(options) rescue nil
    end
  end
  
  class SecurityGroup < Ec2
    def run
      if should_create_security_group?
        create_security_group! rescue nil
      end
      authorizes.each {|a| a.run }
      revokes.each {|a| a.run }
    end
    def authorize(o={}, &block)
      authorizes << Authorize.new("#{name}", o.merge(:parent => parent, :cloud => cloud), &block)
    end
    def revoke(o={}, &block)
      revokes << Revoke.new("#{name}", o.merge(:parent => parent, :cloud => cloud), &block)
    end
    def create_security_group!
      ec2.create_security_group(:group_name => cloud.proper_name, :group_description => "PoolParty generated security group: #{cloud.proper_name}")
    end
    def should_create_security_group?
      security_groups.select {|sg| sg[:name] == cloud.proper_name }.empty?
    end
    def security_groups
      @security_groups ||= ec2.describe_security_groups.securityGroupInfo.item.map do |sg|
        perms = sg["ipPermissions"] || {"item" => []} rescue [{"item" => []}]
        {
          :name => sg["groupName"],
          :description => sg["groupDescription"],
          :ip_permissions => perms["item"].map do |i|
            ip_ranges = i["ipRanges"] || {"item" => []} rescue {"item" => []}
            {
              :from_port => i["fromPort"],
              :to_port => i["toPort"],
              :ip_ranges => ip_ranges["item"].map do |ip|
                {
                  :cidrIp => ip["cidrIp"]
                }
              end
            }
          end
        }
      end
    end
    def to_s
      name
    end
    def authorizes
      @authorizes ||= []
    end
    def revokes
      @revokes ||= []
    end
  end
  class AutoScaler < Ec2
  end
  class ElasticLoadBalancer < Ec2
    default_options(
      :internal_port => 8080,
      :external_port => 80,
      :protocol => "http"
    )
    def run
      if should_create_load_balancer?
        puts "-----> Creating ElasticLoadBalancer: #{name}"
        create_load_balancer!
      end
    end
    
    def should_create_load_balancer?
      elastic_load_balancers.select {|lb| lb.name == name }.empty?
    end
    def create_load_balancer!
      elb.delete_load_balancer(:load_balancer_name => name)
      elb.create_load_balancer(
        :availability_zones => parent.availability_zones,
        :load_balancer_name => name,
        :listeners => [{:protocol => protocol, 
                        :load_balancer_port => external_port.to_s,
                        :instance_port => internal_port.to_s}]
      )
      
    end
    def elastic_load_balancers
      @elastic_load_balancers ||= elb.describe_load_balancers.DescribeLoadBalancersResult.LoadBalancerDescriptions.member.map do |lb|
        {
          :created_time => lb["CreatedTime"],
          :availability_zones => (lb["AvailabilityZones"]["member"] rescue []),
          :dns_name => lb["DNSName"],
          :name => lb["LoadBalancerName"],
          :instances => (g["Instances"]["member"] rescue []).map {|i| {:instance_id => i["InstanceId"]}},
          :health_check => ([lb["HealthCheck"]] rescue []).map do |hc|
            {
              :healthy_threshold => hc["HealthyThreshold"],
              :timeout => hc["Timeout"],
              :unhealthy_threshold => hc["UnhealthyThreshold"],
              :interval => hc["Interval"],
              :target => hc["Target"]
            }
          end,
          :listeners => (lb["Listeners"]["member"] rescue []).map do |listener|
            {
              :instance_port => listener["InstancePort"],
              :protocol => listener["Protocol"],
              :load_balancer_port => listener["LoadBalancerPort"]
            }
          end
        }
      end
    end
  end
  class ElasticAutoScaler < Ec2
    def run
      puts "-----> Checking for launch configuration named: #{name}"
      if should_create_launch_configuration?
        create_launch_configuration!
      end
      if should_create_autoscaling_group?
        create_autoscaling_group
      end
    end
    def should_create_autoscaling_group?
      known = autoscaling_groups.select {|ag| ag.name == cloud.proper_name }
      if known.empty?
        true
      else
        puts "Autoscaling group already defined...: #{name}"
        false
      end
    end
    def should_create_launch_configuration?
      known = launch_configurations.select {|lc| lc.name == name }
      if known.empty?
        true
      else
        differences = known.map do |k|
         t = k.diff({
            :name => cloud.proper_name,
            :image_id => parent.image_id,
            :instance_type => parent.instance_type,
            :security_groups => parent.security_groups.flatten,
            :key_name => cloud.keypair,
            :user_data => parent.user_data,
          }, :user_data, :name, :image_id, :instance_type, :security_groups, :key_name)
          t.empty? ? nil : t
        end.reject {|a| a.nil? }
        if differences.empty?
          false
        else
          puts "-----> Recreating the launch configuration as details have changed: #{differences.inspect}"
          true
        end
      end
    end
    def create_launch_configuration!
      puts "-----> Creating launch configuration: #{cloud.proper_name}"
      begin
        # as.delete_autoscaling_group(:autoscaling_group_name => cloud.proper_name)
        # as.delete_launch_configuration(:launch_configuration_name => cloud.proper_name)
        as.create_launch_configuration({
          :launch_configuration_name => cloud.proper_name,
          :image_id => parent.image_id,
          :instance_type => parent.instance_type,
          :security_groups => parent.security_groups,
          :key_name => cloud.keypair,
          :user_data => parent.user_data,
          :kernel_id => parent.kernel_id,
          :ramdisk_id => parent.ramdisk_id,
          :block_device_mappings => parent.block_device_mappings
        })
      rescue Exception => e
        puts <<-EOE
-----> There was an error: #{e.inspect} when creating the launch_configurations
        EOE
      end
    end
    def launch_configurations
      @launch_configurations ||= as.describe_launch_configurations.DescribeLaunchConfigurationsResult.LaunchConfigurations.member.map do |a|
        {
          :name => a["LaunchConfigurationName"],
          :ramdisk_id => a["RamdiskId"],
          :image_id => a["ImageId"],
          :security_groups => (a["SecurityGroups"]["member"] rescue ["default"]),
          :created_time => a["CreatedTime"],
          :user_data => a["UserData"] || "",
          :keypair => a["KeyName"],
          :instance_type => a["InstanceType"]
        }
      end
    end
    def create_autoscaling_group
      as.delete_autoscaling_group(:autoscaling_group_name => cloud.proper_name) rescue nil
      as.create_autoscaling_group({
        :autoscaling_group_name => cloud.proper_name,
        :availability_zones => parent.availability_zones,
        :launch_configuration_name => cloud.proper_name,
        :min_size => parent.minimum_instances,
        :max_size => parent.maximum_instances,
        :load_balancer_names => parent.load_balancers.map {|k,v| k }
      })
    end
    def autoscaling_groups
      as.describe_autoscaling_groups.DescribeAutoScalingGroupsResult.AutoScalingGroups.member.map do |g|
        {
          :cooldown => g["Cooldown"],
          :desired_capacity => g["DesiredCapacity"],
          :created_time => g["CreatedTime"],
          :min_size => g["MinSize"],
          :max_size => g["MaxSize"],
          :load_balancer_names => (g["LoadBalancerNames"]["member"] rescue []),
          :availabilityZones => (g["AvailabilityZones"]["member"] rescue []),
          :launch_configuration_name => g["LaunchConfigurationName"],
          :name => g["AutoScalingGroupName"],
          :instances => (g["Instances"]["member"] rescue []).map {|i|
            {:instance_id => i["InstanceId"],
            :state => i["LifecycleState"],
            :availability_zone => i["AvailabilityZone"]
          }}
        }
      end
    end
    
  end
end
module PoolParty
  class Base
    include Dslify
    attr_reader :name
    def initialize(name, o={}, &block)
      @name = name
      @init_opts = o
      set_vars_from_options(o)
      instance_eval &block if block
      after_initialized
    end
    def after_initialized
    end
    def run
      warn "#{self.class} does not implement run. Something is wrong"
    end
    private
  end
  class Pool < Base
    attr_accessor :verbose, :very_verbose, :debugging, :very_debugging
    def cloud(name, &block)
      clouds[name] = Cloud.new(name, {:parent => self}, &block)
    end
    def clouds
      @clouds ||= {}
    end
    def run
      clouds.each do |cloud_name, cld|
        puts "---- Starting to build cloud #{cloud_name}"
        cld.run
      end
    end
  end
  class Cloud < Base
    default_options(:keypair => nil)
    def load_balancer(name, o={}, &block);load_balancers[name] = [name, o, block];end
    def load_balancers;@load_balancers ||= {};end
    
    def autoscale(name, o={}, &block);autoscales[name] = [name, o, block];end
    def autoscales;@autoscales ||= {};end
    
    attr_reader :cloud_provider
    def using(provider_name, &block)
      return @cloud_provider if @cloud_provider
      @cloud_provider = "#{provider_name}".constantize(CloudProviders).send :new, provider_name, :cloud => self, &block
    end
    def run
      puts "  running on #{cloud_provider.class}"
      load_balancers.each do |lb_name, lb|
        cloud_provider.load_balancer(*lb)
      end
      autoscales.each do |as_name, as|
        cloud_provider.autoscale(*as)
      end
      cloud_provider.run
    end
    
    def proper_name
      "#{parent.name}-#{name}"
    end
  end
end


pool "skinnytest2" do
  
  cloud "app" do
    load_balancer "mapA", :external_port => 8000 do
      internal_port '81'
    end
    load_balancer "mapB", :external_port => 443 do
      internal_port 8443
    end
    autoscale "a"
    using :ec2 do
      security_group "test_cloud" do
        revoke :from_port => "8080", :to_port => "8081"
        authorize :from_port => "22", :to_port => "22"
      end
      minimum_instances 1
      maximum_instances 1
    end
  end
  
end
pool.run