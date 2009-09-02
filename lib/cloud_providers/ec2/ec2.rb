=begin rdoc
  EC2 CloudProvider
  This serves as the basis for running PoolParty on Amazon's ec2 cloud.
=end
require "openssl"
if OpenSSL::OPENSSL_VERSION_NUMBER < 0x00908000
  warn "the ec2 cloud provider may not work with your version of ruby and OpenSSL.  Consider upgrading if you encoutner authentication errors."
end
begin
  require 'right_aws'
rescue LoadError
  puts <<-EOM
Error: In order to use ec2, you need to install the right_aws gem

Ec2 is the default cloud provider for PoolParty. If you intend on using
a different provider, specify it with:

using :provider_name
EOM
end

require "#{File.dirname(__FILE__)}/ec2_response"
require "#{File.dirname(__FILE__)}/ec2_instance"

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
    def self.load_keys_from_file(filename='/etc/poolparty/env.yml', caching=true)
      return @aws_yml if @aws_yml && caching==true
      return {} unless File.exists?(filename)
      ddputs("Reading keys from file: #{filename}")
      @aws_yml = YAML::load( open(filename).read )
    end
    
    default_options({
        :image_id               => 'ami-bf5eb9d6',
        :instance_type          => 'm1.small',
        :addressing_type        => "public",
        :availability_zone      => "us-east-1a",
        :security_group         => ["default"],
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
        :availability_zone      => nil,
        :block_device_mappings  => nil,
        :elastic_ips            => nil, # An array of the elastic ips
        :ebs_volume_id          => nil  # The volume id of an ebs volume # TODO: ensure this is consistent with :block_device_mappings
      })
      
      
    def ec2(o={})
      @ec2 ||= Rightscale::Ec2.new(access_key, secret_access_key, o.merge(:logger => PoolParty::PoolPartyLog, :default_host => ec2_url))
    end
    
    # Start a new instance with the given options
    def run_instance(o={})
      set_vars_from_options o
      raise StandardError.new("You must pass a keypair to launch an instance, or else you will not be able to login. options = #{o.inspect}") if !keypair_name
      response_array = ec2(o).run_instances(image_id,
                                      min_count,
                                      max_count,
                                      security_group,
                                      keypair.basename,
                                      user_data,
                                      addressing_type,
                                      instance_type,
                                      kernel_id,
                                      ramdisk_id,
                                      availability_zone,
                                      block_device_mappings
                                      )
      instances = response_array .collect do |aws_response_hash|
        Ec2Instance.new( Ec2Response.pp_format(aws_response_hash).merge(o) )
      end
      #FIXME: This needs to deal with the case when an array is returned if max_instances > 1
      instances.first
    end
    
    # Will select the first instance matching the provided criteria hash
    def describe_instance(hash_of_criteria_to_select_instance_against)
      describe_instances(hash_of_criteria_to_select_instance_against).first
    end
    
    # Describe instances
    def describe_instances(o={})
      instants = Ec2Response.describe_instances(ec2.describe_instances).select_with_hash(o)
      return [] if instants.empty?
      ec2_instances = instants.collect{|i| Ec2Instance.new(dsl_options.merge(i))}
      ec2_instances.sort {|a,b| a[:launch_time].to_i <=> b[:launch_time].to_i }
    end
    
    # Terminate an instance (or instances) by passing :instance_id and :instance_ids
    def terminate_instance!(o={})
      raise StandardError.new("You must pass an instance_id when terminating an instance with ec2") unless o[:instance_id] || o[:instance_ids]
      instance_ids = o[:instance_ids] || [o[:instance_id]]
      response = ec2.terminate_instances(instance_ids)
      response.collect{|i| Ec2Instance.new(Ec2Response.pp_format(i)) }
    end
    
    
=begin rdoc
  Helper methods for the Ec2 Cloud Provider. Helpers are not necessarily supported across all CloudProviders
=end
    # Are we running on amazon?
    def amazon?
      !['https://ec2.amazonaws.com', 
       'https://us-east-1.ec2.amazonaws.com', 
       'https://eu-west-1.ec2.amazonaws.com'
       ].include?(ec2_url)
    end
    
    # Callbacks
    def before_compile(cld)
    end
    
    def after_compile(cld)
      save_aws_env_to_yml(cld.tmp_path/"etc"/"poolparty"/"env.yml")
    end
    
    # Read  yaml file and use it to set environment variables and local variables.
    def set_aws_env_from_yml_file(filename='/etc/poolparty/env.yml')
      aws = self.class.load_keys_from_file(filename)
      aws.each{|k,v| ENV[k.upcase]=v.to_s}
      set_vars_from_options aws
    end
    
    # Save aws keys and env variables to a yaml file
    def save_aws_env_to_yml(filename='/etc/poolparty/env.yml')
      File.open(filename, 'w') {|f| f<<YAML::dump(aws_hash(dsl_options, "/etc/poolparty/ec2")) } rescue nil
    end
    
    # Return a hash of the aws keys and environment variables
    # If base_dir string is provided as second argument, replace path to 
    # file based variables, such as cert, with the base_dir.
    def aws_hash(opts={}, base_dir=nil)
      aws={
        :user_id            => user_id,
        :private_key        => private_key,
        :cert               => cert,
        :access_key         => access_key,
        :secret_access_key  => secret_access_key,
        :ec2_url            => ec2_url,
        :s3_url             => s3_url,
        :cloud_cert    => cloud_cert
      }.merge(opts)
      if base_dir
        aws[:cert] = "#{base_dir}/#{File.basename(cert)}" if cert
        aws[:private_key] = "#{base_dir}/#{File.basename(private_key)}" if private_key
        aws[:cloud_cert] = "#{base_dir}/#{File.basename(cloud_cert)}" if cloud_cert
      end
      aws.reject{|k,v| v.nil?}
    end
    
    # shortcut to 
    # ec2-add-keypair name > ~./.ec2/kname
    def create_keypair(kname, path='~/.ec2')
      ` ec2-add-keypair #{kname} > #{path}/#{kname} &&  chmod 600 #{path}/#{kname}`
    end
    
  end
end