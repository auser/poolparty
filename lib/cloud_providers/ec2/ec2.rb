=begin rdoc
  EC2 CloudProvider
  This serves as the basis for running PoolParty on Amazon's ec2 cloud.
=end

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
    
    # Set the aws keys from the environment, or load from /etc/poolparty/aws.yml if the environment variable is not set
    def self.default_access_key
      ENV['EC2_ACCESS_KEY'] || load_keys_from_file[:access_key]
    end
    
    def self.default_secret_access_key
      ENV['EC2_SECRET_KEY'] || load_keys_from_file[:access_key]
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
    
    def self.load_keys_from_file(filename='/etc/poolparty/aws.yml')
      raise StandardError.new("#{filename} does not exist") if File.exists? filename
      @keys ||= YAML::load( open(filename).read )
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
        :access_key             => default_access_key,
        :secret_access_key      => default_secret_access_key,
        :min_count              => 1,
        :max_count              => 1,
        :user_data              => '',
        :addressing_type        => nil,
        :kernel_id              => nil,
        :ramdisk_id             => nil,
        :availability_zone      => nil,
        :block_device_mappings  => nil,
        :elastic_ips            => nil, # An array of the elastic ips
        :ebs_volume_id          => nil  # The volume id of an ebs volume
      })

    def ec2(o={})
      @ec2 ||= Rightscale::Ec2.new(access_key, secret_access_key, o.merge(:logger => PoolParty::PoolPartyLog))
    end
    
    # Start a new instance with the given options
    def run_instance(o={})
      set_vars_from_options o
      keypair_name ||= o[:keypair_name] || keypair || (clouds[o[:cloud_name]].keypair.basename if o[:cloud_name])
      raise "You must pass a keypair to launch an instance, or else you will not be able to login. options = #{o.inspect}" if !keypair_name        
      response_array = ec2(o).run_instances(image_id,
                                      min_count,
                                      max_count,
                                      security_group,
                                      key_name,
                                      user_data,
                                      addressing_type,
                                      instance_type,
                                      kernel_id,
                                      ramdisk_id,
                                      availability_zone,
                                      block_device_mappings
                                      )
      instances = response_array.collect do |aws_response_hash|
         Ec2RemoteInstance.new( Ec2Response.pp_format(aws_response_hash) )
      end
      #FIXME: This needs to deal with the case when an array is returned if max_instances > 1
      instances.first
    end
    
  end
end