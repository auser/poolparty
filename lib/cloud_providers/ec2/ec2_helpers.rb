module CloudProviders
  module Ec2Helpers
    
    # Are we running on amazon?
    def amazon?
      !['https://ec2.amazonaws.com', 
       'https://us-east-1.ec2.amazonaws.com', 
       'https://eu-west-1.ec2.amazonaws.com'
       ].include?(ec2_url)
    end
    
    # Read  yaml file and use it to set environment variables and local variables.
    def set_aws_env_from_yml_file(filename='/etc/poolparty/env.yml')
      aws = self.class.load_keys_from_file(filename)
      aws.each{|k,v| ENV[k.upcase]=v.to_s}
      set_vars_from_options aws
    end
    
    # Save aws keys and env variables to a yaml file
    def save_aws_env_to_yml(filename='/etc/poolparty/aws.yml')
      aws_values = {
        :user_id            => user_id,
        :private_key        => private_key,
        :cert               => cert,
        :access_key         => access_key,
        :secret_access_key  => secret_access_key,
        :ec2_url            => ec2_url,
        :s3_url             => s3_url,
        :eucalyptus_cert    => eucalyptus_cert
      }
      File.open(filename, 'w') {|f| f<<YAML::dump(aws_values) }
    end
    
    # Associate an address with the instance using ec2
    # Get the next_unused_elastic_ip
    # and if there is one, associate the instance to the 
    # public ip
    def associate_address()
      raise StandardError.new('Not Implemented Yet')
    end
    
    # Get the next usable elastic ip
    # First, get the list of addresses from ec2 that the client
    # has access to, then select only the ones that are not associated
    # with an instance.
    # If the cloud has set elastic_ips to use, then, using the 
    # intersection of the unused ips and those, find the first one available
    # and return that.
    def next_unused_elastic_ip
      raise StandardError.new('Not Implemented Yet')
      if elastic_ips.empty?
        nil
      else
      end
    end

    # Help create a keypair for the cloud
    # This is a helper to create the keypair and add them to the cloud for you
    # def create_keypair
    #   return false unless keypair
    #   unless ::File.exists?( new_keypair_path )
    #     FileUtils.mkdir_p ::File.dirname( new_keypair_path )
    #     vputs "Creating keypair: #{keypair} in #{new_keypair_path}"
    #     Kernel.system "ec2-add-keypair #{keypair} > #{new_keypair_path} && chmod 600 #{new_keypair_path}"
    #   end
    # end
    
    # wrapper for remote base to perform a snapshot backup for the ebs volume
    # def create_snapshot
    #   return nil if ebs_volume_id.nil?
    #   ec2.create_snapshot(:volume_id => ebs_volume_id)
    # end
    
  end
end