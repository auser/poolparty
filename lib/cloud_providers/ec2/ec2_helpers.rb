module CloudProviders
  module Ec2Helpers
    
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