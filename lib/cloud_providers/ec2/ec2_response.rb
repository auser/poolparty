module CloudProviders
  
  # Convert the standard right_aws gem's response object to the poolparty format
  class Ec2Response
    
    # The right_aws gem returns a response similar to the following:
    
    # [{:aws_product_codes=>[],
    #   :dns_name=>"192.168.4.198",
    #   :aws_state_code=>"16",
    #   :private_dns_name=>"192.168.4.198",
    #   :aws_reason=>"",
    #   :aws_instance_type=>"m1.large",
    #   :aws_owner=>"admin",
    #   :ami_launch_index=>"0",
    #   :aws_launch_time=>"2009-07-11T03:21:02.113Z",
    #   :aws_reservation_id=>"r-3753070E",
    #   :aws_kernel_id=>"eki-AEAC17DE",
    #   :ssh_key_name=>"sample_keypair",
    #   :aws_state=>"running",
    #   :aws_groups=>["default"],
    #   :aws_ramdisk_id=>"eri-17051928",
    #   :aws_instance_id=>"i-3E90078F",
    #   :aws_availability_zone=>"jordan",
    #   :aws_image_id=>"emi-39921602"}]
    
    # Convert the standard reponse into output similar to this example
    
    # {:dns_name          =>"ec2-75-101-175-49.compute-1.amazonaws.com",
    #  :private_dns_name  =>"domU-11-31-39-00-DC-78.compute-1.internal",
    #  :reason            =>nil,
    #  :instance_state    =>{"name"=>"running", "code"=>"16"},
    #  :kernel_id         =>"aki-a71cf9ce",
    #  :ramdisk_id        =>"ari-a51cf9cc",
    #  :product_codes     =>nil,
    #  :image_id          =>"ami-bf5eb9d6",
    #  :launch_time       =>"2009-05-29T05:07:09.000Z",
    #  :key_name          =>"poolname_cloudname",
    #  :instance_id       =>"i-1b7b2942",
    #  :launch_index      =>"0",
    #  :instance_type     =>"m1.small"}
    
    # Selects the first instance if an index is not given.
    def self.describe_instance(response_array, index=0)
      pp_format(response_array[index])
    end
    
    # Convert the standard response hash to format used throughout the rest of PoolParty code.
    def self.describe_instances(response)
      response.collect{|inst| pp_format(inst) }
    end
    
    def self.pp_format(response_hash)
      munged = {}
      response_hash.each{|k,v| munged[k.to_s.gsub('aws_', '').to_sym] = v}
      munged[:internal_ip]      = convert_from_ec2_dns_to_ip(munged[:private_dns_name])
      munged[:public_ip]        = convert_from_ec2_dns_to_ip(munged[:dns_name])
      munged[:launch_time]      = parse_datetime(munged[:launch_time])
      munged[:ami_launch_index] = munged[:launch_index]  #TODO: deprecate ami_launch_index
      munged[:key_name]         = munged.delete(:ssh_key_name)
      munged[:status]           = munged.delete(:state)
      munged
    end
    
    # parse IP address from aws dns name
    def self.convert_from_ec2_dns_to_ip(str=nil)
      return str if str.nil?
      # if using eucalyptus we may have the raw IP already, if so just return it
      return str if str.match( /^\d{1,3}\.\d{1,3}.\d{1,3}\.\d{1,3}$/ )
      str.scan(/-(\d{1,3})-(\d{1,3})-(\d{1,3})-(\d{1,3})/).flatten.join('.')
    end
    
    def self.parse_datetime(str)
      Time.parse( str.chomp )
    end
    
  end
  
end
