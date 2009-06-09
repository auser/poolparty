# Convenience class to convert standard amazon-ec2 responses from their camel cased style to a hash using underscore style.
# For example: instanceId to instance_id
class EC2ResponseObject
  def self.get_descriptions(resp)          
    rs = get_instance_from_response(resp)
    group = get_group_from_response(resp)
    
    # puts rs.methods.sort - rs.ancestors.methods
    out = begin
      if rs.respond_to?(:instancesSet)
        [EC2ResponseObject.get_hash_from_response(rs.instancesSet.item, group)]
      else
        rs.collect {|r| 
          if r.instancesSet.item.class == Array
            r.instancesSet.item.map {|t| EC2ResponseObject.get_hash_from_response(t, group)}
          else
            [EC2ResponseObject.get_hash_from_response(r.instancesSet.item, group)]
          end            
        }.flatten.reject {|a| a.nil? }
      end
    rescue Exception => e
      # Really weird bug with amazon's ec2 gem
      rs.collect {|r| EC2ResponseObject.get_hash_from_response(r)}.reject {|a| a.nil? } rescue []
    end

    out
  end
  def self.get_instance_from_response(resp)
    begin
      rs = resp.reservationSet.item unless resp.reservationSet.nil?
      rs ||= resp.DescribeInstancesResponse.reservationSet.item
      rs ||= rs.respond_to?(:instancesSet) ? rs.instancesSet : rs
      rs.reject! {|a| a.nil? || a.empty? }
    rescue Exception => e
      resp
    end
    rs
  end
  def self.get_group_from_response(resp)
    begin
      resp = resp.reservationSet.item.first if resp.reservationSet.item.is_a?(Array)
      group = resp.reservationSet.item.groupSet.item.groupId unless resp.reservationSet.nil?
      group ||= resp.groupSet.item[0].groupId rescue nil
      group ||= resp.DescribeInstancesResponse.reservationSet.item.groupSet.item.groupId
      #rs ||= rs.respond_to?(:instancesSet) ? rs.instancesSet : rs
      #rs.reject! {|a| a.nil? || a.empty? }
    rescue Exception => e
      resp
    end
    group
  end
  def self.get_hash_from_response(resp, group = 'default')
      symbolize_and_snakecase
      {
        :instance_id => resp.instanceId,
        :name => resp.instanceId,
        :status => resp.instanceState.name,
        :public_ip => resp.dnsName || "not-assigned",
        :ip => resp.dnsName || "not-assigned",
        :internal_ip => resp.privateDnsName,
        :launching_time => resp.launchTime.parse_datetime,
        :keypair => (resp.keyName rescue ""),
        :security_group => group
      }
  end
  
  #####
  
  # Convert the standard reponse into output similar to this example
  # {:dns_name          =>"ec2-75-101-175-49.compute-1.amazonaws.com",
  #  :private_dns_name  =>"domU-11-31-39-00-DC-78.compute-1.internal",
  #  :reason            =>nil,
  #  :instance_state    =>{"name"=>"running", "code"=>"16"},
  #  :kernel_id         =>"aki-a71cf9ce",
  #  :ramdisk_id        =>"ari-a51cf9cc",
  #  :placement         =>{"availabilityZone"=>"us-east-1a"},
  #  :product_codes     =>nil,
  #  :image_id          =>"ami-bf5eb9d6",
  #  :launch_time       =>"2009-05-29T05:07:09.000Z",
  #  :key_name          =>"poolname_cloudname",
  #  :instance_id       =>"i-1b7b2942",
  #  :ami_launch_index  =>"0",
  #  :instance_type     =>"m1.small"}
  #
  # Selects the first instance if an index is not given.
  def self.describe_instance(response, index=0)
    inst=response['reservationSet']['item'].first['instancesSet']['item'][index]
    Ec2RemoteInstance.new(symbolize_and_snakecase(inst))
  end
  
  def self.describe_instances(response)
    return [] if response['reservationSet'].nil?
    ec2_insts = response['reservationSet']['item'].collect do |ri|
      ri['instancesSet']['item'].collect{|i| i}
    end
    ec2_insts.flatten.collect {|i| symbolize_and_snakecase(i) }
  end
  
  # Convert the standard response hash to format used throughout the rest of PoolParty code.
  # And add in some more values we rely on
  def self.symbolize_and_snakecase(inst)
    n = inst.symbolize_keys(:snake_case)
    n[:internal_ip] = convert_from_ec2_dns_to_ip(n[:private_dns_name])
    n[:public_ip]   = convert_from_ec2_dns_to_ip(n[:dns_name])
    n[:ip]          = n[:public_ip]
    n[:launch_time] = parse_datetime(n[:launch_time])
    n[:status]      = n[:instance_state][:name]
    n[:availability_zone] = n[:placement][:availability_zone]
    n
  end
  
  def self.convert_from_ec2_dns_to_ip(str)
    return nil if str.nil?
    str.scan(/-(\d{1,3})-(\d{1,3})-(\d{1,3})-(\d{1,3})/).flatten.join('.')
  end
  
  def self.parse_datetime(str)
    DateTime.parse( str.chomp ) rescue self
  end
  
end