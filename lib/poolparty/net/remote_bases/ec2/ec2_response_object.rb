class EC2ResponseObject
  def self.get_descriptions(resp)      
    rs = get_response_from(resp)

    # puts rs.methods.sort - rs.ancestors.methods
    out = begin
      if rs.respond_to?(:instancesSet)
        [EC2ResponseObject.get_hash_from_response(rs.instancesSet.item)]
      else
        rs.collect {|r| 
          if r.instancesSet.item.class == Array
            r.instancesSet.item.map {|t| EC2ResponseObject.get_hash_from_response(t)}
          else
            [EC2ResponseObject.get_hash_from_response(r.instancesSet.item)]
          end            
        }.flatten.reject {|a| a.nil? }
      end
    rescue Exception => e
      # Really weird bug with amazon's ec2 gem
      rs.collect {|r| EC2ResponseObject.get_hash_from_response(r)}.reject {|a| a.nil? } rescue []
    end

    out
  end
  def self.get_response_from(resp)
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
  def self.get_hash_from_response(resp)      
    begin
      {
        :instance_id => resp.instanceId,
        :name => resp.instanceId, 
        :ip => resp.dnsName || "not-assigned",
        :status => resp.instanceState.name,
        :launching_time => resp.launchTime.parse_datetime,
        :internal_ip => resp.privateDnsName,
        :keypair => resp.keyName
        # :security_group => resp.groupSet.item[0].groupId
      }        
    rescue Exception => e
      nil
    end      
  end
end