class EC2ResponseObject
  def self.get_descriptions(resp)    
    item_resp = get_response_from(resp)

    item_resp.collect do |items|
      item = parse_items(items)
      groupSet = item.first.groupSet
      group = parse_tag(groupSet.item)#.first["groupId"]
      puts "groupId: #{parse_tag(groupSet.item)}"
      instancesSet = item.first.instancesSet
      begin
        instancesSet.item.collect do |instance|
          [EC2ResponseObject.get_hash_from_response(instance, group)]
        end
      rescue Exception => e
        puts "Error: #{e}"
        rs.collect {|r| EC2ResponseObject.get_hash_from_response(r, rs)}.reject {|a| a.nil? } rescue []
      end.flatten.reject {|a| a.nil? }      
    end
  end
  def self.parse_items(item_resp)
    item_resp.collect do |i|
      i.is_a?(Array) ? i.first.is_a?(Hash) ? i.first : parse_items(i.first) : nil
    end.reject {|a| a.nil? }
  end
  def self.get_response_from(resp)
    begin
      rs = resp.reservationSet unless resp.reservationSet.nil? rescue nil
      rs ||= resp.DescribeInstancesResponse.reservationSet rescue nil
      rs ||= resp.RunInstancesResponse
      rs.each {|arr| arr.reject! {|i| !i.is_a?(OpenStruct) }}
    rescue Exception => e
      rs = resp
    end
    rs
  end
  def self.get_hash_from_response(resp, group)
    begin
      {
        :instance_id => resp.instanceId,
        :name => resp.instanceId, 
        :ip => resp.dnsName || "not-assigned",
        :status => resp.instanceState.name,
        :launching_time => resp.launchTime.parse_datetime,
        :internal_ip => resp.privateDnsName,
        :keypair => resp.keyName,
        :security_group => group
      }        
    rescue Exception => e
      puts "Error: #{e}"
      nil
    end     
  end
end
class DescribeResponse
  
end