module CloudProviders
  class Authorize < Ec2Helper
    default_options({
                :protocol   => "tcp",
                :from_port  => "22",
                :to_port    => "22",
                :network    => "0.0.0.0/0",
                :group_name => nil,
                :owner_id   => nil})
    
    def run
      options = 
      if group_name
        puts "Authorizing #{name} for group named: #{group_name} of owner id: #{owner_id}"
        {:group_name => name, :source_security_group_name=> group_name, :source_security_group_owner_id => owner_id}
      else
        puts "Authorizing: #{name} for #{protocol} to #{from_port}:#{to_port} #{network}"
        to_hash
      end
      begin
        ec2.authorize_security_group_ingress(options) 
      rescue AWS::InvalidPermissionDuplicate => e
        nil
      end
        
    end
    
    def to_hash
      if group_name
        {:group_name => group_name}
      else
        {
          :group_name   => name,
          :ip_protocol  => protocol,
          :from_port    => from_port,
          :to_port      => to_port,
          :cidr_ip      => network
        }
      end
    end
    
  end
end
