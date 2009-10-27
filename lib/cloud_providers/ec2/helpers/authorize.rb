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
      puts "Authorizing: #{cloud.proper_name} for #{protocol} to #{from_port}:#{to_port} #{network}"
      options = 
      if group_name
        {:authorize_security_group_ingress => group_name, :source_security_group_owner_id => owner_id}
      else
        to_hash
      end
      ec2.authorize_security_group_ingress(options) rescue nil
    end
    
    def to_hash
      {
        :group_name   => cloud.proper_name,
        :ip_protocol  => protocol,
        :from_port    => from_port,
        :to_port      => to_port,
        :cidr_ip      => network
      }
    end
    
  end
end