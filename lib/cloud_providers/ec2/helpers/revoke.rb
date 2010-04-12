module CloudProviders

  class Revoke < Ec2Helper
    default_options({
                :protocol => "tcp",
                :from_port => "22",
                :to_port => "22",
                :cidr_ip => "0.0.0.0/0"})
    def run
      puts "Revoking: #{name} for #{protocol} to #{from_port}:#{to_port} #{network}"
      options = { :group_name => name,
                  :ip_protocol => protocol,
                  :from_port => from_port,
                  :to_port => to_port,
                  :cidr_ip => network}

      ec2.revoke_security_group_ingress(options) rescue nil
    end

    alias :network :cidr_ip

  end

end
