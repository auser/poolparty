module CloudProviders
  class SecurityGroup < Ec2Helper
    
    def run
      if should_create_security_group?
        create_security_group!
      end
      current_security_groups = security_groups.map {|a| 
        a[:ip_permissions].map do |perm|
          if perm[:ip_ranges].size > 1
            perm[:ip_ranges].map do |range|
              {
                :group_name => a[:name],
                :from_port => perm[:from_port],
                :to_port => perm[:to_port],
                :cidr_ip => range,
                :ip_protocol => perm[:protocol]
              }.flatten
            end.flatten
          else
            {
              :group_name => a[:name],
              :from_port => perm[:from_port], 
              :to_port => perm[:to_port],
              :cidr_ip => perm[:ip_ranges].map {|c| c[:cidrIp] }.first, # first for simplicity for now...
              :ip_protocol => perm[:protocol]
            }
          end
        end.flatten
      }.flatten
      
      authorizers = []
      authorizes.each do |a|
        unless current_security_groups.include?(a.to_hash)
          authorizers << a
        end
      end
      
      defined_security_group_hashes = authorizes.map {|a| a.to_hash}
      
      current_security_groups.each do |hsh|
        unless defined_security_group_hashes.include?(hsh)
          revoke(hsh.merge(:protocol => hsh[:ip_protocol]))
        end
      end
      
      revokes.each {|r| r.run }
      authorizers.each {|a| a.run}
    end
    def authorize(o={}, &block)
      authorizes << Authorize.new("#{name}", o.merge(:parent => parent, :cloud => cloud), &block)
    end
    def revoke(o={}, &block)
      revokes << Revoke.new("#{name}", o.merge(:parent => parent, :cloud => cloud), &block)
    end
    def create_security_group!
      ec2.create_security_group(:group_name => name, :group_description => "PoolParty generated security group: #{name}")
    end
    def should_create_security_group?
      security_groups.empty?
    end
    def security_groups
      @security_groups ||= all_security_groups.select {|sg| sg[:name] == name }
    end
    def all_security_groups
      @all_security_groups ||= ec2.describe_security_groups.securityGroupInfo.item.map do |sg|
        perms = sg["ipPermissions"] || {"item" => []} rescue [{"item" => []}]
        {
          :name => sg["groupName"],
          :description => sg["groupDescription"],
          :ip_permissions => perms["item"].map do |i|
            ip_ranges = i["ipRanges"] || {"item" => []} rescue {"item" => []}
            {
              :protocol => i["ipProtocol"],
              :from_port => i["fromPort"],
              :to_port => i["toPort"],
              :ip_ranges => ip_ranges["item"].map do |ip|
                {
                  :cidrIp => ip["cidrIp"]
                }
              end
            }
          end
        }
      end
    end
    def to_s
      name
    end
    def authorizes
      @authorizes ||= []
    end
    def revokes
      @revokes ||= []
    end
  end

end