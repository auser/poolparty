module CloudProviders

  class SecurityGroup < Ec2
    def run
      if should_create_security_group?
        create_security_group! rescue nil
      end
      authorizes.each {|a| a.run }
      revokes.each {|a| a.run }
    end
    def authorize(o={}, &block)
      authorizes << Authorize.new("#{name}", o.merge(:parent => parent, :cloud => cloud), &block)
    end
    def revoke(o={}, &block)
      revokes << Revoke.new("#{name}", o.merge(:parent => parent, :cloud => cloud), &block)
    end
    def create_security_group!
      ec2.create_security_group(:group_name => cloud.proper_name, :group_description => "PoolParty generated security group: #{cloud.proper_name}")
    end
    def should_create_security_group?
      security_groups.select {|sg| sg[:name] == cloud.proper_name }.empty?
    end
    def security_groups
      @security_groups ||= ec2.describe_security_groups.securityGroupInfo.item.map do |sg|
        perms = sg["ipPermissions"] || {"item" => []} rescue [{"item" => []}]
        {
          :name => sg["groupName"],
          :description => sg["groupDescription"],
          :ip_permissions => perms["item"].map do |i|
            ip_ranges = i["ipRanges"] || {"item" => []} rescue {"item" => []}
            {
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