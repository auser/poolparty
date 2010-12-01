module CloudProviders
  class SecurityGroup < Ec2Helper

    def merge! init_opts, &block
      set_vars_from_options(init_opts)
      instance_eval &block if block
    end

    def run
      if should_create_security_group?
        create_security_group!
      end
      current_security_groups = security_groups.map {|a|
        # Example:
        #    [{ :ip_permissions=>[
        #       {:ip_ranges=>[{:cidr_ip=>"0.0.0.0/0"}], :from_port=>"22", :protocol=>"tcp", :to_port=>"22"},
        #       {:ip_ranges=>[{:cidr_ip=>"0.0.0.0/0"}], :from_port=>"80", :protocol=>"tcp", :to_port=>"80"} ],
        #     :description=>"PoolParty generated security group: clyde-chefclient", :name=>"clyde-chefclient"}]
        #
        a[:ip_permissions].map do |perm|
          if perm[:group_name]
            {
              :group_name => perm[:group_name]
            }
          else
            (perm[:ip_ranges] || ["0.0.0.0/0"]).map do |range|
              range = range[:cidr_ip] if range.is_a?(Hash)
              {
                :group_name  => a[:name],
                :from_port   => perm[:from_port].to_i,
                :to_port     => perm[:to_port].to_i,
                :cidr_ip     => range,
                :ip_protocol => perm[:protocol]
              }
            end.flatten
          end
        end.flatten
      }.flatten.uniq
      # p ['running', name, 'with', authorizes.map{|a| a.to_hash}, 'currently', current_security_groups]

      authorizes_requested        = authorizes.select{|a| a.name.to_s == name.to_s }
      authorizes_requested_hashes = authorizes_requested.map{|a| a.to_hash}
      authorizes_needed = []

      # take each requested authorization. If it doesn't exist in the current_security_groups, we need to add it.
      authorizes_requested.each do |a|
        next if current_security_groups.include?(a.to_hash)
        p ['authorize needed:', name, a.to_hash]
        authorizes_needed << a
      end
      # conversely, every current_security_groups authorization that isn't in the authorizes_requested list must be revoked
      current_security_groups.each do |hsh|
        next if authorizes_requested_hashes.include?(hsh)
        if name == 'default'
          p ['Not revoking, but please check that this security opening is valid:', name, hsh]
        else
          p ['revoke needed:', name, hsh]
          revoke(hsh.merge(:protocol => hsh[:ip_protocol]))
        end
      end

      maybe 'revoke security group settings' do
        revokes.each{|r| r.run }
      end unless revokes.blank?
      if not authorizes_needed.blank?
        authorizes_needed.each{|a| a.run}
      end
    end
    def authorize(o={}, &block)
      authorizes << Authorize.new("#{name}", o.merge(:parent => parent, :cloud => cloud), &block)
    end
    def revoke(o={}, &block)
      revokes << Revoke.new("#{name}", o.merge(:parent => parent, :cloud => cloud), &block)
    end
    def create_security_group!
      $stderr.puts "creating security group #{name}"
      ec2.create_security_group(:group_name => name, :group_description => "PoolParty generated security group: #{name}")
    end
    def should_create_security_group?
      security_groups.empty?
    end
    def security_groups
      @security_groups ||= all_security_groups.select{|sg| sg[:name] == name }
    end
    def all_security_groups
      @all_security_groups ||= self.class.all_security_groups(ec2)
    end
    def self.all_security_groups(ec2)
      @all_security_groups ||= ec2.describe_security_groups.securityGroupInfo.item.map do |sg|
        perms = sg["ipPermissions"] || {"item" => []} rescue [{"item" => []}]
        {
          :name => sg["groupName"],
          :description => sg["groupDescription"],
          :ip_permissions => perms["item"].map do |i|
            ip_ranges = i["ipRanges"] || {"item" => []} rescue {"item" => []}
            hsh = {
              :protocol => i["ipProtocol"],
              :from_port => i["fromPort"],
              :to_port => i["toPort"],
              :ip_ranges => ip_ranges["item"].map do |ip|
                {
                  :cidr_ip => ip["cidrIp"]
                }
              end
            }
            if i["groups"].blank?
              hsh
            else
              i["groups"]["item"].map{|grp| hsh.merge :group_name => grp["groupName"] }
            end
          end.flatten
        }
      end
    end
    def to_s
      name.to_s
    end
    def authorizes
      @authorizes ||= []
    end
    def revokes
      @revokes ||= []
    end
  end

end
