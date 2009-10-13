module CloudProviders
  class ElasticLoadBalancer < Ec2
    default_options(
      :listeners => []
    )
    def run
      if should_create_load_balancer?
        puts "-----> Creating ElasticLoadBalancer: #{name}"
        create_load_balancer!
      end
    end
    
    def teardown
      puts "-----> Tearing down load balancer: #{name}"
      elb.delete_load_balancer(:load_balancer_name => name)
    end
    
    def listener(*listener_hashes)
      listener_hashes.each do |hsh|
        _listeners << ElasticListener.new(hsh)
      end
    end
    
    private
    def _listeners
      @_listeners ||= []
    end
    def real_name
      name
    end
    public 
    
    def should_create_load_balancer?
      elastic_load_balancers.select {|lb| lb.name == name }.empty?
    end
    def create_load_balancer!
      p elb.delete_load_balancer(:load_balancer_name => name)
      p elb.create_load_balancer(
        :availability_zones => availability_zones,
        :load_balancer_name => real_name,
        :listeners => _listeners.map {|l| l.to_hash }
      )
    
    end
    def elastic_load_balancers
      @elastic_load_balancers ||= elb.describe_load_balancers.DescribeLoadBalancersResult.LoadBalancerDescriptions.member.map do |lb|
        {
          :created_time => lb["CreatedTime"],
          :availability_zones => (lb["AvailabilityZones"]["member"] rescue []),
          :dns_name => lb["DNSName"],
          :name => lb["LoadBalancerName"],
          :instances => (g["Instances"]["member"] rescue []).map {|i| {:instance_id => i["InstanceId"]}},
          :health_check => ([lb["HealthCheck"]] rescue []).map do |hc|
            {
              :healthy_threshold => hc["HealthyThreshold"],
              :timeout => hc["Timeout"],
              :unhealthy_threshold => hc["UnhealthyThreshold"],
              :interval => hc["Interval"],
              :target => hc["Target"]
            }
          end,
          :listeners => (lb["Listeners"]["member"] rescue []).map do |listener|
            {
              :instance_port => listener["InstancePort"],
              :protocol => listener["Protocol"],
              :load_balancer_port => listener["LoadBalancerPort"]
            }
          end
        }
      end
    end
  end
  class ElasticListener < Ec2
    default_options(
      :internal_port => 8080,
      :external_port => 80,
      :protocol => "http"
    )
    
    def to_hash
      {:protocol => protocol, :load_balancer_port => external_port.to_s, :instance_port => internal_port.to_s}
    end
  end
end