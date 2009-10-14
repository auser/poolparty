module CloudProviders
  class ElasticLoadBalancer < Ec2
    default_options(
      :listeners => []
    )
    def run
      if should_create_load_balancer? || true
        puts "-----> Creating ElasticLoadBalancer: #{name}"
        create_load_balancer!
      elsif should_update_load_balancer?
      end
      _health_checks.each do |ck|
        create_health_check!(ck)
      end
      detach_instances_if_necessary
      attach_instances_if_necessary
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
    
    def health_check(*health_check_hashes)
      health_check_hashes.each do |hsh|
        _health_checks << HealthCheck.new(hsh)
      end
    end
    
    private
    def _listeners
      @_listeners ||= []
    end
    def _health_checks
      @_health_checks ||= []
    end
    def real_name
      name
    end
    public 
    def attach_instances_if_necessary
      instances = parent.nodes.map {|a| {:instance_id => a.instance_id} }
      elb.register_instances_with_load_balancer(:instances => instances, :load_balancer_name => "#{name}")
    end
    def detach_instances_if_necessary
      instances = parent.all_nodes.select {|a| !a.running? }.map {|a| {:instance_id => a.instance_id} }
      elb.deregister_instances_from_load_balancer(:instances => instances, :load_balancer_name => "#{name}") unless instances.empty?
    end
    def should_create_load_balancer?
      elastic_load_balancers.select {|lb| lb.name == name }.empty?
    end
    def create_load_balancer!
      elb.delete_load_balancer(:load_balancer_name => name)
      elb.create_load_balancer(
        :availability_zones => parent.availability_zones,
        :load_balancer_name => real_name,
        :listeners => _listeners.map {|l| l.to_hash }
      )
    end
    def create_health_check!(hc)
      elb.configure_health_check(:health_check => hc.to_hash, :load_balancer_name => name)
    end
    def should_update_load_balancer?
      known = elastic_load_balancers.select {|lc| lc.name =~ /#{name}/ }
      if known.empty?
        true
      else
        differences = _listeners.map do |listener|
          known.map {|a| listener.diff(a.listeners.first) } # TODO: enable multiple listeners
        end
        if differences.empty?
          false
        else
          true
        end
      end
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
  class HealthCheck < Ec2
    default_options(
      :target => "HTTP:80/",
      :interval => 5,
      :timeout => 3,
      :unhealthy_threshold => 2,
      :healthy_threshold => 2
    )
    def initialize(name, init_opts={}, &block)
      set_vars_from_options(name)
      super
    end
    
    def to_hash
      {   :target => target, 
          :interval => interval.to_s, 
          :timeout => timeout.to_s, 
          :unhealthy_threshold => unhealthy_threshold.to_s, 
          :healthy_threshold => healthy_threshold.to_s}
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
    
    def diff(hsh={})
      hsh.select {|k,v| hsh[k] != self.send(k) }
    end
    def load_balancer_port; external_port; end
    def instance_port; internal_port; end
    
  end
end