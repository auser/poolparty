$:.unshift("#{File.dirname(__FILE__)}/../lib")
require "rubygems"
require "poolparty"

pool "skinnytest2" do
  
  cloud "app" do
    chef_repo "/Users/auser/Development/work/chacha/chef-repo"
    recipe "ganglia::default"
    recipe "ganglia::gmetad"
    recipe "ganglia::monitor_sshd"
    chef_attributes :collectd => {:server => "10.208.114.145"},
                    :rsyslog  => {:server => "ec2.monitor.chacha.metavirt.com",  :protocol=>'udp'},
                    :apache2 => {:listen_ports => ["80", "443"]}

    
    load_balancer do
      listener :external_port => 8082, :internal_port => 82, :protocol => 'tcp'
    end

    autoscaler do
      cooldown "30"
    end
    using :ec2 do
      security_group do
        revoke :from_port => "8080", :to_port => "8081"
        authorize :from_port => "22", :to_port => "22"
      end
      user_data <<-EOE
#!/bin/bash -x
echo "New User Data! ho."    
      EOE
  
      instances 1..1
    end
  end
end
