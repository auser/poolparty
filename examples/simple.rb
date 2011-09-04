# the following three lines are only necessary if you want to irb -r thisfile
$:.unshift("#{File.dirname(__FILE__)}/../lib")
require "rubygems"
require "poolparty"

pool "poolparty" do
  
  cloud "simple" do
    instances 1..3
    using :ec2
    # autoscale do      
    #   trigger :lower_threshold => 0.3, :upper_threshold => 1.0, :measure => :cpu
    # end
    image_id "ami-ccf615a5" #alestic jaunty
    availability_zones ['us-east-1b']
    #TODO: accept array of hashes defining security group rules
    # security_gropup [
    #   {:port=>22, :protocol=>'tcp' },
    #   {:port=>80, :protocol=>'tcp'  :source=>'10.0.0.0/8', :group=>'monitor'},
    #   {:port=>3000..3006, :protocol=>'tcp'  :group=>'monitor' },
    #   {:port=>53, :protocol=>'udp' }
    # ]
    security_group "dummy-test-security-group" do
      %w(22 80 443 8642).each {|port|  authorize :from_port => port, :to_port => port}
    end
    # load_balancer do
    #   listener :external_port => 8080, :internal_port => 8080, :protocol => 'tcp'
    # end
  end
  
end