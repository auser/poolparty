require File.dirname(__FILE__)+'/../lib/poolparty.rb'
$PP_VERBOSE=true

pool "CloudteamExample" do
  do_not_execute = true

  #TODO: declarative, serializable syntax, and allow definition outside a cloud
  # security_group 'minerva_monitor', [
  #   {:port=>80, :protocol=>'tcp', :network=>'0.0.0.0/0'},
  #   {:port=>443, :protocol=>'tcp', :network=>'0.0.0.0/0'},
  #   {:port=>22, :protocol=>'tcp', :network=>'0.0.0.0/0'},
  #   {:port=>25826, :protocol=>'udp', :security_group=>'minerva_chacha'}
  #  ]
  # end

  #TODO: allow definition of a load_balancer in the pool that balances between clouds, default name of pool.name
  #TODO: default cloud load_balancer taking an array of balancer/port/protocol hashes.
  #NOTE: loadbalancers are paid for per balancer, and are limited availabiliyt, so good to use multiple ports per LB
  # load_balancer [ {:external_port=>80, :internal_port=>8080, :protocol=>'tcp'},
  #                  {:external_port=>443, :internal_port=>8443, :protocol=>'tcp'} ]

  cloud "basic" do
    minimum_instances 1
    maximum_instances 2
    #keypair 'keyname' #keypair will be generated if it does not exist
    using :ec2
    security_group "chaca_thin_test_group" do
      revoke :from_port => "8080", :to_port => "8081"  #NOTE: why have a revoke method?  closed by default, and only what is declared is open
      authorize :from_port => "22", :to_port => "22"
    end
    availability_zones ['us-east-1b', 'us-east-1c']
    load_balancer do
      listener :external_port => 8080, :internal_port => 8080, :protocol => 'tcp'
    end
    autoscaler
  end

end







#####################################################################################
# Testing Stuff                                                                     #
#####################################################################################

# pool.run  #uncomment to execute the cloud
sleep 4
puts "pool has been run\n----"
puts "describing current autscaling activities:"

 # Set things up for amazon-ec2
@opts={:access_key_id => ENV['EC2_ACCESS_KEY'], :secret_access_key => ENV['EC2_SECRET_KEY'], :symbolize_keys=>:snake_case}
if ENV['EC2_URL']
  @opts[:server] = URI.parse(ENV['EC2_URL']).host
end
@ec2 = AWS::EC2::Base.new(@opts)
@elb = AWS::ELB::Base.new(@opts)
@as = AWS::Autoscaling::Base.new(@opts)

# Iterate over the clods and ensure the elb and autoscaling groups exist
pool.clouds.each do |name,cld|
  puts "\ncloud[:#{name}]\n"
  cld.autoscalers.each do |asg|
    puts "AS #{asg.first} ===========================================\n"
     grp = @as.describe_autoscaling_groups['DescribeAutoScalingGroupsResult']['AutoScalingGroups']['member'].select_with_hash('AutoScalingGroupName'=>asg.first)
     (!grp || grp.empty?) ? warn("ASGroup #{asg.first} was not created") : pp(grp)
     activities = @as.describe_scaling_activities(:autoscaling_group_name=>'cloudteam-cha')["DescribeScalingActivitiesResult"]["Activities"]["member"]
     activities.each{|act| puts act.Cause}
  end
  cld.load_balancers.each do |name, lb|
    puts "ELB #{name} ===========================================\n"
    elbt = @elb.describe_load_balancers.DescribeLoadBalancersResult.LoadBalancerDescriptions.member.select_with_hash(:load_balancer_name=>name)
    (!elbt || elbt.empty?) ? warn("LB #{name} was not created: #{elbt}") : pp(elbt)
  end
end
