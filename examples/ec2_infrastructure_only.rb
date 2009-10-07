require File.dirname(__FILE__)+'/../lib/poolparty.rb'
$PP_VERBOSE=true

pool "cloudteam" do
  execute false
  # security_group 'minerva_chacha' do
  #   authorize :port=>80, :protocol=>'tcp', :network=>'0.0.0.0/0'
  #   authorize :port=>22, :protocol=>'tcp', :network=>'0.0.0.0/0'
  #   authorize :port=>25826, :protocol=>'udp', :security_group=>'minerva-monitor'
  #   authorize :from_port=>1, :to_port=>65535 :protocol=>'all', :security_group=>'minerva_monitor',
  # end
  # security_group 'minerva_monitor', [
  #   {:port=>80, :protocol=>'tcp', :network=>'0.0.0.0/0'},
  #   {:port=>443, :protocol=>'tcp', :network=>'0.0.0.0/0'},
  #   {:port=>22, :protocol=>'tcp', :network=>'0.0.0.0/0'},
  #   {:port=>25826, :protocol=>'udp', :security_group=>'minerva_chacha'}
  #  ]
  # end
  
  cloud "cha" do
    
    #TODO: default cloud load_balancer taking an array of balancer/port/protocol hashes.
    #NOTE:loadbalancers are paid for per balancer, and are limited availabiliyt, so good to use multiple ports per LB
    # load_balancer [ {:external_port=>80, :internal_port=>8080, :protocol=>'tcp'}, 
    #                  {:external_port=>443, :internal_port=>8443, :protocol=>'tcp'} ]
    
    
    load_balancer do
      # listener :external_port => 80, :internal_port => 8000
      listener :external_port => 8080, :internal_port => 8080, :protocol => 'tcp'
    end
    
    autoscaler #"chaAS" #,{:cooldown=>60}  #TODO: take hash of options

    # autoscale 'skinyscaler', {
    #   :availability_zones => ['us-east-1a]  #NOTE  good to be able to have multiple scaling groups per cloud for fine grained scaling per AZ controll
    #   :cooldown => 30,
    #   :trigger => {
    #       :namespace              => "AWS/EC2/#{pool.name}",
    #       :measure                => "CPUUtilization",
    #       :statistic              => "Average",
    #       :dimensions             => "AutoScalingGroupName=#{self.name}",
    #       :period                 => 60,
    #       :lower_threshold        => 35,
    #       :upper_threshold        => 80,
    #       :lower_breach_increment => -1,
    #       :upper_breach_increment => 1,
    #       :breach_duration        => 600
    #     }
    #   }
    using :ec2 do
      security_group "chaca_thin_test_group" do
        revoke :from_port => "8080", :to_port => "8081"  #NOTE: why have a revoke method?  closed by default, and only what is declared is open
        authorize :from_port => "22", :to_port => "22"
      end
      minimum_instances 1
      maximum_instances 2
      user_data 'Hello user data'
    end
  end
  
end


pool.run if !pool.running?
sleep 4

puts "pool has been run\n----"
puts "describing current autscaling activities:"

@opts={:access_key_id => ENV['EC2_ACCESS_KEY'], :secret_access_key => ENV['EC2_SECRET_KEY'], :symbolize_keys=>:snake_case}
if ENV['EC2_URL']
  @opts[:server] = URI.parse(ENV['EC2_URL']).host
end
@ec2 = AWS::EC2::Base.new(@opts)
@elb = AWS::ELB::Base.new(@opts)
@as = AWS::Autoscaling::Base.new(@opts)

pool.clouds.each do |name,cld| 
  puts "\ncloud[:#{name}]\n"
  cld.autoscalers.each do |asg| 
    puts "AS #{asg.first} ===========================================\n"
    # pp cld.cloud_provider.send(:as).describe_scaling_activities(:autoscaling_group_name=>'skinnytest-cha')
    # puts `$HOME/Dropbox/cloudteam/ec2_api_tools/AutoScaling-1.0.4.4/bin/as-describe-scaling-activities #{asg.first}`
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





# puts `$HOME/Dropbox/cloudteam/ec2_api_tools/AutoScaling-1.0.4.4/bin/as-describe-scaling-activities #{'a'}`