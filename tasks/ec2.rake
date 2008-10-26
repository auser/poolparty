namespace(:ec2) do
  desc "Prepare your cloud for poolparty"
  task :init do
    cmd =<<-EOE
ec2-authorize default -P icmp -t -1:-1 -s 0.0.0.0/0
ec2-authorize -p 7000-7050 default
ec2-authorize -p 8140 default
ec2-authorize -p 4369 default
    EOE
    
    Kernel.system cmd.split(/\n/).join(" && ")
  end
end