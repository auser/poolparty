namespace(:ec2) do
  task :init do
    %x[ec2-authorize default -P icmp -t -1:-1 -s 0.0.0.0/0]
  end
end