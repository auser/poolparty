$:.unshift File.dirname(__FILE__)+'/../lib/'
require "poolparty"

pool :eucalyptus do
    
  cloud :sample do
    instances 2
    keypair "eucalyptus_sample"
    using :ec2 do
      image_id 'emi-39CA160F'
    end
    
    has_file "/etc/motd", :content => "Welcome to your eucalyptus cloud!!!!!!!"
    has_directory '/mnt/ebs', :owner => 'poolparty'
    has_exec 'touch /tmp/touched'
    has_group 'partiers'
    has_user 'fred'
    
    has_cron :minute=>'5', :command => 'touch /tmp/touched'
    has_line_in_file '/tmp/touched', :line => 'lined up HERE!'
    
    has_package 'vim'
    has_variable "hookie", "pookie"
    
    has_directory "/etc/poolparty/nodes"

    clouds.each do |oname, cld|
      has_directory "/etc/poolparty/nodes/#{oname}"
      cld.nodes.each do |n|
        p "Adding #{n.instance_id} => #{n.public_ip}"
        has_file "/etc/poolparty/nodes/#{oname}/#{n.instance_id}", :content => "#{n.public_ip}"
      end
    end
    
  end
  
  cloud :bab do
    keypair "ari"
    using :ec2 do
      image_id 'emi-39CA160F'
    end
    
    has_directory "/etc/poolparty/nodes"
    
    clouds.each do |oname, cld|
      has_directory "/etc/poolparty/nodes/#{oname}"
      cld.nodes.each do |n|
        has_file "/etc/poolparty/nodes/#{oname}/#{n.instance_id}", :content => "#{n.public_ip}"
      end
    end
    
  end
  
end