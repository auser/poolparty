=begin rdoc
  EC2 Remoter Base
  
  This serves as the basis for running PoolParty on Amazon's ec2 cloud
  cluster. 
=end
require "date"
require "#{::File.dirname(__FILE__)}/ec2/ec2_response_object"

begin
  require 'EC2'
  
  class String
    def convert_from_ec2_to_ip
      self.gsub(/.compute-1.amazonaws.com*/, '').gsub(/ec2-/, '').gsub(/-/, '.')
    end
    def parse_datetime
      DateTime.parse( self.chomp ) rescue self
    end
  end
  module PoolParty
    module Ec2
      include PoolParty::Remote::RemoterBase
      
      def launch_new_instance!(num=1)
        instance = ec2.run_instances(
          :image_id => (ami || Base.ami),
          :user_data => "",
          :minCount => 1,
          :maxCount => num,
          :key_name => (keypair || Base.keypair),
          :availability_zone => nil,
          :instance_type => "#{size || Base.size}",
          :group_id => ["#{security_group || Base.security_group}"])
        begin
          h = EC2ResponseObject.get_hash_from_response(instance)
          #h = instance.instancesSet.item.first
        rescue Exception => e
          h = instance
        end
        h
      end
      # Terminate an instance by id
      def terminate_instance!(instance_id=nil)
        ec2.terminate_instances(:instance_id => instance_id)
      end
      # Describe an instance's status
      def describe_instance(id=nil)
        describe_instances.select {|a| a[:name] == id}[0] rescue nil
      end
      def describe_instances
        id = 0
        get_instances_description.each_with_index do |h,i|
          if h[:status] == "running"
            inst_name = id == 0 ? "master" : "node#{id}"
            id += 1
          else
            inst_name = "#{h[:status]}_node#{i}"
          end
          h.merge!({
            :name => inst_name,
            :hostname => h[:ip],
            :ip => h[:ip].convert_from_ec2_to_ip,
            :index => i,
            :launching_time => (h[:launching_time])
          })
        end.sort {|a,b| a[:index] <=> b[:index] }
      end
      # Get the s3 description for the response in a hash format
      def get_instances_description
        EC2ResponseObject.get_descriptions(ec2.describe_instances)
      end

      def after_launch_master(inst=nil)
        instance = master
        vputs "Running tasks after launching the master"
        begin
          # when_no_pending_instances do
            if instance
              ec2.associate_address(:instance_id => instance.instance_id, :public_ip => set_master_ip_to) if set_master_ip_to
              ec2.attach_volume(:volume_id => ebs_volume_id, :instance_id => instance.instance_id, :device => ebs_volume_device) if ebs_volume_id && ebs_volume_mount_point
            end
          # end
        rescue Exception => e        
          vputs "Error in after_launch_master: #{e}"
        end
        reset_remoter_base!
        when_all_assigned_ips {wait "5.seconds"}
      end

      # Help create a keypair for the cloud
      # This is a helper to create the keypair and add them to the cloud for you
      def create_keypair
        return false unless keypair
        unless ::File.exists?( new_keypair_path )
          FileUtils.mkdir_p ::File.dirname( new_keypair_path )
          vputs "Creating keypair: #{keypair} in #{new_keypair_path}"
          Kernel.system "ec2-add-keypair #{keypair} > #{new_keypair_path} && chmod 600 #{new_keypair_path}"
        end
      end
      
      # wrapper for remote base to perform a snapshot backup for the ebs volume
      def create_snapshot
        return nil if ebs_volume_id.nil?
        ec2.create_snapshot(:volume_id => ebs_volume_id)
      end
      
      # EC2 connections
      def ec2
        @ec2 ||= EC2::Base.new( :access_key_id => (access_key || Base.access_key), 
                                :secret_access_key => (secret_access_key || Base.secret_access_key)
                              )
      end

      # Callback
      def custom_install_tasks_for(o)
        [
          "# ec2 installation tasks",
          "# Set hostname",
          # "if [ -z $(grep -v '#' /etc/hosts | grep '#{o.name}') ]; then echo \"$(curl http://169.254.169.254/latest/meta-data/public-ipv4) #{o.name}\" >> /etc/hosts; fi",
          "if [ -z \"$(grep -v '#' /etc/hosts | grep '#{o.name}')\" ]; then echo '127.0.0.1 #{o.name}' >> /etc/hosts; fi",
          "hostname #{o.name}",
          "echo #{o.name} > /etc/hostname",
          "cd /var/poolparty && wget http://rubyforge.org/frs/download.php/43666/amazon-ec2-0.3.1.gem -O amazon-ec2.gem 2>&1"
        ]
      end

      def custom_configure_tasks_for(o)
        [
          "# ec2 configuration",
          "/usr/bin/gem install -y --no-ri --no-rdoc amazon-ec2.gem 2>&1"
        ]
      end

      def reset_base!
        @describe_instances = @cached_descriptions = nil
      end
    end
    register_remote_base :Ec2
  end
rescue LoadError
  puts <<-EOM
Error: In order to use ec2, you need to install the amazon-ec2 gem

Ec2 is the default remoter base for PoolParty. If you intend on using
a different remoter base, specify it with:

using :remoter_name

in your config file, otherwise, to continue install amazon-ec2 with

gem install amazon-ec2
EOM
end