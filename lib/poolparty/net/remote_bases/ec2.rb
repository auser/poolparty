require 'rubygems'
require 'EC2'

class String
  def convert_from_ec2_to_ip
    self.gsub(/.compute-1.amazonaws.com*/, '').gsub(/ec2-/, '').gsub(/-/, '.')
  end
end
module PoolParty
  module Ec2
    def launch_new_instance!
      instance = ec2.run_instances(
        :image_id => self.respond_to?(:ami) ? ami : Base.ami,
        :user_data => "",
        :minCount => 1,
        :maxCount => 1,
        :key_name => "#{self.respond_to?(:keypair) ? keypair : Base.keypair}",
        :availability_zone => nil,
        :size => "#{self.respond_to?(:size) ? size : Base.size}")
      begin
        item = instance#.instancesSet.item
        EC2ResponseObject.get_hash_from_response(item)
      rescue Exception => e          
      end
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
      unless @describe_instances && !@describe_instances.empty?
        @id = 0
        @describe_instances = get_instances_description.each_with_index do |h,i|
          if h[:status] == "running"
            @name = "node#{@id}"
            @id += 1
          else
            @name = "#{h[:status]}_node#{i}"
          end
          h.merge!({
            :name => @name,
            :hostname => h[:ip],
            :ip => h[:ip].convert_from_ec2_to_ip
          })
        end
        @describe_instances.first[:name] = "master" unless @describe_instances.empty?
      end
      @describe_instances
    end
    # Get the s3 description for the response in a hash format
    def get_instances_description
      @cached_descriptions ||= EC2ResponseObject.get_descriptions(ec2.describe_instances).sort_by {|a| a[:launching_time]}
    end
    
    # Help create a keypair for the cloud
    # This is a helper to create the keypair and add them to the cloud for you
    def create_keypair
      return false unless keypair
      unless ::File.exists?( new_keypair_path )
        FileUtils.mkdir_p ::File.dirname( new_keypair_path )
        Kernel.system "ec2-add-keypair #{keypair} > #{new_keypair_path} && chmod 600 #{new_keypair_path}"
      end
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
        "echo #{o.name} > /etc/hostname"
      ]
    end
    
    def custom_configure_tasks_for(o)
      [
        "# ec2 configuration"
      ]
    end
    
    def reset!
      @describe_instances = @cached_descriptions = nil
    end
  end
  register_remote_base :Ec2
end

# Provides a simple class to wrap around the amazon responses
class EC2ResponseObject
  def self.get_descriptions(resp)      
    rs = get_response_from(resp)
    
    # puts rs.methods.sort - rs.ancestors.methods
    out = begin
      if rs.respond_to?(:instancesSet)
        [EC2ResponseObject.get_hash_from_response(rs.instancesSet.item)]
      else
        rs.collect {|r| 
          if r.instancesSet.item.class == Array
            r.instancesSet.item.map {|t| EC2ResponseObject.get_hash_from_response(t)}
          else
            [EC2ResponseObject.get_hash_from_response(r.instancesSet.item)]
          end            
        }.flatten.reject {|a| a.nil? }
      end
    rescue Exception => e
      # Really weird bug with amazon's ec2 gem
      rs.collect {|r| EC2ResponseObject.get_hash_from_response(r)}.reject {|a| a.nil? } rescue []
    end
    
    out
  end
  def self.get_response_from(resp)
    begin
      rs = resp.reservationSet.item unless resp.reservationSet.nil?
      rs ||= resp.DescribeInstancesResponse.reservationSet.item
      rs ||= rs.respond_to?(:instancesSet) ? rs.instancesSet : rs
      rs.reject! {|a| a.nil? || a.empty? }
    rescue Exception => e
    end
    rs
  end
  def self.get_hash_from_response(resp)
    begin
      {
        :instance_id => resp.instanceId,
        :name => resp.instanceId, 
        :ip => resp.dnsName || "not-assigned",
        :status => resp.instanceState.name,
        :launching_time => resp.launchTime,
        :keypair => resp.keyName
      }        
    rescue Exception => e
      nil
    end      
  end
end