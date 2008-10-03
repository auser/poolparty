require "ec2"
module PoolParty
  module Ec2
    def launch_new_instance!
      instance = ec2.run_instances(
        :image_id => ami || Base.ami,
        :user_data => "",
        :minCount => 1,
        :maxCount => 1,
        :key_name => "#{keypair || Base.keypair}",
        :availability_zone => nil,
        :size => "#{ size || Base.size}")
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
      get_instances_description.select {|a| a.instance_id == id}[0] rescue nil
    end
    def describe_instances
      get_instances_description.each {|h| h.merge!(:name => h[:instance_id]) }
    end
    # Override the master method
    def master
      PoolParty::Remote::RemoteInstance.new(instances_list[0])
    end
    # Get the s3 description for the response in a hash format
    def get_instances_description
      @cached_descriptions ||= EC2ResponseObject.get_descriptions(ec2.describe_instances)
    end
    # EC2 connections
    def ec2
      @ec2 ||= EC2::Base.new( :access_key_id => (access_key || Base.access_key), 
                              :secret_access_key => (secret_access_key || Base.secret_access_key)
                            )
    end
    def reset!
      @cached_descriptions = nil
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
        :ip => resp.dnsName, 
        :status => resp.instanceState.name,
        :launching_time => resp.launchTime,
        :keypair => resp.keyName
      }        
    rescue Exception => e
      nil
    end      
  end
end