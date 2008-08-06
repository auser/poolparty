module PoolParty
  extend self
  
  module Ec2Wrapper
    
    module ClassMethods      
    end
    
    module InstanceMethods
      # Run a new instance, with the user_data and the ami described in the config
      def launch_new_instance!
        instance = ec2.run_instances(
          :image_id => Application.ami, 
          :user_data => "#{Application.launching_user_data}",
          :minCount => 1,
          :maxCount => 1,
          :key_name => "#{Application.keypair}",
          :availability_zone => nil,
          :size => "#{Application.size}")
        begin
          item = instance#.instancesSet.item
          EC2ResponseObject.get_hash_from_response(item)
        rescue Exception => e          
        end
      end
      # Shutdown the instance by instance_id
      def terminate_instance!(instance_id)
        ec2.terminate_instances(:instance_id => instance_id)
      end
      def associate_address_with(ip, instance_id)
        ec2.associate_address(:instance_id => instance_id, :public_ip => ip)
      end
      # Instance description
      def describe_instance(id)
        EC2ResponseObject.get_hash_from_response(ec2.describe_instances(:instance_id => id))
      end
      # Get instance by id
      def get_instance_by_id(id)
        get_instances_description.select {|a| a.instance_id == id}[0] rescue nil
      end
      # Get the s3 description for the response in a hash format
      def get_instances_description
        @cached_descriptions ||= EC2ResponseObject.get_descriptions(ec2.describe_instances)
      end
      # EC2 connections
      def ec2
        @ec2 ||= EC2::Base.new(:access_key_id => Application.access_key, :secret_access_key => Application.secret_access_key)
      end
      def reset!
        @cached_descriptions = nil
      end      
    end
    
    def self.included(receiver)
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods
    end
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
end