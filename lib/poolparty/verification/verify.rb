=begin rdoc
  Verification
  
  Add a verify block to a clouds.rb
=end
module PoolParty
  module Verification

    module ClassMethods
      attr_reader :verifiers
    end
    
    module InstanceMethods
      def verifiers
        @verifiers ||= []
      end
      def verify(&block)
        v = Verify.new(&block)
        v.verifiers.each {|v| verifiers << v}
      end
      def passing?
        reset!
        verifiers.each do |v|          
          ip = nodes(:status => "running").first.ip rescue "127.0.0.1"
          v.host = ip
          raise "Verification failed at #{v.class}" unless v.passing?
        end
        return true
      end
    end
    
    def self.included(receiver)
      receiver.extend         ClassMethods
      receiver.send :include, InstanceMethods
    end
  end
  
  require "#{::File.dirname(__FILE__)}/verifier_base.rb"
  Dir[::File.dirname(__FILE__)+"/verifiers/*"].each {|m| require m }  
  
  class Verify
    def initialize(&block)
      ::PoolParty.require_user_directory "verifiers"
      
      instance_eval &block if block
    end
    
    def verifiers;@verifiers ||= [];end
    
    # Check in with the verifiers and make sure they are all passing?
    def passing?
      verifiers.each {|v| raise "Not working" unless v.passing? }
    end
    
    # Take a string and return a ruby object if a match is found in the base_objects namespace.
    def constantize(name, base_object=Monitors)
      begin
        const = base_object.constants.find{|cnst| cnst == name.camelcase }
        base_object.module_eval const
      rescue Exception => e
        puts "#{name.camelcase} is not defined. #{e}"
        nil
      end
    end
    def method_missing m,*a,&block
      begin
        verifiers << constantize(m.to_s.capitalize, PoolParty::Verifiers).new(*a, &block)
      rescue 
        super
      end
    end
    
  end
end