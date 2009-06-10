=begin rdoc

=end
require 'xmlsimple'

module PoolParty
  module Remote
    class Libvirt < Remote::RemoterBase
      include Dslify
      
      default_options :image_id => nil 
      
      def initialize(opts={}, &block)
        set_vars_from_options opts
        super
      end
      
      def self.launch_new_instance!(o={})
        new(o).launch_new_instance!
      end
      def launch_new_instance!(o={})
        `virsh start #{image_id}`
        describe_instance :image_id=>image_id
      end
      # Terminate an instance by id
      def self.terminate_instance!(o={})
        new(o).terminate_instance!
      end
      def terminate_instance!(o={})
        `virsh shutdown #{image_id}`
      end
      
      def self.describe_instance(o={})
        new(o).describe_instance
      end
      def describe_instance(o={})
        LibvirtInstance.new(o).description
      end
      
      # Returns an array of instance name => state
      # For example:
      #   [{"i-3687065A" =>"shut off"},
      #     {"jaunty19"  =>"shut off"},
      #     {"jauntykvm" =>"running"}]
      def self.describe_instances(o={})
        new(o).describe_instances
      end
      def describe_instances(o={})
        output = `virsh list`.split("\n")
        return [] if output.empty? || output.size < 3
        output[2..-1].collect do |i|
          d=i.split(' ')
          LibvirtInstance.new(:name => d[1], :status  => d[2..-1].join(' '))
        end
      end
      
      # After launch callback
      # This is called after a new instance is launched
      def after_launched(force=false)
        puts "new instance was launched"
      end
      
      # Before shutdown callback
      # This is called before the cloud is contracted
      def before_shutdown
      end
      def self.virsh
        new({}).path_to_binary
      end
      
    end
  end
end
