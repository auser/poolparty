=begin rdoc

=end
require 'xmlsimple'

module PoolParty
  module Remote
    class Vmrun < Remote::RemoterBase
      include Dslify

      default_options(
        :image_id           => nil,
        :keypair_name       => nil
      )
      
      def initialize(opts={}, &block)
        super
      end

      def self.launch_new_instance!(o={})
        new(o).launch_new_instance!
      end
      def launch_new_instance!(o={})
        `virsh start #{image_id}`
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
        xml = `virsh dumpxml #{image_id}`
        hsh = XmlSimple.xml_in(xml, 'KeyToSymbol'=>true)
        hsh[:state] = `virsh domstate #{image_id}`
        hsh
      end
      
      def self.describe_instances(o={})
        new(o).describe_instances
      end
      def describe_instances(o={})
        `virsh list`
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
