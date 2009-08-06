module PoolParty
  module Installers
    class Ec2 < Installer
      
      def steps
        [
          :todo
        ]
      end
      
      def self.name
        "Ec2"
      end
      
      def self.description
        "Ec2 installer"
      end
      
      def todo
        colored_say <<-EOE
          <line>
          <red>Not implemented yet</red>
          <line>
        EOE
        exit 0
      end
      
    end
  end
end