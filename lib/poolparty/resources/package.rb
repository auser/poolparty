module PoolParty
  module Resources
=begin rdoc
  Package

  Usage:
    has_package "name_of_package"

  This will install the "name_of_package" package with the package provider (apt, yum, etc)
=end
    class Package < Resource

      def present
        :install
      end

      def absent
        :remove
      end

    end

  end
end