module PoolParty
  module Resources
=begin rdoc

== Package

The package resources defines a package that must be present on all of the
instances This will install the "name_of_package" package with the package
provider (apt, yum, etc)

== Usage

  has_package "name_of_package"
  has_package(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> The package name. The default provider for your OS will be picked by the <acronym title='The dependency resolver is the describer for the cloud. The default one for PoolParty is puppet'>dependency resolver</acronym>

== Examples
  has_package(:name => 'apache2')
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