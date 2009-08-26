=begin rdoc

== Mount

The mount specifies a mount that is to be mounted on the instances

== Usage

  has_mount(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> The location of the mount (default: /data)
* <tt>device</tt> The device location for the mount. This mounts at the directory set by the name
* <tt>options</tt> The options to be set in the mount file fstab (default: rw,nosuid,noquota)
* <tt>fstype</tt> The Type of mount (default: xfs)

== Examples

  has_mount(:name => "/data", :device => "/dev/sda100")
=end
module PoolParty
  module Resources
    
    class Mount < Resource
      
      default_options(
        :action       => :mount,
        :device       => nil,
        :device_type  => :device,
        :fstype       => nil,
        :options      => "rw",
        :dump         => 0,
        :pass         => 2
      )
      
      def print_to_chef
        str = <<-EOE
mount "<%= name %>" do
  action <%= print_variable(action ? action : (exists ? :mount : :unmount)) %>
  device_type <%= print_variable(device_type) %>
EOE
        str << "  device <%= print_variable(device) %>\n" if device
        str << "  fstype <%= print_variable(fstype) %>\n" if fstype
        str << "  options <%= print_variable(options) %>\n" if options
        str << "  dump <%= print_variable(dump) %>\n" if dump
        str << "  pass <%= print_variable(pass) %>\n" if pass
        str << "end"
      end
      
    end
    
  end
end
