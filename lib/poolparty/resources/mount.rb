module PoolParty
  module Resources
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
    
    class Mount < Resource
      
      default_options({
        :mountpoint => "/data",
        :remounts => "true",
        :mount_options => "rw,nosuid,noquota",
        :fstype => "xfs",
        :atboot => "yes"
      })
      
    end
    
  end
end