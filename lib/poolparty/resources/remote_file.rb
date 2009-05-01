module PoolParty    
  module Resources
        
=begin rdoc

== Remote File

The remotefile resource is used to describe a file that is hosted on the master instance and propagated to the other instances

== Usage

  has_remotefile(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> Describe the location of the file with the name
* <tt>mode</tt> Describe the mode of the file (default: 644)
* <tt>owner</tt> The owner of the file (default: poolparty user)
* <tt>content</tt> The contents of the file
* <tt>source</tt> Used to describe a file that is hosted on the master instance.
* <tt>template</tt> The file contents are described with the template. The location given must be readable
  
    To write a file to the template directory, use:
    copy_template_to_storage_directory(filepath)

== Example
  has_remotefile(:name => "/etc/haproxy.cfg") do
    mode 644
    template File.join(File.dirname(__FILE__), "..", "templates/haproxy.conf")
  end
=end
    class Remotefile < Resource
      
      default_options({
        :ensures => "present",
        :mode => 644,
        # :owner => "#{Default.user}",
        :source => nil
      })
      
      def source(arg=nil)
        arg ? options[:source] = arg : "#{Default.fileserver_base}/#{::File.basename(name)}"
      end
      
    end
    
  end
end