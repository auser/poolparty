module PoolParty    
  module Resources
=begin rdoc

== Host

The host parameter sets hosts on the instances. Setting this, every node will have the host.

== Usage

  has_host(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt>The name of the instance
* <tt>ip</tt> IP address of the instance for the host entry

== Examples

  has_host({:name => "other_machine", :ip => 192.168.0.101 })
=end       
    class Host < Resource
            
      default_options({
        :ip => "127.0.0.1"
      })
      
      def aka(i=nil)
        i ? options[:alias] = i : options[:alias]
      end
            
    end
    
  end
end