module PoolParty    
  module Plugin
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
    class Host < Plugin
            
      default_options({
        :name => "localhost",
        :ip => "127.0.0.1"
      })
      
      def loaded(o={},&block)
        has_line_in_file(:file => "/etc/hosts", :line => "#{o[:ip]} #{o[:name]}")
      end
            
    end
    
  end
end