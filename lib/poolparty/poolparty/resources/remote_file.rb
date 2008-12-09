module PoolParty    
  module Resources
        
    class Remotefile < Resource
      # Not really my favorite of lines
      include PoolParty::Configurable
      
      default_options({
        :ensure => "present",
        :mode => 644,
        # :owner => "#{Base.user}",
        :source => nil
      })
      
      def class_type_name
        "file"
      end
      
      def source(arg=nil)
        arg ? options[:source] = arg : "#{Base.fileserver_base}/#{::File.basename(name)}"
      end
      
    end
    
  end
end