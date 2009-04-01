module PoolParty    
  module Resources
        
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