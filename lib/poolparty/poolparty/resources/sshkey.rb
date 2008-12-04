module PoolParty    
  module Resources
    
    class Sshkey < Resource
      
      default_options({
        # :key => "ALONGSTRINGOFDIGITS",
        :target => "~/.ssh/poolparty_id_rsa"
      })
      
      def keyfile=(file)
        keyfile open(file).read
        options[:key] = keyfile
      end
      
      def key
        name
      end
      
    end
    
  end
end