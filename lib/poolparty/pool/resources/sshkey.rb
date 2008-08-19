module PoolParty    
  module Resources
        
    class SshKey < Resource
      
      default_options({
        :command => nil,
        :key => "ALONGSTRINGOFDIGITS",
        :target => "~/.ssh/poolparty_id_rsa"
      })
      
      def keyfile=(file)
        keyfile open(file).read
      end
      
    end
    
  end
end