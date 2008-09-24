module PoolParty    
  module Resources
    
    class Sshkey < Resource
      
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