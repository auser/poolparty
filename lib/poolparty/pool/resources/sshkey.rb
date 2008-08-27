module PoolParty    
  module Resources
        
    def sshkey(opts={}, &block)
      resource(:sshkey) << PoolParty::Resources::Sshkey.new(opts, &block)
    end
    
    class Sshkey < Resource
      
      default_options({
        :command => nil,
        :key => "ALONGSTRINGOFDIGITS",
        :target => "~/.ssh/poolparty_id_rsa",
        :name => "key"
      })
      
      def keyfile=(file)
        keyfile open(file).read
      end
      
    end
    
  end
end