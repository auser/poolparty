module PoolParty    
  module Resources
    
    def file(&block)
      File.new(&block)
    end
    
    class File < Resource
      
      def options(h={})
        @options ||= {
          :ensure => "present",
          :mode => 644,
          :owner => "poolparty",
          :name => nil
        }.merge(h)
      end      
    end
    
  end
end