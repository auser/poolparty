module PoolParty    
  module Resources
    
    def file(&block)
      @file ||= File.new(&block)
    end
    
    class File < Resource
      
      def options(h={})
        @options ||= {
          :ensure => "present",
          :mode => 0644,
          :owner => "poolparty"
        }.merge(h)
      end      
    end
    
  end
end