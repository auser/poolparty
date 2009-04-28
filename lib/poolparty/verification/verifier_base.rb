module PoolParty
  module Verifiers
    
    class VerifierBase
      attr_reader :host
      
      def host=(h=nil)
        @host ||= h
      end
      
      def name
        @name ||= self.class.to_s.top_level_class
      end
    end
    
  end
end