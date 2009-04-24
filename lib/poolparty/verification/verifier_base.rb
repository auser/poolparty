module PoolParty
  module Verifiers
    
    class VerifierBase
      attr_reader :host
      
      def host=(h=nil)
        @host ||= h
      end
    end
    
  end
end