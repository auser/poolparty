module PoolParty
  module Verifiers
    @available_verifiers =[]
    def self.available
      @available_verifiers
    end
    
    class VerifierBase
      attr_reader :host
      
      def self.inherited(subclass)
        unless Verifiers.available.include?(subclass)
          Verifiers.available << subclass
        end
      end
      
      def host=(h=nil)
        @host ||= h
      end
      
      def name
        @name ||= self.class.to_s.top_level_class
      end
    end
    
  end
end