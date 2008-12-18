module PoolParty
  module VERSION #:nodoc:
    MAJOR = 0
    MINOR = 2
    TINY  = 90

    STRING = [MAJOR, MINOR, TINY].join('.')    
  end
  class Version
    def self.to_s
      [VERSION::MAJOR, VERSION::MINOR, VERSION::TINY].join('.')
    end
  end
  
end
