module PoolParty
  module VERSION #:nodoc:
    MAJOR = 1
    MINOR = 0
    TINY  = 0

    STRING = [MAJOR, MINOR, TINY].join('.')    
  end
  class Version
    def self.to_s
      [VERSION::MAJOR, VERSION::MINOR, VERSION::TINY].join('.')
    end
  end
  
end
