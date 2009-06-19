module PoolParty
  module DependencyResolvers
    
    class Base
      
      
      
    end
    
  end
end

%w().each do |lib|
  require "#{::File.dirname(__FILE__)}/#{lib}"
end