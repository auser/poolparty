module PoolParty
  class Resource < Base
    
    default_options(
      :name => "resource"
    )
    
  end
end

Dir["#{File.dirname(__FILE__)}/../resources/*.rb"].each {|lib| require lib }