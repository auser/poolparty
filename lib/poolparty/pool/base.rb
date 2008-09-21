=begin rdoc
  Base
  This handles user interaction, loading the parameters, etc.
=end
require "open-uri"
module PoolParty
  class Base
    include Configurable
    extend MethodMissingSugar
    
    default_options({
      :environment => "production",
      :user => "poolparty",
      :base_keypair_path => "~/.ec2"
    })
    
    # Class methods
    class << self
      def options(h={})
        @options ||= default_options.merge(h)
      end
    end
  end    
end