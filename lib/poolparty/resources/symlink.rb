module PoolParty    
  module Resources
        
=begin rdoc

== Symlink

The symlink resource sets a symlink 

== Usage

  has_symlink(:key => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> The location of the symlink
* <tt>target</tt> The source of the symlink, the existing file or directory

== Examples

  has_symlink(:name => "/var/www/poolpartyrb.com/public", :source => "/var/www/poolpartyrb.com/poolparty-website/site")
=end
    class Symlink < Resource
      
      def initialize *args, &block
        super
      end
      
    end
    
  end
end