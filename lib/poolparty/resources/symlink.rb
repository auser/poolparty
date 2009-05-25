module PoolParty    
  module Resources
        
=begin rdoc

== Symlink

The symlink resource sets a symlink 

== Usage

  has_symlink(:name => 'target', :source=>'/where/to' ) do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> The location of the symlink (target)
* <tt>source</tt> The source of the symlink, the existing file or directory

== Examples

puppet style:
  has_symlink(:name => "/var/www/poolpartyrb.com/public", :source => "/var/www/poolpartyrb.com/poolparty-website/site")
chef style:
  has_symlink(:to => "/var/www/poolpartyrb.com/public", :target_file => "/var/www/poolpartyrb.com/poolparty-website/site")
=end
    class Symlink < Resource
      
      dsl_methods :source,
                  :to,
                  :target_file
                  
      default_options :link_type => :symbolic
      
      def present
        :create
      end
      
      def absent
        :delete!
      end
      
    end
    
  end
end