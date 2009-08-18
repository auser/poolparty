=begin rdoc

== Link

The link resource sets a symlink 

== Usage

  has_link(:name => 'target', :source=>'/where/to' ) do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> The location of the symlink (target)
* <tt>source</tt> The source of the symlink, the existing file or directory

== Examples

  puppet style:
    has_link(:name => "/var/www/poolpartyrb.com/public", :source => "/var/www/poolpartyrb.com/poolparty-website/site")
  chef style:
    has_link(:to => "/var/www/poolpartyrb.com/public", :target_file => "/var/www/poolpartyrb.com/poolparty-website/site")
=end
module PoolParty
  module Resources
    
    class Link < Resource
      
      default_options(
        :link_type  => :symbolic,
        :to         => nil
      )
      
      def print_to_chef
        <<-EOE
link "<%= name %>" do
  link_type <%= print_variable(link_type) %>
  action :<%= exists? ? :create : :delete %>
  to <%= print_variable(to) %>
end
        EOE
      end
      
    end
    
  end
end