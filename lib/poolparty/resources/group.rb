=begin rdoc
== Group

The group resource ensures there is a group created on the nodes

== Usage

  has_group(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> Name of the service to be running
* <tt>gid</tt> gid of the user
* <tt>members</tt> Members that belong in the group

== Examples

  has_group "admins"
=end
module PoolParty
  module Resources
    
    class Group < Resource
      
      default_options(
        :action   => :create,
        :gid      => nil,
        :members  => nil,
        :append   => false
      )
      
      def print_to_chef
        str = "group \"<%= name %>\" do\n   action :<%= (action ? action : (exists ? :create : :remove)) %>"
        str << "  gid <%= print_variable(gid) %>\n" if gid
        str << "  members <%= print_variable(members) %>\n" if members
        str << "  append <%= print_variable(append) %>\n" if append
        str << "\nend"
      end
      
    end
    
  end
end