module PoolParty
  module DefinableResource
    # Define a new resource that can be called like any other resource
    # Similar to any other resource (like file or exec)
    # but you define it how you want it to work
    # Example:
    # 
    # define_resource(:line_in_file) do
    # end
    # 
    # Within the block, you can define any methods you want to run as
    # part of the resource
    # 
    # One thing to note is that this is NOT like other resources in the sense
    # that it does not give you any extra methods like the resources do
    # 
    # For example usage, see lib/poolparty/plugins/line.rb
    def define_resource(name, &block)
      name.to_s.new_resource_class &block
    end
    
    # DEPRICATED
    # use plugin instead
    #
    # Allow us to create virtual resources
    # Generally, in plugins
    # This sets a virtual resource against the Resource class
    # Example:
    # 
    # virtual_resource(:virtualhost) do    
    # end
    # 
    # This defines a virtualhost as a virtual resource
    #  and consequently gives the methods has_virtualhost and does_not_have_virtualhost
    # 
    # Note that you can define any resources within the virtual resource
    # within the definition or the call.
    # Call example:
    # has_virtualhost do
    #  name "xnot.org"
    # end
    # 
    # Which sets the virtual host's name as xnot.org
    # 
    # An example is included in the poolparty-apache-plugin
    def virtual_resource(name=:virtual_resource, opts={}, &block)
      $stderr.puts "virtual_resource (#{name}) is depricated"
      plugin(name, &block)
    end
    
  end
end