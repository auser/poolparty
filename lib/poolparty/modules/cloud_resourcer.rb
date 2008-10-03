require "ftools"

module PoolParty
  module CloudResourcer
    
    # Set instances with a range
    def instances(arg)
      if arg.is_a?(Range)
        minimum_instances arg.first
        maximum_instances arg.last
      end
    end
    
    def keypair_path
      ::File.join(get_keypair_path, keypair)
    end
    
    def get_keypair_path
      keypair_paths.reject {|f| f unless ::File.exists?(::File.join(f, keypair)) }.first || ""
    end
    
    def keypair_paths
      [
        Base.base_keypair_path,
        Base.base_config_directory
      ]
    end
    
    # Set the parent on the resource
    def set_parent(pare)
      @parent = pare
      # Add self as a service on the parent
      pare.add_service(self) if pare.respond_to?(:add_service)
      # Take the options of the parents
      configure(pare.options) if pare.respond_to?(:options)
    end
    
    def number_of_resources
      arr = resources.map do |n, r|
        r.size
      end
      resources.map {|n,r| r.size}.inject(0){|sum,i| sum+=i}
    end
    
    def parent
      @parent ||= nil
    end
    
  end
end