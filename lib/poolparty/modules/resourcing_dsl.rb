module PoolParty
  module ResourcingDsl
    def ensures(str = :present)
      if [:absent, :present].include?(str) 
        str == :absent ? is_absent : is_present
      else
        str
      end
    end
    def present
      :install
    end
    def absent
      :remove
    end
    # Allows us to send an ensure to ensure the presence of a resource
    def is_present(*args)
      dsl_options.merge!(:ensures => present)
      present
    end
    # Ensures that what we are sending is absent
    def is_absent(*args)
      dsl_options.merge!(:ensures => absent)
      absent
    end

    #TODO: Diet
    def get_client_or_gem_template(file)
      if ::File.file?(file) && ::File.readable?(file)
        file
      elsif client_templates_directory_exists? && client_template_exists?(file)
        vputs "using custom template #{::File.join(Dir.pwd, "templates/#{file}")}"
        ::File.join(Dir.pwd, "templates/#{file}")
      else
        vputs "using standard template: #{::File.join(::File.dirname(__FILE__), "..", "templates/#{file}")}"
        ::File.join(::File.dirname(__FILE__), "..", "templates/#{file}")
      end
    end
    
    def client_templates_directory_exists?
      ::File.directory?("#{Dir.pwd}/templates")
    end
    
    def client_template_exists?(filename)
      return true if ::File.file?(filename) && ::File.readable?(filename)
      file = ::File.join("#{Dir.pwd}/templates", filename)
      ::File.file?(file) && ::File.readable?(file)
    end
  end
end
