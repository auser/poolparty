module Baker
  class Config < Base
    
    attr_accessor :file, :string
    
    def initialize(opts=nil)
      @options = extract_options(opts) || {}
      f = (options.has_key?(:file) ? options[:file] : options[:key])
      
      if f && File.file?(f)
        @file = File.expand_path( f )
      else
        @string = options[:key] || default_config
      end
      super
    end
    
    def content
      @content ||= if file && File.file?(file)
        open(file).read
      else
        string
      end
    end
    
    def compile(config_name="solo.rb")
      dir = "#{cookbook_directory}/config"
      ::FileUtils.mkdir_p dir unless ::File.directory?(dir)
      File.open("#{dir}/#{config_name}", "w") {|f| f << content}
    end
        
    private
    
    def default_config
          <<-EOE
cookbook_path     "/etc/chef/cookbooks"
node_path         "/etc/chef/nodes"
log_level         :info
file_store_path  "/etc/chef"
file_cache_path  "/etc/chef"
          EOE
    end
    
  end
end