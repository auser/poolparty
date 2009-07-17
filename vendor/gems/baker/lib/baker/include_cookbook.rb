=begin rdoc
  Include recipes
=end
module Baker
  class IncludeCookbook < Base
    
    attr_accessor :directory
    
    def initialize(opts=nil)
      @options = extract_options(opts)
      @directory = File.expand_path( options[:key] ? options[:key] : options[:directory])
      raise StandardError.new("Given recipe #{directory} does not exist") unless File.directory?(directory)
      super
    end

    def compile
      dir = "#{cookbook_directory}/cookbooks"
      ::FileUtils.mkdir_p dir unless ::File.directory?(dir)
      FileUtils.cp_r directory, dir
    end
    
    def cookbook_name
      ::File.basename(directory)
    end
    
  end
end