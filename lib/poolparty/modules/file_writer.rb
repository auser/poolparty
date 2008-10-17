module PoolParty
  module FileWriter
    def copy_file_to_storage_directory(file, preceded="")
      make_base_directory
      path = ::File.join( Base.storage_directory, preceded, ::File.basename(file) )      
      FileUtils.cp file, path unless file == path || ::File.exists?(path)
    end
    def copy_template_to_storage_directory(file)
      make_template_directory
      path = ::File.join( Base.template_directory, ::File.basename(file) )
      FileUtils.cp file, path unless file == path || ::File.exists?(path)
    end
    def write_to_file_in_storage_directory(file, str, preceded="", &block)
      path = ::File.join( Base.storage_directory, preceded, ::File.basename(file) )
      write_to_file(path, str, &block)
    end
    def write_to_file(file, str, preceded="", &block)
      path = ::File.join( Base.storage_directory, preceded, ::File.basename(file) )
      make_base_path( Base.storage_directory )
      ::File.open(path, "w+") do |f|
        f.print str
        f.flush
        f.print block.call(f) if block
      end
    end
    # Write a temp file with the content str and return the Tempfile
    # It creates a random file name
    def write_to_temp_file(str="", &block)
      returning Tempfile.new("#{Base.storage_directory}/PoolParty-#{str[0..10].chomp}-#{rand(1000)}") do |fp|
        fp.print str
        fp.flush
        block.call(fp)
      end
    end
    def make_base_path(path)
      unless FileTest.directory?(path)
        begin          
          ::FileUtils.mkdir_p path
        rescue Errno::ENOTDIR
        rescue Errno::EEXIST
          puts "There was an error"
        end
      end
    end
    def make_base_directory
      FileUtils.mkdir_p Base.storage_directory unless ::File.directory?(Base.storage_directory)
    end
    def make_template_directory
      FileUtils.mkdir_p Base.template_directory unless ::File.directory?(Base.template_directory)      
    end
    def clear_base_directory
      FileUtils::rm_rf "#{Base.storage_directory}"
    end
  end
end