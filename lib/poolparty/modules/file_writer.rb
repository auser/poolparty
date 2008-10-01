module PoolParty
  module FileWriter
    def copy_file_to_storage_directory(file, dir=".")
      path = ::File.join(Base.storage_directory, dir)
      make_base_path(path)
      FileUtils.cp file, path
    end
    def write_to_file_in_storage_directory(file, str, &block)
      path = ::File.join(Base.storage_directory, file)
      write_to_file(path, str, &block)
    end
    def write_to_file(path, str, &block)
      make_base_path(::File.dirname(path))
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
      ::FileUtils.rm_rf path unless ::File.directory?(path)
      unless ::File.directory?(path)
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
    def clear_base_directory
      FileUtils.rm_r Base.storage_directory if ::File.directory?(Base.storage_directory)
    end
  end
end