module PoolParty
  module FileWriter
    def copy_file_to_storage_directory(file)
      FileUtils.cp file, Base.storage_directory
    end
    def write_to_file_in_storage_directory(file, str, &block)
      path = ::File.join(Base.storage_directory, file)
      write_to_file(path, str, &block)
    end
    def write_to_file(path, str, &block)
      FileUtils.mkdir_p ::File.dirname(path) unless ::File.directory?(::File.dirname(path))
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
    def make_base_directory
      FileUtils.mkdir_p Base.storage_directory unless ::File.directory?(Base.storage_directory)
    end
    def clear_base_directory
      FileUtils.rm_r Base.storage_directory if ::File.directory?(Base.storage_directory)
    end
  end
end