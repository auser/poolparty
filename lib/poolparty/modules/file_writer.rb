module PoolParty
  module FileWriter
    def copy_file_to_storage_directory(file, preceded="")
      make_base_directory
      path = ::File.join( Base.storage_directory, preceded, ::File.basename(file) )
      ::FileUtils.cp file, path unless file == path || ::File.file?(path)
    end
    def cleanup_storage_directory
      Dir["#{Base.storage_directory}/**/*"].each do |f|
        ::FileUtils.rm f if ::File.file?(f)
      end
    end
    def copy_template_to_storage_directory(file)
      make_template_directory
      path = ::File.join( Base.tmp_path, Base.template_directory, ::File.basename(file) )
      FileUtils.cp file, path unless file == path || ::File.exists?(path)
    end
    def copy_directory_into_template_storage_directory(dir)
      path = make_template_directory(dir)
      Dir["#{dir}/*"].each do |file|
        FileUtils.cp file, path unless ::File.exists?(::File.join(path, ::File.basename(file)))
      end
      ::File.basename(path)
    end
    def copy_directory_into_storage_directory(from, pat)
      to = ::File.join(Base.storage_directory, pat)
      
      make_directory_in_storage_directory(to) unless ::File.directory?(to)
      puts "from: #{to}"
      FileUtils.cp_r(from, to)
    end
    def make_directory_in_storage_directory(dirname="newdir")
      path = ::File.join( Base.storage_directory, dirname )
      make_base_path path
    end
    def write_to_file_in_storage_directory(file, str="", preceded="", &block)
      path = ::File.join( Base.storage_directory, preceded, ::File.basename(file) )
      write_to_file(path, str, &block)
    end
    def write_to_file(file, str="", preceded="", &block)
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
          ::FileUtils.rm path if ::File.file?(path)
          ::FileUtils.mkdir_p path
        rescue Errno::ENOTDIR
        rescue Errno::EEXIST
          puts "There was an error"
        end
      end
    end
    def make_base_directory
      begin
        FileUtils.mkdir_p Base.storage_directory unless ::File.directory?(Base.storage_directory)
      rescue Errno::EEXIST
        FileUtils.rm Base.storage_directory
        make_base_directory
      end            
    end
    def make_template_directory(dir=nil)
      path = dir ? ::File.join(Base.tmp_path, Base.template_directory, ::File.basename(dir)) : ::File.join(Base.tmp_path, Base.template_directory)
      begin
        make_base_directory
        FileUtils.mkdir path unless ::File.directory?(path)
      rescue Errno::EEXIST
        FileUtils.rm path if ::File.exists?(path)
        make_template_directory(dir)
      end      
      path
    end
    def clear_base_directory
      Dir["#{Base.storage_directory}/**/*"].each do |f|
        ::FileUtils.rm f if ::File.file?(f)
      end
    end
  end
end