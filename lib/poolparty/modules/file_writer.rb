module PoolParty
  module FileWriter
    def write_to_file_for(f="haproxy", node=nil, str="", &block)      
      make_base_directory
      File.open("#{base_tmp_dir}/#{node ? "#{node.name}-" : ""}#{f}", "w+") do |file|
        file << str
        file << block.call if block_given?
      end
    end
    # Write a temp file with the content str
    def write_to_temp_file(str="")        
      tempfile = Tempfile.new("#{base_tmp_dir}/PoolParty-#{rand(1000)}-#{rand(1000)}")
      tempfile.print(str)
      tempfile.flush
      tempfile
    end
    def with_temp_file(str="", &block)
      Tempfile.open "#{base_tmp_dir}/PoolParty-#{rand(10000)}-#{rand(10000)}" do |fp|
        fp.puts str
        fp.flush
        block.call(fp)
      end
    end    
    def make_base_directory
      `mkdir -p #{base_tmp_dir}` unless File.directory?(base_tmp_dir)
    end
    def clear_base_directory
      `rm -rf #{base_tmp_dir}/*` if File.directory?(base_tmp_dir)
    end
    # Copy all the files in the directory to the dest
    def copy_files_in_directory_to_tmp_dir(dir)
      require "ftools"
      dest_dir = "#{base_tmp_dir}/#{File.basename(dir)}"
      FileUtils.mkdir_p dest_dir
            
      if File.directory?("#{user_dir}/#{dir}")
        Dir["#{user_dir}/#{dir}**/**"].each do |file|
          File.copy(file, dest_dir)
        end
      else
        Dir["#{root_dir}/#{dir}**/**"].each do |file|
          File.copy(file, dest_dir)
        end
      end      
    end
  end
end