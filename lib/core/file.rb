class File
  class << self
    alias_method :old_symlink, :symlink
    def symlink(old_name, new_name)
      begin
        old_symlink(old_name, new_name)
      rescue Errno::EEXIST
        $stderr.puts "warning: symlinking #{old_name} -> #{new_name}. Already exists"
      end
    end
  end
end
