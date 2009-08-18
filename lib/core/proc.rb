class Proc
  
  # Get the code, based on the inspect's filename and line number start
  def code(filename=source_file)
    return @code if @code
    current_block_count = 1
    line_count = 0
    
    source(filename).each do |line|
      if line =~ /do(\W*?)$/
        current_block_count+=1
      elsif line =~ /end$/
        current_block_count-=1
        if current_block_count == 0
          return @code = source(filename)[0..(line_count-1)].join("\n")
        end        
      end
      line_count+=1
    end    
  end
  
  # Source
  def source(filename = source_file, start_line = source_line_number)
    @source ||= if File.exist?(filename)
      begin
        open(filename).read.split("\n")[start_line .. -1]
      rescue
        nil
      end
    else
      raise StandardError.new("Cannot find the source file for #{self.inspect}")
    end
  end
  
  # Grab the location of the proc by filename
  def source_file
    @source_file ||= proc_info[0]
  end
  
  # Grab the line number in the source_file
  def source_line_number
    @source_line_number ||= proc_info[1]
  end
  
  # Grab the source location from the inspect method
  def proc_info
    return @proc_info if @proc_info
    if md = /^#<Proc:0x[0-9A-Fa-f]+@(.+):(\d+)>$/.match(inspect)
      filename, line = md.captures
      @proc_info = File.expand_path(filename), line.to_i
    end
    @proc_info
  end
  
end