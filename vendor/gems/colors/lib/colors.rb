class Colors
  class << self
    
    def default_terminal_colors
      @default_terminal_colors ||= "\e[0m\e[37m\e[40m"
    end
    
    def process(data)
      begin
        _process(data)
      ensure
        STDOUT.flush
        reset!
      end      
    end
    
    def reset!
      STDOUT.write("\e[0m")
      STDOUT.flush
    end
    
    def _process(data)	
      # Backrounds
      if m = data.match(%r{<(.*?) bg=(.*?)>(.*?)<\/(.*?)>}m)
        colors = {:red => 1, :green => 2, :yellow => 3, :blue => 4, :purple => 5, :sea => 6, :white => 7}
        colors.each do |k,v|
          t = data.match(%r{<(.*?) bg=#{k}>(.*?)<\/(.*?)>}m)
          data.gsub!(%r{<(.*?) bg=#{k}>(.*?)<\/(.*?)>}m, "\e[1m\e[4#{v}m<\\1>\\2</\\1>#{default_terminal_colors}")
        end
      end

      # Colored text
    	colors = {:red => 1, :green => 2, :yellow => 3, :blue => 4, :purple => 5, :sea => 6, :white => 7}
    	colors.each do |k,v|
        data.gsub!(%r{<#{k}>(.*?)</#{k}>}m, "\e[1m\e[3#{v}m\\1#{default_terminal_colors}")
    	end

    	data.gsub!(%r{<b>(.*?)</b>}m, "\e[1m\\1#{default_terminal_colors}")
    	data.gsub!(%r{<banner>(.*?)</banner>}m, "\e[33m\e[44m\e[1m\\1#{default_terminal_colors}")
    	return data
    end
    
  end
end