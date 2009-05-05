Dir["#{File.dirname(__FILE__)}/poolparty/helpers/**.rb"].each do |lib|
  require lib
end

def help_array
  ["-h", "--help", "-V", "--version", "--debug", "-d"]
end

def ask_with_help(opts={}, &block)
  help_str = opts[:help]
  message = opts[:message]
  
  o = ask("#{message} (h for help)") do |q|    
    q.validate = opts[:validate] if opts.has_key?(:validate)
  end
  
  if %w(h H).include?(o)
    colored_say help_str, :help
    ask_with_help(opts, &block)
  else
    block.call(o)
  end
  o
end

def rescued_ask(m, r)
  begin
    t = colored_ask m
  rescue Exception => e
    say r
  end
end

def colored_ask(str, color = :notice)
  setup_colors
  ask("<%= color(\"#{str}\", :#{color}) %>")
end

def colored_say(str, color = :headline)
  setup_colors
  say("<%= color(\"#{str}\", :#{color}) %>") 
end

def setup_colors
    unless @setup_colors
    ft = HighLine::ColorScheme.new do |cs|
      cs[:headline]         = [ :bold, :yellow, :on_black ]
      cs[:horizontal_line]  = [ :bold, :white, :on_blue]
      cs[:critical]         = [ :yellow, :on_red  ]
      cs[:error]            = [ :bold, :red  ]
      cs[:help]             = [ :bold, :white, :on_blue]
      cs[:notice]           = [ :blue, :on_white]
    end

    HighLine.color_scheme = ft
    @setup_colors = true
  end
end

def are_you_sure?(msg)
  puts msg
  resp = gets.strip!

  case resp
  when "Y"
  when "yes"
  when "y"
    return true
  else
    return false
  end
end