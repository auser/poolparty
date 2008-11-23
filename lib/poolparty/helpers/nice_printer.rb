class NicePrinter
  
  attr_accessor :num_lines
  attr_reader :centered_lines
  
  def initialize(num_lines=60)
    @num_lines = num_lines
    @centered_lines = @num_lines - 4
  end
  
  def center(line)
    lines << "* #{line.center(@centered_lines)} *"
  end
  def left(line)
    lines << "* #{line.ljust(@centered_lines)} *"
  end
  def header
    lines << "*"*@num_lines
  end
  def footer
    header
  end
  def empty
    lines << "* #{" ".ljust(@centered_lines)} *"
  end
  def print
    puts lines.join("\n")
    ""
  end
  def <<(line)
    left(line)
  end
  def lines
    @lines ||= []
  end
end