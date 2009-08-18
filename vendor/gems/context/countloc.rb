
def extract_path(argv)
  if argv[1].nil?
    if argv[0] =~ /-a/
      return "/**/*.rb"
    elsif argv[0]
      if argv[0] =~ /\.rb$/
        return argv[0]
      end
      return argv[0] + "/**/*.rb"
    else
      return "/**/*.rb"
    end
  elsif argv[1] =~ /\.rb$/
    return argv[1]
  else
    return argv[1] + "/**/*.rb"
  end
end

def all?
  ARGV.join =~ /-a/
end

def comment?(line)
  line =~ /^\s*#/
end

def blank?(line)
  line =~ /^\s*$/
end

def puke(header, locs, comments, blanks)
  puts header + ":"
  puts "#{locs} loc"
  puts "#{comments} lines of comments"
  puts "#{blanks} blank lines"
end

dir = File.dirname(__FILE__)
full_path = File.join(dir,extract_path(ARGV))
gloc = gcmts = gblanks = 0
Dir[File.expand_path("#{full_path}")].uniq.each do |file|
  if file =~ /.*\.rb$/ 
    
    loc = cmts = blanks = 0

    File.open(file, "r") do |f|
      while f.gets
        if comment?($_)
          cmts += 1
        elsif blank?($_)
          blanks += 1
        else
          loc += 1
        end
      end
    end
    gcmts   += cmts
    gloc    += loc
    gblanks += blanks 
    puke(file, loc, cmts, blanks) if all?
  end
end
puke("Total", gloc, gcmts, gblanks)
