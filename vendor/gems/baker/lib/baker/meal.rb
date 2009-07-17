=begin rdoc
  The meal of the baker
=end
module Baker
  class Meal
    
    attr_reader :cookbook_directory
    
    def initialize(cookbook_directory, opts={})
      raise StandardError.new("You must pass a directory to make the meal") unless cookbook_directory && cookbook_directory.is_a?(String)
      @cookbook_directory = cookbook_directory
      FileUtils.rm_rf cookbook_directory if File.directory?(cookbook_directory)
      ::FileUtils.mkdir_p cookbook_directory unless ::File.directory?(cookbook_directory)
    end
    
    def compile      
      compileables.each do |compileable|
        self.send(:"#{compileable}s").each {|item| item.compile }
      end
    end
    
    def compileables
      [:template, :recipe, :files, :attribute]
    end
    
    def json(str, &block)
    end
    
    %w(template recipe files attribute).each do |meth|
      module_eval <<-EOE
def #{meth}(*arr)
  arr.each do |temp|
    tfile = File.expand_path(temp)
    if File.file?(tfile)
      #{meth}s << Baker::#{meth.capitalize}.new(:file => tfile, :cookbook_directory => cookbook_directory) unless #{meth}_files.include?(tfile)
    elsif File.directory?(tfile)
      Dir["\#{tfile}/**/*"].each do |t|
        #{meth}(File.expand_path(t))
      end
    else
      raise StandardError.new("Meal #{meth} accepts only files or directories. Please check your call to #{meth}(\#{tfile})")
    end
  end
end
alias :#{meth}s :#{meth}

def #{meth}_files
  #{meth}s.map {|a| a.file }
end

def #{meth}s
  @#{meth}s ||= []
end      
      EOE
    end
    
    def recipe(*args)
      args.each do |r|
        f = File.file?(r)
        opts = f ? {:file => r, :cookbook_directory => cookbook_directory} : {:string => r, :cookbook_directory => cookbook_directory}
        recipes << Baker::Recipe.new(opts)
      end
    end
    
    def attribute(hsh)
      att = Baker::Attributes.new
      att.variables hsh
      attributes << att
      att
    end
        
  end
end