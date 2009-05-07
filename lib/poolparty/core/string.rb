class String
  def hasherize(format=[])
    hash = {}
    i = 0
    self.split(%r{[\n|\t|\s| ]+}).map {|a| a.strip}.each do |f|
      next unless format[i]
      unless f == "" || f.nil?
        hash[format[i].to_sym] = f
        i+=1
      end      
    end
    hash
  end
  def ^(h={})
    self.gsub(/:([\w]+)/) {h[$1.to_sym] if h.include?($1.to_sym)}
  end
  def grab_filename_from_caller_trace
    self.gsub(/\.rb(.*)/, '.rb')
  end
  def arrayable
    self.strip.split(/\n/)
  end
  def runnable(quite=true)
    # map {|l| l << "#{" >/dev/null 2>/dev/null" if quite}" }.
    self.strip.split(/\n/).join(" && ")
  end
  def top_level_class
    self.split("::")[-1].underscore.downcase rescue self.class.to_s
  end
  def sanitize
    self.gsub(/[ \.\/\-]*/, '')
  end
  def keyerize
    signed_short = 0x7FFFFFFF
    len = self.sanitize.length
    hash = 0 
    len.times{ |i| 
      hash = self[i] + ( hash << 6 ) + ( hash << 16 ) - hash 
    } 
    hash & signed_short
  end
  def dir_safe
    self.downcase.gsub(/[ ]/, '_')
  end
  def safe_quote
    self.gsub(/['"]/, '\\\"')
    # self.gsub(/["']/, "\\\"")
  end
  def path_quote
    self.safe_quote.gsub(/[ ]/, '\ ')
  end
  def nice_runnable(quite=true)
    self.split(/ && /).join("\n")
  end

  # Refactor this guy to get the class if the class is defined, and not always create a new one
  # although, it doesn't really matter as ruby will just reopen the class
  def class_constant(superclass=nil, opts={}, &block)
    symc = ((opts && opts[:preserve]) ? ("#{self.camelcase}Class") : "PoolParty#{self.camelcase}Class").classify

    kla=<<-EOE
      class #{symc} #{"< #{superclass}" if superclass}
      end
    EOE
    
    Kernel.module_eval kla
    klass = symc.constantize
    klass.module_eval &block if block
    
    klass
  end
  
  def new_resource_class(superclass=nil, opts={}, &block)
    symc = "::PoolParty::Resources::#{self.camelcase}"
    kla=<<-EOE
      class #{symc} < ::PoolParty::Resources::Resource
      end
    EOE
    
    Kernel.module_eval kla
    klass = symc.constantize
    klass.module_eval &block if block

    klass
  end
  
  def camelcase
    gsub(/(^|_|-)(.)/) { $2.upcase }
  end
  def camelize
    camelcase
  end
  
  # "FooBar".snake_case #=> "foo_bar"
   def snake_case
     gsub(/\B[A-Z]+/, '_\&').downcase
   end
   def underscore
     snake_case
   end
   
    # "FooBar".dasherize #=> "foo-bar"
    def dasherize
      gsub(/\B[A-Z]+/, '-\&').downcase
    end
    
    def classify
      self.sub(/.*\./, '').camelcase
    end
    
    #TODO: implement here to drop activesupport
    # # Form can be either :utc (default) or :local.
    # def to_time(form = :utc)
    #   ::Time.send("#{form}_time", *::Date._parse(self, false)(:year, :mon, :mday, :hour, :min, :sec).map { |arg| arg || 0 })
    # end
    
    
  # Constantize tries to find a declared constant with the name specified
  # in the string. It raises a NameError when the name is not in CamelCase
  # or is not initialized.
  #
  # Examples
  #   "Module".constantize #=> Module
  #   "Class".constantize #=> Class
  def constantize(mod=Object)
    camelcased_word = camelcase
    begin
      mod.module_eval(camelcased_word, __FILE__, __LINE__)
    rescue NameError
      puts "#{camelcased_word} is not defined."
      nil
    end
  end
  
  def preserved_class_constant(append="")
    klass = "#{self}#{append}".classify
    Object.const_defined?(klass.to_sym) ? klass.to_s.constantize : nil
  end
  
  def module_constant(append="", &block)
    symc = "#{self}_Module#{append}".camelcase
    mod = Object.const_defined?(symc) ? Object.const_get(symc.to_sym) : Module.new(&block)
    Object.const_set(symc, mod) unless Object.const_defined?(symc)
    symc.to_s.constantize
  end
  def preserved_module_constant(ext="", from="PoolParty::", &block)
    symc = "#{self}#{ext}".camelcase
    mod = Kernel.const_defined?(symc) ? Kernel.const_get(symc.to_sym) : Module.new(&block)
    Kernel.const_set(symc, mod) unless Kernel.const_defined?(symc)
    symc.to_s.constantize
  end
  def collect_each_line_with_index(&block)
    returning [] do |arr|
      arr << self.split(/\n/).collect_with_index(&block)
    end.flatten
  end
  
  # Read a new-line separated string and turn 
  # the string from the form 
  #   a = "b"
  #   b = "c"
  # into a hash
  #  {:a => "b", :b => "c"}
  def to_hash
    split("\n").inject({}) do |sum,line| 
      if line.include?("=")
        l = line.split("=").map {|a| a.strip }
        key = l[0].to_sym
        value = l[1].gsub(/\"/, '')
        sum.merge(key => value)
      else
        sum
      end
    end
  end
  
  # Take a mac address and split it to map to the arp -a response
  # Just rip off the first 0 if the first char is 0
  def macify
    split(":").map {|a| a[0].chr == "0" ? a[1].chr : a}.join(":")
  end

  ##
  # @param o<String> The path component to join with the string.
  #
  # @return <String> The original path concatenated with o.
  #
  # @example
  #   "merb"/"core_ext" #=> "merb/core_ext"
  def /(o)
    File.join(self, o.to_s)
  end
  
end