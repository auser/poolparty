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
  def arrayable
    self.strip.split(/\n/)
  end
  def runnable(quite=true)
    # map {|l| l << "#{" >/dev/null 2>/dev/null" if quite}" }.
    self.strip.split(/\n/).join(" && ")
  end
  def top_level_class
    self.split("::")[-1].downcase rescue self
  end
  def sanitize
    self.gsub(/[ \.\/\-]*/, '')
  end
  def dir_safe
    self.downcase.gsub(/[ ]/, '_')
  end
  def safe_quote
    self.gsub(/[']/, '\\\\\'')
  end
  def nice_runnable(quite=true)
    self.split(/ && /).join("\n")
  end
  # This is the method we use to turn the options into a string to build the main 
  # manifests
  def to_option_string(ns=[])
    a_template = (self =~ /template/) == 0
    a_service = self =~ /^[A-Z][a-zA-Z]*\[[a-zA-Z0-9\-\.\"\'_\$\{\}\/]*\]/
    a_function = self =~/(.)*\((.)*\)(.)*/
    if is_a?(PoolParty::Resources::Resource)
      self.to_s
    else
      (a_service || a_template || a_function) ? self : "'#{self}'"
    end    
  end
  # Refactor this guy to get the class if the class is defined, and not always create a new one
  # although, it doesn't really matter as ruby will just reopen the class
  def class_constant(superclass=nil, opts={}, &block)
    symc = ((opts && opts[:preserve]) ? ("#{self.classify}Classs") : "PoolParty#{self.classify}Classs").classify
    
    kla=<<-EOE
      class #{symc} #{"< #{superclass}" if superclass}
      end
    EOE
    
    Kernel.module_eval kla
    klass = symc.constantize
    klass.module_eval &block if block
    
    klass
  end
  
  def module_constant(&block)
    symc = "#{self}_Module".classify
    mod = Object.const_defined?(symc) ? Object.const_get(symc.to_sym) : Module.new(&block)
    Object.const_set(symc, mod) unless Object.const_defined?(symc)
    symc.to_s.constantize
  end
  def preserved_module_constant(ext="", from="PoolParty::", &block)
    symc = "#{self}#{ext}".classify
    mod = Kernel.const_defined?(symc) ? Kernel.const_get(symc.to_sym) : Module.new(&block)
    Kernel.const_set(symc, mod) unless Kernel.const_defined?(symc)
    symc.to_s.constantize
  end
  def collect_each_line_with_index(&block)
    returning [] do |arr|
      arr << self.split(/\n/).collect_with_index(&block)
    end.flatten
  end
end