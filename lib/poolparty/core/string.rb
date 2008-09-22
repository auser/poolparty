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
  def nice_runnable(quite=true)
    self.split(/ && /).join("\n")
  end
  def class_constant(superclass=nil)
    symc = "#{self}_Class".classify
    unless Object.const_defined?(symc)
      klass = Class.new(superclass ? superclass : Object) do
        yield if block_given?
      end
      Object.const_set(symc, klass) 
    end
    symc.constantize
  end
  def module_constant(&block)
    symc = "#{self}_Module".classify
    mod = Module.new(&block)    
    Object.const_set(symc, mod) unless Object.const_defined?(symc)
    symc.to_s.constantize
  end
  def preserved_module_constant(ext="", &block)
    symc = "#{self}#{ext}".classify
    mod = Module.new(&block)    
    Object.const_set(symc, mod) unless Object.const_defined?(symc)
    symc.to_s.constantize
  end
  def collect_each_line_with_index(&block)
    returning [] do |arr|
      arr << self.split(/\n/).collect_with_index(&block)
    end.flatten
  end
end