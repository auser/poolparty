module Dslify
  def self.included(base)
    base.send     :include, InstanceMethods
    base.extend(ClassMethods)
  end
  
  module ClassMethods    
    def default_options(hsh={})
      (@_dsl_options ||= {}).merge! hsh
      set_default_options(@_dsl_options)
    end
    
    def dsl_options
      @_dsl_options ||= {}
    end
    def options
      dsl_options
    end
    
    def dsl_methods(*syms)
      syms.each {|sym| set_default_options({sym => nil}) }
    end
    
    def set_default_options(new_options)
      new_options.each do |k,v|
        dsl_options[k] = v
        class_eval define_dsl_method_str(k)
      end
    end
    
    def define_dsl_method_str(k)
      <<-EOE
        def #{k}(n=nil)
          if n.nil?
            fetch(:#{k})
          else
            self.#{k}=n
          end          
        end
        def #{k}=(n)
          dsl_options[:#{k}] = n
        end
        def fetch(k)
          dsl_options[k]                    
        end
      EOE
    end
    
    def inherited(subclass)
      subclass.set_default_options(dsl_options)
    end
  end
  module InstanceMethods
    def dsl_options
      @dsl_options ||= self.class.dsl_options.clone
    end
    def default_options
      Hash[*dsl_options.select{|k,v| self.class.default_options.has_key?(k) }.inject([]){|res,(k,v)| res << k << v }]
    end
    def set_vars_from_options(hsh={})
      hsh.each do |k,v| 
        instance_eval self.class.define_dsl_method_str(k) unless self.respond_to?(k)
        self.send k, v
      end
    end
    
    def set_default_options(hsh={})
      self.class.set_default_options(hsh)
    end
    
    def method_missing(m,*a,&block)
      if m.to_s[-1..-1] == '?'
        t = m.to_s.gsub(/\?/, '').to_sym
        warn "DEPRECATED: Dslify will no longer support ? methods. Fix yo code.: #{m}"
        respond_to?(t) && !self.send(t, *a, &block).nil?
      else
        super
      end
    end
  end
end