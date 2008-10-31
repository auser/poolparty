=begin rdoc
  Aska
  TODO: Extract this into a proper gem
=end
module Aska
  module ClassMethods
    def rules(name=:rules, arr=[])
      returning look_up_rules(name) do |rs|
        arr.each do |line|
          next unless line
          k = line[/(.+)[=\\\<\>](.*)/, 1].gsub(/\s+/, '')
          v = line[/(.+)[=\\<>](.*)/, 2].gsub(/\s+/, '')
          m = line[/[=\\<>]/, 0].gsub(/\s+/, '')
          
          create_instance_variable(k)
          rs << {k => [m, v]}
        end
        self.send(:define_method, name) do
          look_up_rules(name)
        end        
      end
    end
    def create_instance_variable(k)
      aska_attr_accessors << k.to_sym unless aska_attr_accessors.include?(k.to_sym)
      attr_reader k.to_sym unless respond_to?("#{k}".to_sym)
      attr_writer k.to_sym unless respond_to?("#{k}=".to_sym)
    end
    def look_up_rules(name)
      defined_rules[name.to_sym] ||= Rules.new
    end
    def are_rules?(name)
      !look_up_rules(name).empty?
    end
    def aska_attr_accessors
      @aska_attr_accessors ||= Rules.new
    end
    def defined_rules
      @defined_rules ||= {}
    end
    def aska_named(name)
      "#{name}_aska"
    end
  end
  
  module InstanceMethods
    def rules
      @rules ||= self.class.defined_rules
    end
    def valid_rules?(name=:rules)
      self.class.look_up_rules(name).reject {|rule| valid_rule?(rule) }.empty?
    end
    def __aska_aska_stuff(m)
      if respond_to?(m.to_sym)
        self.send(m.to_sym)
      else
        m
      end
    end
    def valid_rule?(rule)
      rule.each do |key,value|
        begin
          # puts "#{aska(key)} #{value[0].to_sym} #{get_var(value[1])} (#{attr_accessor?(value[1])})"
          return __aska_aska_stuff(key).send(value[0].to_sym, __aska_get_var(value[1]))
        rescue Exception => e
          return false
        end
      end
    end
    # Get the variable from the class
    # If it's defined as an attr_accessor, we know it has been defined as a rule
    # Otherwise, if we are passing it as a 
    def __aska_get_var(name)
      # attr_accessor?(name) ? aska(name) : 
      (supported_method?(name) ? name.to_sym : name.to_f)
    end
    def __aska_aska(name)
      self.class.aska_named(name)
    end
    def attr_accessor?(name)
      self.class.aska_attr_accessors.include?(name.to_sym)
    end
    def supported_method?(meth)
      %w(< > == => =<).include?("#{meth}")
    end
    
    def look_up_rules(r);self.class.look_up_rules(r);end
    def are_rules?(r);self.class.are_rules?(r);end
    
    # def method_missing(m, *args, &block)
    #   if self.class.defined_rules.has_key?(m.to_sym)
    #     self.class.send(:define_method, m) do
    #       self.class.look_up_rules(m)
    #     end
    #     self.send m
    #   else
    #     super
    #   end
    # end
  end
  
  def self.included(receiver)
    receiver.extend         ClassMethods
    receiver.send :include, InstanceMethods
  end
  
  class Rules < Array
    def to_s
      self.map {|r| v=r.keys.first;"'#{v} #{r[v][0]} #{r[v][1]}'"}.join(", ")
    end
  end
end