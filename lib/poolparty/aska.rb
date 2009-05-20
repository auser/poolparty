=begin rdoc
  Aska
  
  A rule parser.
  
  TODO: Extract this into a proper gem
=end
module Aska
  module ClassMethods
    def rules(name=:rules, arr=[], create_vars=true)
      rs = look_up_rules(name)
      # returning look_up_rules(name) do |rs|
      arr.each do |line|
        next unless line
        rule = Rule.new(line)
        next unless rule.valid?
        k = rule.key
        v = rule.var
        m = rule.comparison

        create_instance_variable(k, create_vars)
        rs << {k => [m, v]}
        rs << {k => [">", "0"]} unless rs.reject {|a| a.to_s == "#{k}>0" }
      end
      self.send(:define_method, name) do
        look_up_rules(name)
      end
      rs
      # end
    end
    def create_instance_variable(k, create_vars=true)
      aska_attr_accessors << k.to_sym unless aska_attr_accessors.include?(k.to_sym)
      if create_vars
        attr_reader k.to_sym unless respond_to?("#{k}".to_sym)
        attr_writer k.to_sym unless respond_to?("#{k}=".to_sym)
      end
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
    def __aska_send_value(m)
      if respond_to?(m.to_sym)
        self.send(m.to_sym)
      else
        m
      end
    end
    def valid_rule?(rule)
      rule.each do |key,value|
        begin
          return __aska_send_value(key).send(value[0].to_sym, __aska_get_var(value[1]))
        rescue Exception => e
          return false
        end
      end
    end
    def rules_values
      returning Array.new do |arr|
        self.class.defined_rules.each do |name,rule_array|
          arr << "#{name} : #{valid_rules?(name.to_sym)}"
          rule_array.each do |rule|
            rule.map do |k,v|
              arr << "  #{k} -> #{__aska_send_value(k)} (#{v[0]} #{v[1]})"
            end
          end
        end
      end.join("\n")
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
  end
  
  def self.included(receiver)
    receiver.extend         ClassMethods
    receiver.send :include, InstanceMethods
  end
  
  class Rule < String
    attr_accessor :value
    
    def initialize(v)
      @value = v
    end
    def valid?
      value =~ /(.+)[=\\\<\>](.*)/
    end
    def key
      value[/(.+)[=\\\<\>](.*)/, 1].gsub(/\s+/, '')
    end
    def comparison
      value[/[=\\<>]/, 0].gsub(/\s+/, '')
    end
    def var
      value[/(.+)[=\\<>](.*)/, 2].gsub(/\s+/, '')
    end
    def value
      @value ||= ""
    end
  end
  
  class Rules < Array
    def to_s
      self.map {|r| v=r.keys.first;"'#{v} #{r[v][0]} #{r[v][1]}'"}.join(", ")
    end
  end
end