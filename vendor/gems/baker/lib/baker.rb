$:.unshift("#{File.dirname(__FILE__)}/baker")
require "rubygems"
require "json"

%w(base jsoner template recipe config include_cookbook attributes files meal).each do |lib|
  require "#{lib}"
end

class Object
  def meal(dir, &block)
    m = Baker::Meal.new(dir)
    m.instance_eval &block if block
    m.compile
  end
end