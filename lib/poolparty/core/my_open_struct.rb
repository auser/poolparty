=begin rdoc
  Create an extended open struct
=end
require "ostruct"
class MyOpenStruct < OpenStruct
  attr_accessor :keys
  def initialize(h)
    @keys = h.keys
    super
  end
  def to_hash
    m = {}
    @keys.map do |key|
      m.update( {key => self.send(key)} )
    end
    m
  end
end