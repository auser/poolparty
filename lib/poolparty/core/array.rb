=begin rdoc
  Array extensions
=end
require "enumerator"
class Array
  def to_os
    map {|a| a.to_os }
  end
  def collect_with_index &block
    self.enum_for(:each_with_index).collect &block
  end
  def runnable(quiet=true)
    self.join(" \n ").runnable(quiet)
  end
  def nice_runnable(quiet=true)
    reject{|a| a.nil? || a.empty?}.join(" \n ").chomp.nice_runnable(quiet)
  end
  def to_string(prev="")
    map {|a| a.to_string(prev)}.join("\n")
  end
  def get_named(str="")
    map {|a| a.name == str ? a : nil }.reject {|a| a.nil? }
  end
end