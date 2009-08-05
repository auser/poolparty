# A priority queue implementation.
# This extension contains two implementations, a c extension and a pure ruby
# implementation. When the compiled extension can not be found, it falls back
# to the pure ruby extension.
#
# See CPriorityQueue and RubyPriorityQueue for more information.

begin
  require 'priority_queue/CPriorityQueue'
  PriorityQueue = CPriorityQueue
rescue LoadError # C Version could not be found, try ruby version
  require 'priority_queue/ruby_priority_queue'
  PriorityQueue = RubyPriorityQueue
end
