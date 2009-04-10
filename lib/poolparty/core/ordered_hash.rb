# Credit to Brian Candler on a ruby-forum post for this
# http://www.ruby-forum.com/topic/166075
require 'rubygems'
require 'json'

class OrderedHash
	include Enumerable
	def initialize
		@h = {}
		@a = []
	end
	def [](k)
		@h[k]
	end
	def []=(k,v)
		@a << k unless @h.has_key?(k)
		@h[k] = v
	end
	def merge(o={})
		na = dup
		o.each {|k,v| na[k] = v }
		na
	end
	def keys
		map {|k,v| k }
	end
	def has_key?(k)
	 
	end
	def values
		map {|k,v| v }
	end
	def size
		@h.size
	end
	def merge!(o={})
		o.each {|k,v| self[k] = v }
	end
	def each
		@a.each { |k| yield k,@h[k] }
	end
	
	#TODO: Need specs below here
	def each_value
		@a.each {|k| yield(@h[k])}
	end
	def each_key
		@a.each {|k| yield k}
	end
	def clear
		@a.clear
		@h.clear
	end
	def delete(k, &block)
		@a.delete k
		@h.delete(k, &block)
	end
	def reject!
		del = []
		each_pair {|k,v| del << k if yield k,v}
		del.each {|k| delete k}
		del.empty? ? nil : self
	end
	def delete_if(&block)
		reject!(&block)
		self
	end
	def method_missing(*args)
	  @h.send(*args)
	end
	def to_hash
		self
	end
	def to_h
		self
	end
	def to_json
	  
	  "{#{collect{|k, v| "\"#{k.to_s}\":#{v.to_json}"}.join(',')}}"
	end
end

# {}.to_json
# oh2= OrderedHash.new
# oh2[:thing] = 'majobber'
# oh2['stuff'] = 'bb'
# 
# oh = OrderedHash.new
# oh[nil] = nil
# oh[:var1]=3
# oh['time']=Time.now
# oh[3]=[1,2,3]
# oh['false'] = false
# oh['ordered_hash'] = oh2
# oh["var4"]={:keyme=>'valueme'}
# oh.each { |k,v| puts "#{k}=>#{v}" }
# p oh.to_json


