# Credit to Brian Candler on a ruby-forum post for this
# http://www.ruby-forum.com/topic/166075

class OrderedHash < Hash
  def initialize
    @h, @a = {}, []
    super
  end
  def [](k)
    @h[k]
  end
  def []=(k,v)
    @a << k unless @h.has_key?(k)
    @h[k] = v
  end
  def each
    @a.each { |k| yield k,@h[k] }
  end
end


# oh = OrderedHash.new
# oh["var1"]=123
# oh["var2"]=2
# oh["var3"]=3
# oh["var4"]=23
# oh.each { |k,v| puts "#{k}=>#{v}" }
# puts oh["var2"]
# puts oh.collect { |k,v| k }.inspect  #=> ["var1", "var2", "var3", "var4"]

# result =>
# var1=>123
# var2=>2
# var3=>3
# var4=>23
# 2
