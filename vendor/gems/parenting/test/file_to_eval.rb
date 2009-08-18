self.class.send :attr_reader, :b
@b = Quickie.new do
  self.class.send :attr_reader, :c
  @c = Quickie.new do
    self.class.send :attr_reader, :d
    $d = @d = Quickie.new do
    end
  end
end