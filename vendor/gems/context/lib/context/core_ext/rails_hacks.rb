class Test::Unit::TestCase
  class << self
    def setup_for_shoulda
      self.instance_eval do
        alias :setup :before
        alias :teardown :after
      end
    end
  end
end
