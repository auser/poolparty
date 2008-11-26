module Spec
  module Matchers
    module SpecExtensions
      class :classname
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          @target =~ /:type(\W+)#{@expected}#{@extra}(\.*)/
          # @target =~ :matcher
        end
        def failure_message
          "expected #{@target.inspect} to have :type #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have :type #{@expected}"
        end
      end
    end
    def have_:type(name, extra="")
      :includer
    end
    def not_have_:type(name, extra="")
      :includer
    end
  end
end