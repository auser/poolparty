module Spec
  module Matchers
    module SpecExtensions
      class HaveCustomresource < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have customresource #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have customresource #{@expected}"
        end
        def type
          "customresource"
        end
      end
    end
    def have_customresource(name, extra="")
      SpecExtensions::HaveCustomresource.new(name, extra)
    end
  end
end