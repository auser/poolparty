module Spec
  module Matchers
    module SpecExtensions
      class HaveMount < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have mount #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have mount #{@expected}"
        end
        def type
          "mount"
        end
      end
    end
    def have_mount(name, extra="")
      SpecExtensions::HaveMount.new(name, extra)
    end
  end
end