module Spec
  module Matchers
    module SpecExtensions
      class HaveCustomservice < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have customservice #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have customservice #{@expected}"
        end
        def type
          "customservice"
        end
      end
    end
    def have_customservice(name, extra="")
      SpecExtensions::HaveCustomservice.new(name, extra)
    end
  end
end