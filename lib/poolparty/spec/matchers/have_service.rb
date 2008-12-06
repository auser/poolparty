module Spec
  module Matchers
    module SpecExtensions
      class HaveService < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have service #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have service #{@expected}"
        end
        def type
          "service"
        end
      end
    end
    def have_service(name, extra="")
      SpecExtensions::HaveService.new(name, extra)
    end
  end
end