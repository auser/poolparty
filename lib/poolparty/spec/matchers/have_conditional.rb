module Spec
  module Matchers
    module SpecExtensions
      class HaveConditional < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have conditional #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have conditional #{@expected}"
        end
        def type
          "conditional"
        end
      end
    end
    def have_conditional(name, extra="")
      SpecExtensions::HaveConditional.new(name, extra)
    end
  end
end