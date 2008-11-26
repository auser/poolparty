module Spec
  module Matchers
    module SpecExtensions
      class HaveDeploydirectory < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have deploydirectory #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have deploydirectory #{@expected}"
        end
        def type
          "deploydirectory"
        end
      end
    end
    def have_deploydirectory(name, extra="")
      SpecExtensions::HaveDeploydirectory.new(name, extra)
    end
  end
end