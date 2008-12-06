module Spec
  module Matchers
    module SpecExtensions
      class HaveExec < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have exec #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have exec #{@expected}"
        end
        def type
          "exec"
        end
      end
    end
    def have_exec(name, extra="")
      SpecExtensions::HaveExec.new(name, extra)
    end
  end
end