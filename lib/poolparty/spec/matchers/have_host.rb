module Spec
  module Matchers
    module SpecExtensions
      class HaveHost < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have host #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have host #{@expected}"
        end
        def type
          "host"
        end
      end
    end
    def have_host(name, extra="")
      SpecExtensions::HaveHost.new(name, extra)
    end
  end
end