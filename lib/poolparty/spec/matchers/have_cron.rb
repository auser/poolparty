module Spec
  module Matchers
    module SpecExtensions
      class HaveCron < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have cron #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have cron #{@expected}"
        end
        def type
          "cron"
        end
      end
    end
    def have_cron(name, extra="")
      SpecExtensions::HaveCron.new(name, extra)
    end
  end
end