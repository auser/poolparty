module Spec
  module Matchers
    module SpecExtensions
      class HaveRsyncmirror < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have rsyncmirror #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have rsyncmirror #{@expected}"
        end
        def type
          "rsyncmirror"
        end
      end
    end
    def have_rsyncmirror(name, extra="")
      SpecExtensions::HaveRsyncmirror.new(name, extra)
    end
  end
end