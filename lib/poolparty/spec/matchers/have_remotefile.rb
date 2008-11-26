module Spec
  module Matchers
    module SpecExtensions
      class HaveRemotefile < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have remotefile #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have remotefile #{@expected}"
        end
        def type
          "remotefile"
        end
      end
    end
    def have_remotefile(name, extra="")
      SpecExtensions::HaveRemotefile.new(name, extra)
    end
  end
end