module Spec
  module Matchers
    module SpecExtensions
      class HaveGempackage < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have gempackage #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have gempackage #{@expected}"
        end
        def type
          "gempackage"
        end
      end
    end
    def have_gempackage(name, extra="")
      SpecExtensions::HaveGempackage.new(name, extra)
    end
  end
end