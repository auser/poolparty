module Spec
  module Matchers
    module SpecExtensions
      class HaveFile < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have file #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have file #{@expected}"
        end
        def type
          "file"
        end
      end
    end
    def have_file(name, extra="")
      SpecExtensions::HaveFile.new(name, extra)
    end
  end
end