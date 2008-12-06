module Spec
  module Matchers
    module SpecExtensions
      class HaveDirectory < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present? && is_a_directory?
        end
        def is_a_directory?
          ensured_as("directory")
        end
        def failure_message
          "expected #{@target.inspect} to have directory #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have directory #{@expected}"
        end
        def type
          "file"
        end
      end
    end
    def have_directory(name, extra="")
      SpecExtensions::HaveDirectory.new(name, extra)
    end
  end
end