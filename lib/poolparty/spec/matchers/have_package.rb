module Spec
  module Matchers
    module SpecExtensions
      class HavePackage < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have package #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have package #{@expected}"
        end
        def type
          "package"
        end
      end
    end
    def have_package(name, extra="")
      SpecExtensions::HavePackage.new(name, extra)
    end
  end
end