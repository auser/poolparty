module Spec
  module Matchers
    module SpecExtensions
      class HaveSymlink < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have symlink #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have symlink #{@expected}"
        end
        def type
          "symlink"
        end
      end
    end
    def have_symlink(name, extra="")
      SpecExtensions::HaveSymlink.new(name, extra)
    end
  end
end