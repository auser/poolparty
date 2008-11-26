module Spec
  module Matchers
    module SpecExtensions
      class HaveGit < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have git #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have git #{@expected}"
        end
        def type
          "git"
        end
      end
    end
    def have_git(name, extra="")
      SpecExtensions::HaveGit.new(name, extra)
    end
  end
end