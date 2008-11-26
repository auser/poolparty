module Spec
  module Matchers
    module SpecExtensions
      class HaveSshkey < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def failure_message
          "expected #{@target.inspect} to have sshkey #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have sshkey #{@expected}"
        end
        def type
          "sshkey"
        end
      end
    end
    def have_sshkey(name, extra="")
      SpecExtensions::HaveSshkey.new(name, extra)
    end
  end
end