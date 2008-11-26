module Spec
  module Matchers
    module SpecExtensions
      class HaveVariable < SpecExtensionsBase
        def initialize(varname, expected,extra="")
          @varname = varname
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          is_present?
        end
        def is_present?
          @target.match(/\$#{@varname}(\W+)=(\W+)#{@expected}/)[0]
        end
        def failure_message
          "expected #{@target.inspect} to have variable #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have variable #{@expected}"
        end
        def type
          "variable"
        end
      end
    end
    def have_variable(name, extra="")
      SpecExtensions::HaveVariable.new(name, extra)
    end
  end
end