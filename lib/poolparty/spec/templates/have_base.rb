module Spec
  module Matchers
    module SpecExtensions
      class :classname < SpecExtensionsBase
        def initialize(expected,extra="")
          @expected = expected
          @extra = extra
        end
        def matches?(target)
          @target = target
          required_values.reject {|v| @target.send v, @expected }.empty?
        end
        def required_values
          [:matches]
        end
        def failure_message
          "expected #{@target.inspect} to have :type #{@expected}"
        end
        def negative_failure_message
          "expected #{@target.inspect} not to have :type #{@expected}"
        end
        def type
          ":type"
        end
      end
    end
    def have_:type(name, extra="")
      :includer
    end
  end
end