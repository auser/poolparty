module Spec
  module Matchers
    module SpecExtensions
      class SpecExtensionsBase
        def ensured_as(ensured="present")
          str = "ensure => '#{ensured}'"
          !grab_entry.scan(/#{str}/).empty?
        end
        def is_present?
          !grab_entry.empty?
        end
        def grab_entry
          @target.grab_entry_for(type, @expected)
        end
        def type
          "file"
        end
      end
    end
  end
end