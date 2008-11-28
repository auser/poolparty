module Spec
  module Matchers
    module SpecExtensions
      # Mainly extensions, these methods are inherent in all of the matchers
      # and are intended for helping parse the final manifest for spec'ing purposes.
      class SpecExtensionsBase
        def ensured_as(ensured="present")
          str = "ensure => '#{ensured}'"
          !grab_entry.scan(/#{str}/).empty?
        end
        def is_present?
          !grab_entry.empty? && is_valid_resource?
        end
        def grab_entry
          @target.grab_entry_for(type, @expected)
        end
        def type
          "file"
        end
        def is_valid_resource?
          grab_entry.split(/\n/).select {|l| l.match(/(.*)=>(\W+),$/) }.empty?
        end
      end
    end
  end
end