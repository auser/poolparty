module Spec
  module Matchers
    module SpecExtensions
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
          grab_entry.split(/\n/).select {|l| l.match(/(.*)=>([ \t]*),$/) }.empty?
        end
      end
    end
  end
end