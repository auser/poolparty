module Spec
  module Matchers
    module SpecExtensions
      class SpecExtensionsBase
        def ensured_as(ensured="present")
          str = "ensure => '#{ensured}'"
          !grab_entry.scan(/#{str}/).empty?
        end
        def is_present?
          !grab_entry.empty? && valid?
        end
        def grab_entry
          @target.grab_entry_for(type, @expected)
        end
        def type
          "file"
        end
        def valid?
          grab_entry.split(/\n/).select {|l| l.match(/(.*)=>([ \t]*),$/) }.empty?
        end
      end
    end
  end
end