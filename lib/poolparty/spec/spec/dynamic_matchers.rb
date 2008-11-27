require "rubygems"
require "spec"
require "#{::File.dirname(__FILE__)}/../matchers/a_spec_extensions_base.rb"
class Object
  # For all of the available_resources, write the matcher template
  # based on the have_base base and write the method have_
  # for each of the resources available
  def write_dynamic_matchers
    PoolParty::Resources::Resource.available_resources.each do |ty|
      ty.downcase!
      filename = "#{::File.dirname(__FILE__)}/../matchers/have_#{ty}.rb"
      unless ::File.file?(filename)
        str = make_dynamic_matcher_string_for(ty)
        ::File.open(filename, "w+") {|f| f << str}
      end
    end
  end
  # Create a dynamic matcher
  # Create a matcher for spec based in the context called from
  # Usage example:
  #   dynamic_matcher_for(:virtualhost) do
  #     set_description "should have virtualhost"
  #     it "should have the directory /var/www" do        
  #       have_directory("/var/www")
  #     end
  #   end
  # This creates a method by the name of the dynamic matcher
  # so in the above example for virtualhost, the 
  # method have_virtualhost(name) &block is created in the spec 
  # context and can be called from within the describe/context
  # in a spec
  def dynamic_matcher_for(ty, &block)
    name = "#{ty}".camelcase
    
    begin
      const = Object.const_set(name, Spec::Example::SharedExampleGroup.register(name.to_sym, &block))
    rescue NameError => e
      raise NameError.new(e.message + "\nThe first argument to share_as must be a legal name for a constant\n")
    end
    
    described_type = "#{ty.to_s.underscore.downcase}"    
    
    ::Spec::Example::ExampleGroupMethods.module_eval <<-EOM
      def have_#{ty.to_s.underscore.downcase}(*args)        
        include #{const}
        self.module_eval "@@described_type = args;def #{described_type}; @@described_type; end"
      end
    EOM
  end
  # Load the have_base file and fill in the variables for the
  # have_base base template
  def make_dynamic_matcher_string_for(ty, matcher="is_present?")
    @basestr ||= open("#{::File.dirname(__FILE__)}/../templates/have_base.rb").read
    typestring = ty.to_s
    begin
      @basestr ^ {:classname => "Have#{typestring.capitalize}",
                  :type => typestring,
                  :matches => matcher,
                  :includer => "SpecExtensions::Have#{typestring.capitalize}.new(name, extra)"}
    rescue Exception => e
    end
  end
end