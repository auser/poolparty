require "rubygems"
require "spec"
require "#{::File.dirname(__FILE__)}/../matchers/a_spec_extensions_base.rb"
class Object
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
  def create_dynamic_matcher_for(ty, &block)    
    begin
      eval make_dynamic_matcher_string_for(ty)
      klass = "::Spec::Matchers::SpecExtensions::Have#{ty.to_s.capitalize}".constantize
      # klass.module_eval &block if block
      klass
    rescue Exception => e
      puts "e: #{e}"
    end
  end
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