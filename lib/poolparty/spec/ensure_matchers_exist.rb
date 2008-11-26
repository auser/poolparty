require "rubygems"
require "poolparty"
Dir["#{::File.dirname(__FILE__)}/core/*.rb"].each {|f| require f}

if @ensure_matchers_exist
  @basestr = open("#{::File.dirname(__FILE__)}/have_base.rb").read

  PoolParty::Resources::Resource.available_resources.each do |ty|
    ty.downcase!
    filename = "#{::File.dirname(__FILE__)}/matchers/have_#{ty}.rb"
    unless ::File.file?(filename)
      str = @basestr ^ {:classname => "Have#{ty.capitalize}", 
                        :type => ty,
                        :includer => "SpecExtensions::Have#{ty.capitalize}.new(name, extra)"}

      ::File.open(filename, "w+") {|f| f << str}
    end
  end  
end

Dir["#{::File.dirname(__FILE__)}/matchers/*.rb"].each {|f| require f}