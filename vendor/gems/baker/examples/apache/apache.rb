=begin rdoc
  Apache cookbook
  
  with Baker
=end
$:.unshift("#{File.dirname(__FILE__)}/../../")
require "lib/baker"

meal "#{File.dirname(__FILE__)}/output" do
  
  attribute :case => {
    :of => "platform",
    :
  }
  
end