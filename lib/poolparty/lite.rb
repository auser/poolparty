require "rubygems"
require "json"
begin
  require "#{::File.dirname(__FILE__)}/../../vendor/gems/dslify/lib/dslify"
rescue Exception => e
  require "dslify"
end
require "#{::File.dirname(__FILE__)}/poolparty/default"
require "#{::File.dirname(__FILE__)}/modules/user_helpers"
require "#{::File.dirname(__FILE__)}/schema"
require "#{::File.dirname(__FILE__)}/net/init"
require "#{::File.dirname(__FILE__)}/core/string"
require "#{::File.dirname(__FILE__)}/core/hash"
require "#{::File.dirname(__FILE__)}/poolparty/neighborhoods"
require "#{::File.dirname(__FILE__)}/exceptions/RemoteException.rb"

module PoolParty
  extend ::PoolParty::Remote
  
  def self.load_cloud_from_json(json_file_path=nil)
    json_file = json_file_path || PoolParty::Default.properties_hash_file
    PoolParty::Schema.new( ::File.read(json_file) ) rescue exit 1
  end  
end

class String
  def camelcase
    gsub(/(^|_|-)(.)/) { $2.upcase }
  end
end