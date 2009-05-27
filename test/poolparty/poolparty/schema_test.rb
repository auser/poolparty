require "#{::File.dirname(__FILE__)}/../../test_helper"

class TestSchema < Test::Unit::TestCase
    should "should be able to load from json string" do
      schema = PoolParty::Schema.new(open(::File.dirname(__FILE__)+'/../../fixtures/metavirt_cloud.json').read)
      assert 'metavirt', schema['name']
      assert 'PoolParty::Remote::Metavirt', schema.options.remote_base      
    end
    should "dump to_json" do
      #TODO
    end

end