require "#{File.dirname(__FILE__)}/../../test_helper"
$:.unshift("#{File.dirname(__FILE__)}/../../../lib/proto")
require "command_query_handler"

class CommandQueryHandlerTest < Test::Unit::TestCase

  def test_has_run_query_command
    assert CommandQueryHandler.respond_to?(:run_query)
  end
  
end