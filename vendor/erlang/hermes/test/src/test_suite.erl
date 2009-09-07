-module (test_suite).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [
    % {module, hermes_logger_test},
    {module, config_test},
    {module, rest_server_test},
    {module, utils_test},
    {module, athens_test},
    {module, mon_server_test},
    {module, mapreduce_test}
  ].