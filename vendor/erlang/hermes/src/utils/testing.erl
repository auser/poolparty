-module (testing).
-include ("hermes.hrl").
-compile (export_all).

% f(). Str = "name=baskinrobbins&boxes=full". testing:turn_equal_to_json(Str).
turn_equal_to_json(Str) ->
  Set = string:tokens(Str, "&"),
  ArrSet = lists:map(
    fun(Line) -> 
      string:tokens(Line, "=")
    end, Set),
  lists:map(fun([K|V]) -> "{'"++K++"':'"++erlang:list_to_binary(V)++"'}" end, ArrSet).
 
spawn_irb() ->
  Cmd = lists:flatten([
    os:find_executable("irb"), 
    " --simple-prompt",
    " -r/Users/alerner/Development/ruby/mine/poolparty/lib/poolparty",
    " -r/Users/alerner/Development/ruby/mine/poolparty/examples/simple.rb"
  ]),
  io:format("Running: ~p~n", [Cmd]),
  open_port({spawn, Cmd}, [ eof, {line, 10}, stream, exit_status ]).
  
irb_get(Port, Cmd) ->  
  Rand = lists:append(erlang:integer_to_list(1), erlang:integer_to_list(random:uniform(100000))),
  SendCommand = lists:append([Cmd, ".to_s+", Rand, ".to_s", "\n"]),
  io:format("Sending: ~p~n", [SendCommand]),
  irb_get(Port, SendCommand, Rand).
  
irb_get(Port, Cmd, Rand) ->
  Timeout = 10000,
  io:format("Sending: ~p~n", [Cmd]),
  port_command(Port, Cmd),
  case collect_response(Port, Cmd, Rand, Timeout) of
    {response, []}  -> 
      irb_get(Port, Cmd, Rand);
    {response, O}   -> O
  end.

collect_response(Port, Command, Rand, Timeout) ->
    collect_response(Port, [], [], Command, Rand, Timeout ).

collect_response( Port, RespAcc, LineAcc, Command, Rand, Timeout) ->
    receive
      {Port, {data, {eol, Command}}} ->
        collect_response(Port, RespAcc, [], Command, Rand, Timeout);
      {Port, {data, {eol, ">> " ++ _Eh }}} ->
        collect_response(Port, RespAcc, [], Command, Rand, Timeout);
      {Port, {data, {eol, End}}} ->
        io:format("Got eol: ~p in ~p~n", [lists:flatten([LineAcc,End]), Rand]),
        String = lists:flatten([LineAcc,End]),
        case regexp:first_match(String, ">> "++clean_regexp(Command)) of
          nomatch ->
            io:format("Not returning the command: ~p, but ~p~n", [Command, String]),
            {response, irb_strip_from(LineAcc, Rand)};
          {match, _, _} -> 
            io:format("The command: ~p~n", [Command]),
            {response, []}
        end;        
      {Port, {data, {noeol, Result}}} ->
        io:format("Got noeol: ~p~n", [Result]),
        collect_response(Port, RespAcc, [LineAcc|Result], Command, Rand, Timeout)

    after Timeout -> 
      {response, lists:flatten(lists:append([RespAcc]))}
    end.     

%%--------------------------------------------------------------------
%% Function: strip_from (Str, Regexp) -> {ok, NewStr} | {error, Reason}
%% Description: Remove the appends that irb puts on to the response
%%--------------------------------------------------------------------
irb_strip_from(Input, Appended) ->
  {ok, String, _} = regexp:gsub(Input, "\"", ""),
  {ok, Str, _} = regexp:sub(String, "=> ", ""),
  case regexp:first_match(Str, Appended) of
    nomatch -> 
      io:format("NO MATCH"),
      {error, "no match"};
    {match, Start, _} ->
      Res = lists:sublist(Str, Start-1),
      % io:format("Got it with ~p (from ~p [~p])~n", [Res, String, Start]),
      {ok, Res}
  end.
  

clean_regexp(String) ->
  Ns1 = clean_regexp_brackets(String),
  clean_regexp_periods(Ns1).

clean_regexp_periods(String) ->
  case regexp:gsub(String, "\\.", "\\.") of
    {ok, NS1, _} -> NS1;
    _ -> String
  end.
clean_regexp_brackets(String) ->
  Ns = case regexp:gsub(String, "\\[", "\\[") of
    {ok, NS, _} -> NS;
    _ -> String
  end,
  case regexp:gsub(Ns, "\\]", "\\]") of
    {ok, NS1, _} -> NS1;
    _ -> Ns
  end.

%%--------------------------------------------------------------------
%% Function: create_fixture_rrds () -> {ok}
%% Description: Create fixture rrd files in the fixtures directory
%%--------------------------------------------------------------------
create_fixture_rrds() -> create_fixture_rrds(["0.2","0.3","0.2","0.1","0.1","0.4", "0.7","0.9","1.5","0.9","0.1","0.1"]).
  
create_fixture_rrds(Values) ->
  Fixtures = [cpu, memory, disk],
  SubTypes = [idle, free, used],  
  
  {Mega, Secs, _} = now(),
  StartTime = Mega*1000000 + Secs,
  
  % Create the directories
  lists:map(
    fun(Module) ->
      file:make_dir(lists:append( [ ?RRD_DIRECTORY, "/", erlang:atom_to_list(Module), "/" ]))
  end, Fixtures),
  
  % Create the rrds
  lists:map(
    fun(Module) ->
      lists:map(fun(SubType) ->
          Ras = lists:append([" --start ", erlang:integer_to_list(StartTime), 
                              " DS:", lists:append([erlang:atom_to_list(Module), "-", erlang:atom_to_list(SubType)]), ":GAUGE:600:0:1250000 RRA:AVERAGE:0.5:1:24 RRA:LAST:0.5:6:10"
                              ]),

          Basedir = lists:append( [ ?RRD_DIRECTORY, "/", erlang:atom_to_list(Module), "/" ]),
          File = lists:append([ Basedir, erlang:atom_to_list(Module), "-", erlang:atom_to_list(SubType), ".", "rrd"]),
          
          file:delete(File),
          Meth = lists:append([File, Ras]),
          erlrrd:create(Meth)
        end, SubTypes)
    end, Fixtures),
  % Update the rrds with some fake data
  lists:map(
    fun(Module) -> 
      % ?TRACE("Creating fixture data for ~p~n", [Module, Values]),
      % Update with the list of values
      lists:zipwith(
        fun(Value, Count) ->
          BDir = lists:append( [ ?RRD_DIRECTORY, "/", erlang:atom_to_list(Module), "/" ]),
          
          lists:map(fun(SubType) ->
          
            M = lists:append([
              BDir, erlang:atom_to_list(Module), "-", erlang:atom_to_list(SubType), ".", "rrd", % File test/fixtures/fixture.rrd
              " ", % Space
              erlang:integer_to_list(StartTime + 600*Count), ":", Value % Value -> Time:Value
            ]),
            erlrrd:update(M)
          end, SubTypes)
        end, Values, lists:seq(1, erlang:length(Values)))
    end, Fixtures).