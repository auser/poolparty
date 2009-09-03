-module(erlrrd).
-include_lib ("eunit/include/eunit.hrl").

-export([create/1, update/1, updatev/1, dump/1, restore/1, last/1,
         first/1, info/1, fetch/1, tune/1, resize/1, xport/1,
         graph/1, lastupdate/1, ls/0, cd/1, mkdir/1, pwd/0
         ]).

-export([start_link/1, start_link/0]).
-export([start/0]).
-export([stop/0]).
-export([combine/1, c/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record( state2, { port, timeout }  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @equiv erlrrd_app:start()
start() -> erlrrd_app:start().
%% @equiv erlrrd_app:stop()
stop() -> erlrrd_app:stop().

%% @spec start_link(RRDToolCmd) -> Result
%%   RRDToolCmd = string()
%%   Result = {ok,Pid} | ignore | {error,Error}
%%     Pid = pid()
%%     Error = {already_started,Pid} | shutdown | term()
%% @doc calls gen_server:start_link
%%   RRDToolCmd is the command passed to open_port()
%%   usually "rrdtool -"
start_link(RRDToolCmd) when is_list(RRDToolCmd) ->
  application:set_env(erlrrd, rrdtoolcmd, RRDToolCmd ),
  start_link().

%% @equiv start_link("rrdtool -")
start_link() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec combine(List) -> List
%%   List = [ term() ]
%% @doc "joins" and quotes the given arg list. 
%%   takes a list of arguments, and returns a deeplist with 
%%   each argument surrounded by double quotes
%%   then separated by spaces. Note 
%%   it does not try to escape any double quotes
%%   in the arguments.
%%
%%   combine(["these", "are", "my args"]). ->
%%
%%   [["\"","these","\""]," ",["\"","are","\""]," ",["\"","my args","\""]]
%%
%%   it is intended as a convinence function to the 
%%   rrdtool commands which all take a single iodata() argument
%%   which represents the string to be passed as the arguments 
%%   to the corresponding rrdtool command.  
%%
%%   erlrrd:xport(erlrrd:c(["DEF:foo=/path with/space/foo.rrd:foo:AVERAGE", "XPORT:foo"])).
combine(Args) -> 
    join([ [ "\"", X, "\"" ] || X <- Args ], " ").
%% @spec c(List) -> List
%%   List = [ term() ]
% @equiv combine(Args)
c(Args) -> combine(Args).


% rrdtool commands

%% @spec create(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Set up a new Round Robin Database (RRD). Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdcreate.en.html rrdcreate].
create     (Args) when is_list(Args) -> do(create,     Args).

%% @spec update(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Store new data values into an RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdupdate.en.html rrdupdate].
update     (Args) when is_list(Args) -> do(update,     Args).

%% @spec updatev(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Operationally equivalent to update except for output. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdupdate.en.html rrdupdate].
updatev    (Args) when is_list(Args) -> do(updatev,    Args).

%% @spec dump(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc    Dump the contents of an RRD in plain ASCII. In connection with
%%         restore you can use this to move an RRD from one computer
%%         architecture to another.  Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrddump.en.html rrddump].
dump       (Args) when is_list(Args) -> do(dump,       Args).

%% @spec restore(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Restore an RRD in XML format to a binary RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdrestore.en.html rrdrestore]
restore    (Args) when is_list(Args) -> do(restore,    Args).

%% @spec last(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = integer()
%% @doc    Return the date of the last data sample in an RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdlast.en.html rrdlast]
last       (Args) when is_list(Args) -> 
  case do(last,       Args) of
    { error, Reason } -> { error, Reason };
    { ok, [[Response]] } -> { ok, erlang:list_to_integer(Response) }
  end.

%% @spec lastupdate(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc    Return the most recent update to an RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdlastupdate.en.html rrdlastupdate]
lastupdate (Args) when is_list(Args) -> do(lastupdate, Args).

%% @spec first(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = integer()
%% @doc Return the date of the first data sample in an RRA within an
%%       RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdfirst.en.html rrdfirst]
first       (Args) when is_list(Args) -> 
  case do(first,       Args) of
    { error, Reason } -> { error, Reason };
    { ok, [[Response]] } -> { ok, erlang:list_to_integer(Response) }
  end.

%% @spec info(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Get information about an RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdinfo.en.html rrdinfo].
info       (Args) when is_list(Args) -> do(info,       Args).

%% @spec fetch(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc   Get data for a certain time period from a RRD. The graph func-
%%         tion uses fetch to retrieve its data from an RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdfetch.en.html rrdfetch].
fetch      (Args) when is_list(Args) -> do(fetch,      Args).

%% @spec tune(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc Alter setup of an RRD. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdtune.en.html rrdtune].
tune       (Args) when is_list(Args) -> do(tune,       Args).

%% @spec resize(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc    Change the size of individual RRAs. This is dangerous! Check
%%         rrdresize.
resize     (Args) when is_list(Args) -> do(resize,     Args).

%% @spec xport(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc   Export data retrieved from one or several RRDs. Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdxport.en.html rrdxport]
%%
%%  erlrrd:xport("'DEF:foo=/path with/space/foo.rrd:foo:AVERAGE' XPORT:foo").
%%
%%  erlrrd:xport(erlrrd:c(["DEF:foo=/path with/space/foo.rrd:foo:AVERAGE", "XPORT:foo"])).
xport      (Args) when is_list(Args) -> do(xport,      Args).

%% @spec graph(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc   Create a graph from data stored in one or several RRDs. Apart
%%         from generating graphs, data can also be extracted to stdout.
%%         Check 
%% [http://oss.oetiker.ch/rrdtool/doc/rrdgraph.en.html rrdgraph].
graph      (Args) when is_list(Args) -> 
  % TODO: scan for this pattern w/out flattening the io_list? 
  % TODO: support  graphing to stdout!!! :) 
  Flat = erlang:binary_to_list(erlang:iolist_to_binary(Args)),
  case regexp:match(Flat, "(^| )-( |$)") of
    { match, _, _ } -> 
      % graph to stdout will break this Ports parsing of reponses..
      { error, "Graphing to stdout not supported." };
    nomatch -> 
      do(graph, Args)
  end.

%%%%% rrd "remote" commands %%%%

%% @spec cd(erlang:iodata()) -> ok |  
%%   { error, Reason } 
%%  Reason = iolist()
%% @doc   ask the rrdtool unix process to change directories
%% 
%%    erlrrd:cd("/usr/share/rrd/data").
%%
%%    erlrrd:cd(erlrrd:combine(["/Users/foo/Library/Application Support/myapp/rrd"]).
cd         (Arg)  when is_list(Arg) -> 
  case do(cd,         Arg) of
    { error, Reason } -> { error, Reason };
    { ok, _ } -> ok
  end.

%% @spec mkdir(erlang:iodata()) -> { ok, Response }  |  
%%   { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc   ask the rrdtool unix process to create a directory
mkdir      (Arg)  when is_list(Arg) -> do(mkdir,      Arg).

%% @spec ls() -> { ok, Response }  | { error, Reason } 
%%  Reason = iolist()
%%  Response = iolist()
%% @doc  lists all *.rrd files in rrdtool unix process'
%%       current working directory
ls         ()     -> do(ls,         []  ).

%% @spec pwd() -> { ok, Response }  |  { error, Reason } 
%%  Reason = iolist()
%%  Response = string()
%% @doc  return the rrdtool unix process'
%%       current working directory.
pwd        ()     -> 
  case do(pwd,       []) of
    { error, Reason } -> { error, Reason };
    { ok, [[Response]] } -> { ok, Response }
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen server interface poo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
init([]) -> 
  RRDToolCmd = case application:get_env(erlrrd, rrdtoolcmd) of
    { ok, Cmd } ->  Cmd;
    undefined -> "rrdtool -"
  end,
  Timeout = case application:get_env(erlrrd, timeout) of
    { ok, T } -> T;
    undefined -> 3000
  end,
  process_flag(trap_exit, true),
  Port = erlang:open_port(
    {spawn, RRDToolCmd},
    [ {line, 10000}, eof, exit_status, stream ] 
  ),
  {ok, #state2{port = Port, timeout = Timeout}}.

%% handle_call
%% @hidden
handle_call(
  {do, Action, Args }, 
  _From, 
  #state2{port = Port, timeout = Timeout } = State
  ) ->
    Line = [ erlang:atom_to_list(Action), " ", Args , "\n"],
    port_command(Port, Line),
    case collect_response(Port, Timeout) of
          {response, Response} -> 
              {reply, { ok, Response }, State};
          { error, timeout } ->
              {stop, port_timeout, State};
          { error, Error } -> 
              {reply, { error, Error  }, State}
    end.

%% handle_cast
%% @hidden
handle_cast(_Msg, State) -> 
  % io:format(user, "Got unexpected cast msg: ~p~n", [Msg]),
  %% TODO error/event loging in erlang style.
  {noreply, State}.

%% handle_info
%% @hidden
handle_info({ Port , {exit_status, Status}}, State) 
  when 
    Port =:= State#state2.port
  -> 
  { stop, { port_exit, Status }, State};

handle_info(_Msg, State) -> 
  % io:format(user, "Got unexpected info msg: ~p~n", [Msg]),
  %% TODO error/event loging in erlang style.
  {noreply, State}.

%% terminate
%% @hidden
terminate(_Reason, _State) -> ok.

%% code_change
%% @hidden
code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private poo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do(Command, Args) -> 
  case has_newline(Args) of
    true  -> { error, "No newlines" };
    false -> gen_server:call (?MODULE, { do, Command, Args } ) 
  end.

join([Head | [] ], _Sep) ->
  [Head];
join([Head | Tail], Sep) ->
  [ Head, Sep | join(Tail, Sep) ].

has_newline([]) -> false;
has_newline(<<>>) -> false;
has_newline([ H |  T]) 
  when is_list(H); is_binary(H) ->
    case has_newline(H) of
      true -> true;
      false -> has_newline(T)
    end;
has_newline([ H | T]) when is_integer(H) ->
  if 
    H =:= $\n -> true;
    true -> has_newline(T)
  end;
has_newline(<<H:8,T/binary>>) ->
  if 
    H =:= $\n -> true;
    true -> has_newline(T)
  end.


collect_response(Port, Timeout ) ->
    collect_response(Port, [], [], Timeout ).

collect_response( Port, RespAcc, LineAcc, Timeout) ->
    receive
        {Port, {data, {eol, "OK u:" ++ _T }}} ->
            {response, lists:reverse(RespAcc)};
        {Port, {data, {eol, "ERROR: " ++ Error }}} ->
            {error, [ Error, lists:reverse(RespAcc)]};
        {Port, {data, {eol, Result}}} ->
            Line = lists:reverse([Result | LineAcc]),
            collect_response(Port, [Line | RespAcc], [], Timeout);
        {Port, {data, {noeol, Result}}} ->
            collect_response(Port, RespAcc, [Result | LineAcc], Timeout)

    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after Timeout ->  
            { error, timeout }
    end.
-ifdef(EUNIT).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_start_stop_(StartFun, StopFun, Tag) -> 
  { spawn, 
    { inorder,
      [
        check_stopped_(Tag),
        { Tag, setup,
          StartFun,
          StopFun,
          check_started_(Tag) 
        },
        check_stopped_(Tag)
      ]
    }
  }.

check_started_(Tag) ->
  wrap_tag_(Tag,
    [
      ?_test(ls()),
      ?_test(pwd()),
      ?_test(ok),
      ?_test(ok)
    ] 
  ).

check_stopped_(Tag) ->
  wrap_tag_(Tag,
    [
      ?_assertExit( { noproc, _ }, pwd()),
      ?_assertExit( { noproc, _ }, ls())
    ]
  ).

check_last_(RRDFile,Now) -> 
  fun () -> 
    { ok, When } = erlrrd:last(RRDFile),
    When = Now
  end.

start_helper_() -> 
  application:unset_env(erlrrd, rrdtoolcmd),
  application:unset_env(erlrrd, timeout),
  {ok, Pid} = start_link(), 
  Pid.
stop_helper_(Pid) -> stop_helper_(Pid, 3000).
stop_helper_(Pid, Timeout) ->
  true = exit(Pid, normal),
  receive 
    {'EXIT', Pid, Reason} -> Reason
  after Timeout -> 
    throw({ timeout, { Pid, "Pid not responding to EXIT?"} })
  end.

% check if the dir we're in end's in /tests
check_cwd_helper_() ->
   { ok, L } = file:get_cwd(),  
   "stset/" ++ _  = lists:reverse(L).

p_func(X,Steps) -> 
  Pi = math:pi(),
  5 * ( 
    math:sin( 3*Pi/2 + X/(Steps / (8*Pi) )  )
    + 1
  ).

time_since_epoch() ->
  calendar:datetime_to_gregorian_seconds( erlang:universaltime())
    - ( 719528 * 86400 ).

wrap_tag_(T,L) when is_list(L) -> 
  [ { T, X } || X <- L ].

cast(Blah) -> 
 gen_server:cast(?MODULE, Blah ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% test starting and stopping %%%%

-define(spew(Args), io:format(user, "~p~n", [{Args}])).
-define(assertExists(File), 
      { ok, _ } = file:read_file_info(File)).
-define(assertEnoent(File), 
      { error, enoent } = file:read_file_info(File)).


start_link_test_() -> 
  test_start_stop_(
    fun start_helper_/0,
    fun stop_helper_/1,
    "start_link test" 
  ).

start_stop_test_() ->
  test_start_stop_(
    fun()  -> ok = start() end,
    fun(_) -> ok = stop()  end,
    "start/0 stop/0"
  ).

start_sup_test_() ->
  test_start_stop_(
    fun() -> 
      {ok,Pid} = erlrrd_sup:start_link(),
      Pid 
    end,
    fun stop_helper_/1,
    "sup:start_link/0 exit/2" 
  ).

start_sup2_test_() ->
  test_start_stop_(
    fun() -> 
      check_cwd_helper_(),
      {ok,Pid} = erlrrd_sup:start_link("./dummyrrdtool"),
      Pid 
    end,
    fun stop_helper_/1,
    "sup:start_link/1 exit/2" 
  ).

start_app_test_() -> 
  test_start_stop_(
    fun()  -> ok = erlrrd_app:start() end,
    fun(_) -> ok = erlrrd_app:stop()  end,
    "app:start/0 app:stop/0"
  ).

%%%% test interfaces %%%%

datain_dataout_test_() ->
  Prefix = "foo",
  RRDFile = Prefix ++ ".rrd",
  PNGFile = Prefix ++ ".png",
  RRDDump = Prefix ++ ".rrd.xml",
  RRDRestoredFile = Prefix ++ ".2.rrd",
  Now = time_since_epoch(),
  Then = Now - 86400,
  StepSize = 60,
  Steps = round((Now - Then) / StepSize),
  { setup,
    fun()  -> 
      check_cwd_helper_(),
      file:delete(RRDFile),
      file:delete(RRDDump),
      file:delete(RRDRestoredFile),
      ?assertEnoent(RRDFile),
      { ok, Pid } = start_link(),
      Pid
    end,
    fun(Pid) -> 
      stop_helper_(Pid),
      file:delete(RRDFile),
      file:delete(RRDDump),
      file:delete(RRDRestoredFile),
      ok 
    end,
    { inorder,
      [
        % create an rrd
        fun() ->
          {ok, _ } = erlrrd:create([
            io_lib:fwrite("~s --start ~B", [RRDFile, Then]),
            io_lib:fwrite(" --step ~B DS:thedata:GAUGE:~B:U:U",
              [ StepSize, StepSize ]),
            io_lib:fwrite(" RRA:AVERAGE:0.5:1:~B",[Steps])
          ])
        end,

        % write sin wave to rrd
        fun() -> 
          lists:foreach(
            fun(X) ->
              P = p_func(X,Steps),
              %io:format(user, "~s ~B:~f~n", [ RRDFile, Then + X * StepSize, P ]),
              {ok, _ } = erlrrd:update(
                io_lib:format("~s ~B:~f", [ RRDFile, Then + X * StepSize, P ])
              )
            end,
            lists:seq(1, Steps)
          )
        end,

        % check the update times
        check_last_(RRDFile, Now),
        fun() -> 
          { ok, When } = erlrrd:first(RRDFile),
          RoundThen = (erlang:trunc(Then/StepSize) + 1) * StepSize,
          When = RoundThen
        end,
        ?_test(?assertMatch({error, _}, erlrrd:last("/somenothing/file"))),
        ?_test(?assertMatch({error, _}, erlrrd:first("/somenothing/file"))),

        % make a graph!! :)
        fun() -> 
          { ok, _ } = erlrrd:graph([
            "-l 0 -r", " ",
            "-w 700 -h 200 -a PNG ", PNGFile,
            " DEF:thedata=", RRDFile, ":thedata:AVERAGE AREA:thedata#CC9945",
            io_lib:format(" --start ~B --end ~B", [ Then, Now ])
          ])
          % ok, now how can we check the graph???  hmm.
        end,

        % run fetch
        fun() -> 
          { ok, _Data } = erlrrd:fetch([
            RRDFile, " AVERAGE ",
            io_lib:format(" --start ~B --end ~B", [ Then, Now ])
          ]),
          %?spew(Data),
          %TODO check data
          ok
        end,

        % dump the rrd
        fun() -> 
          ?assertEnoent(RRDDump),
          { ok, _ } = erlrrd:dump( RRDFile ++ " " ++ RRDDump ),
          ?assertExists(RRDDump)
        end,

        % restore the rrd
        fun() -> 
          ?assertExists(RRDDump),
          ?assertEnoent(RRDRestoredFile),
          { ok, _ } = erlrrd:restore( RRDDump ++ " " ++ RRDRestoredFile ),
          ?assertExists(RRDRestoredFile)
        end,
        check_last_(RRDRestoredFile, Now),

        % xport
        { "xport",
          fun() -> 
            ?assertExists(RRDRestoredFile),
            {ok, Response} = xport([
              "DEF:c=", RRDRestoredFile, ":thedata:AVERAGE",
              " XPORT:c",
              io_lib:format(" --start ~B --end ~B", [ Then, Now ])
            ]),
            %% TODO check response
            Response
          end
        },
        
        %lastupdate
        fun() -> 
          { ok, [ _, _, [Last]] } = lastupdate(RRDRestoredFile),
          ?assertMatch(
            {match, _, _ },
            regexp:match(Last, 
              % TODO make the regex match beginning of line? why no work? 
              io_lib:format("~B", [ Now ])
            )
          ),
          ok
        end,

        % info 
        fun() -> 
          { ok, Info } = info(RRDFile),
          [ ["filename = " ++ _L] | _T ] = Info
        end,
          
        fun() -> commas_are_cool end
      ]
    }
  }.

remote_cmds_test_() ->
  Dir = "makadir",
  { setup,
    fun() -> 
      check_cwd_helper_(),
      ?assertEnoent(Dir),
      start_helper_()
    end,
    fun(P) -> 
      file:del_dir(Dir),
      stop_helper_(P)
    end,
    { inorder, 
      [ 
        % pwd
        { "pwd1", fun() -> 
          { ok, Cwd } = erlrrd:pwd(),
          "stset/" ++ _  = lists:reverse(Cwd)
        end},
        { "mkdir", fun() -> erlrrd:mkdir(Dir) end },
        { "cd ", fun() -> ok = erlrrd:cd(Dir) end},
        % pwd 
        { "pwd2", 
          fun() -> 
            { ok, Cwd } = erlrrd:pwd(),
            { match, _, _ }  = 
              regexp:match(Cwd, "/tests/" ++ Dir ++ "$")
          end 
        },
        { "ls", 
          fun() -> 
            % relys on '.' and '..' always returning first? 
            % is that a bad idea? 
            { ok, List } = ls(),
            io:format(user," ~p~n", [List]),
            ?assert( lists:any(fun(X) -> X =:= ["d .."] end, List))
          end },
        fun() -> commas_are_cool end
      ]
    }
  }.

c_test_() -> 
  [
    ?_test(
      [ 
        ["\"", "these", "\""], " ",
        ["\"", "are",   "\""], " ",
        ["\"", "my",    "\""], " ",
        ["\"", "args",  "\""]  
      ] = c(["these", "are", "my", "args"])),
    ?_test([[ "\"", "a", "\""]] = c(["a"]))
  ].

pass_newlines_test_() ->
  M = "No newlines",
  { setup,
    fun start_helper_/0,
    fun stop_helper_/1,
    [ 
      ?_assertMatch( { error, M }, cd("..\n")),
      ?_assertMatch( { error, M }, info("fart.rrd\n")),
      ?_assertMatch( { error, M }, 
        create(["foo.rrd bar baz", [[[[<<"blahdedah\n">>]]]], "haha"])
      ),
      fun() -> ok end
    ]
  }.

graph_to_stdout_saftey_test_() -> 
  M = { error, "Graphing to stdout not supported." },
  [
    ?_assertMatch( M, graph(" -")),
    ?_assertMatch( M, graph("-")),
    ?_assertMatch( M, graph(" - ")),
    ?_assertMatch( M, graph([" ", "-"," "])),
    ?_assertMatch( M, graph([" ", [[["-"]]]," "])),
    ?_assertMatch( M, graph([" ", [[["-"]]]])),
    ?_assertMatch( M, graph([[[["-"]]]," "])),
    ?_assertMatch( M, graph([" ", [[[<<"-">>]]]," "])),
    ?_assertMatch( M, graph([" ", [[[<<"-">>]]]])),
    ?_assertMatch( M, graph([[[[<<"-">>]]]," "])),
    fun() -> ok end
  ].

%%%% tests for corner cases %%%%

cause_long_response_test_() -> 
  { setup,
    fun()  -> 
      check_cwd_helper_(),
      { ok, Pid } = start_link("./dummyrrdtool -"),
      Pid
    end,
    fun stop_helper_/1,
    ?_test(do(longresponse, []))
  }.

cause_timeout_test_() -> 
  { setup,
    fun()  -> 
      check_cwd_helper_(),
      ok = application:set_env(erlrrd, timeout, 1),
      { ok, Pid } = erlrrd_sup:start_link("./dummyrrdtool -"),
      Pid
    end,
    fun stop_helper_/1,
    ?_assertExit({port_timeout, _}, do(timeout, []))
  }.

%%%% tests of privates %%%%

join_test() -> 
  [ "a", " ", "b", " ", "c"] = join(["a", "b", "c"], " ").
join_test_() ->
  [ 
    ?_test([ "a", " ", "b", " ", "c"] = join(["a", "b", "c"], " ")),
    ?_assertNot([ "a", "b", " ", "c"]  =:= join(["a", "b", "c"], " "))
  ].

has_newline_test_() ->
  [ 
    ?_test( true  = has_newline("\n")),
    ?_test( true  = has_newline(["\n"])),
    ?_test( true  = has_newline(
      ["these", ["are", [ "my args" ] | <<"newline\n">> ], "so", "there"])),
    ?_test( false = has_newline(
      ["these", ["are", [ "my args" ] | <<"newline">> ], "so", "there"])),
    ?_test( true  = has_newline(
      ["these\n", ["are", [ "my args" ] | <<"newline">> ], "so", "there"])),
    ?_test( true  = has_newline(
      ["these", ["are", 
      [ "my args" | [[[[[[[[ "blah\n"]]]]]]]] ] | <<"newline">> ], 
      "so", "there"])),
    ?_test( false = has_newline("")),
    ?_test( false = has_newline([])),
    ?_test( false = has_newline(<<>>))
  ].

%%%%% tests to get coverage %%%%%

should_be_done_better_test_() -> 
  { setup,
    fun start_helper_/0,
    fun stop_helper_/1,
    [
       ?_assertMatch( {error, _ }, tune("blah") ),
       ?_assertMatch( {error, _ }, resize("blah") ),
       ?_assertMatch( {error, _ }, updatev("blah") ),
       ?_test(ok)
    ]
  }.

handle_cast_test_() ->
  { setup,
    fun start_helper_/0,
    fun stop_helper_/1,
    ?_test(cast(blah))
  }.

handle_info_test_() -> 
  { setup,
    fun start_helper_/0,
    fun stop_helper_/1,
    ?_test(?MODULE ! yo)
  }.

stop_helper_test_() -> 
  { setup, 
    % start fun
    fun() -> 
      F = fun(F) -> 
        receive
          Any -> io:format(user,"~p Hey, got: ~p~n", [ self(), Any ])
        end,
        F(F)
      end,
      spawn(
        fun() -> 
          process_flag(trap_exit, true),
          F(F)
        end
      )
    end,
    % stop fun,
    fun(P) -> exit(P, kill) end,
    % test gen
    fun(P) -> 
      ?_assertThrow({timeout,_}, stop_helper_(P, 1))
    end
  }.

port_exit_test_() -> 
  ?_assert(
    begin
      io:format(user, "~n==== test: expect erlrrd exit~n", []),
      {ok,Pid} = start_link("./dummyrrdtool"),
      {ok, _} = do(die, []),
      receive 
        { 'EXIT', Pid, {port_exit, 1} } -> true
      after 4000 -> 
        exit(Pid,kill),
        false
      end
    end
  ).

code_change_test() -> 
  { ok, state } = code_change( oldvsn, state, extra ).

-endif.
