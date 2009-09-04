%%%
%%% Make .rel and boot scripts 
%%%
%%% Usage:
%%% cd ebin; erl -pa . -noshell -run make_boot write_scripts app version [app version]... 
%%% 
%%% e.g.:
%%% cd ebin; erl -pa . -noshell -run make_boot write_scripts hermes "0.0.2" stoplight "0.0.1"
%%%
-module(make_boot).
-export([write_scripts/1, write_release_scripts/1]).

write_scripts(A) ->
    write_scripts(A, local).

write_release_scripts(A) ->
    write_scripts(A, release).

write_scripts(A, Dest) -> 
  Args = pair_up(A),
  [Primary|Others] = Args,
  {Name, Version} = Primary,

  io:format("write_scripts for ~p~n", [Name]),
  Erts = erlang:system_info(version),
  application:load(sasl),

  {value, {kernel, _, Kernel}} = lists:keysearch(kernel, 1,
          application:loaded_applications()),
  {value, {stdlib, _, Stdlib}} = lists:keysearch(stdlib, 1,
          application:loaded_applications()),
  {value, {sasl, _, Sasl}} = lists:keysearch(sasl, 1,
          application:loaded_applications()),

  Rel = "{release, {\"~s\", \"~s\"}, {erts, \"~s\"}, ["
        "{kernel, \"~s\"}, {stdlib, \"~s\"}, {sasl, \"~s\"}, {~s, \"~s\"}~s]}.",
 
  OtherApps = lists:foldl(fun(Elem, AccIn) ->
              {N1, V1} = Elem, 
              AccIn ++ io_lib:format(", {~p, ~p}", [list_to_atom(N1), V1])
      end, "", Others),

  Lowername        = string:to_lower(Name),
  LowernameVersion = string:to_lower(Name ++ "-" ++ Version),

  Filename = lists:flatten(LowernameVersion ++ ".rel"),
  io:format("Writing to ~p (as ~s)~n", [Filename, Lowername]),
  {ok, Fs} = file:open(Filename, [write]),

  io:format(Fs, Rel, [Name, Version, Erts, Kernel, Stdlib, Sasl, Lowername, Version, OtherApps]),
  file:close(Fs),

  case Dest of
      local   ->  systools:make_script(LowernameVersion, [local]);
      release -> 
          systools:make_script(LowernameVersion, [{path, ["deps/*/ebin"]}]),
          systools:make_tar(LowernameVersion)
  end,
  halt().

pair_up([A, B | Tail]) ->
    [{A,B} | pair_up(Tail)];
pair_up([]) ->
    [].