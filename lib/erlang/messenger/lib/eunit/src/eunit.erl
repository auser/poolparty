%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id: eunit.erl 249 2008-05-11 20:06:45Z rcarlsson $
%%
%% @copyright 2004-2007 Mickaël Rémond, Richard Carlsson
%% @author Mickaël Rémond <mickael.remond@process-one.net>
%%   [http://www.process-one.net/]
%% @author Richard Carlsson <richardc@it.uu.se>
%%   [http://user.it.uu.se/~richardc/]
%% @version {@version}, {@date} {@time}
%% @doc This module is the normal EUnit user interface.

-module(eunit).

-include("eunit.hrl").
-include("eunit_internal.hrl").


-export([start/0, start/1, stop/0, stop/1, test/1, test/2, test/3,
	 list/1, submit/1, submit/2, submit/3, watch/1, watch/2,
	 watch/3, watch_path/1, watch_path/2, watch_path/3,
	 watch_regexp/1, watch_regexp/2, watch_regexp/3, watch_app/1,
	 watch_app/2, watch_app/3]).

-export([testp/1]). %% for development testing, not official


%% EUnit entry points

start() ->
    start(?SERVER).

start(Server) ->
    eunit_server:start(Server).

stop() ->
    stop(?SERVER).

stop(Server) ->
    eunit_server:stop(Server).

watch(Target) ->
    watch(Target, []).

watch(Target, Options) ->
    watch(?SERVER, Target, Options).

watch(Server, Target, Options) ->
    eunit_server:watch(Server, Target, Options).

watch_path(Target) ->
    watch_path(Target, []).

watch_path(Target, Options) ->
    watch_path(?SERVER, Target, Options).

watch_path(Server, Target, Options) ->
    eunit_server:watch_path(Server, Target, Options).

watch_regexp(Target) ->
    watch_regexp(Target, []).

watch_regexp(Target, Options) ->
    watch_regexp(?SERVER, Target, Options).

watch_regexp(Server, Target, Options) ->
    eunit_server:watch_regexp(Server, Target, Options).

watch_app(Name) ->
    watch_app(Name, []).

watch_app(Name, Options) ->
    watch_app(?SERVER, Name, Options).

watch_app(Server, Name, Options) ->
    case code:lib_dir(Name) of
	Path when is_list(Path) ->
	    watch_path(Server, filename:join(Path, "ebin"), Options);
	_ ->
	    error
    end.

list(T) ->
    try eunit_data:list(T)
    catch
	{error, R} -> {error, R}
    end.

test(T) ->
    test(T, [{order, inorder}]).

testp(T) ->
    test(T, [{order, inparallel}]).

test(T, Options) ->
    test(?SERVER, T, Options).

test(Server, T, Options) ->
    %% TODO: try to eliminate call to list/1
    try eunit_data:list(T) of
	List ->
	    Listeners = [eunit_tty:start(List, Options)
			 | listeners(Options)],
	    Serial = eunit_serial:start(Listeners),
	    case eunit_server:start_test(Server, Serial, T, Options) of
		{ok, Reference} -> test_run(Reference, Listeners);
		{error, R} -> {error, R}
	    end
    catch
	{error, R} ->
	    io:put_chars(eunit_lib:format_error(R)),
	    {error, R}
    end.

test_run(Reference, Listeners) ->
    receive
	{start, Reference} ->
	    cast(Listeners, {start, Reference})
    end,
    receive
	{done, Reference} ->
	    cast(Listeners, {stop, Reference, self()}),
	    receive 
		{result, Reference, Result} ->
		    Result
	    end
    end.

cast([P | Ps], Msg) ->
    P ! Msg,
    cast(Ps, Msg);
cast([], Msg) ->
    Msg.

%% TODO: functions that run tests on a given node, not a given server
%% TODO: maybe some functions could check for a globally registered server?
%% TODO: some synchronous but completely quiet interface function

submit(T) ->
    submit(T, []).

submit(T, Options) ->
    submit(?SERVER, T, Options).

submit(Server, T, Options) ->
    Dummy = spawn(fun devnull/0),
    eunit_server:start_test(Server, Dummy, T, Options).

listeners(Options) ->
    case proplists:get_value(event_log, Options) of
	undefined ->
	    [];
	LogFile ->
	    [spawn(fun () -> event_logger(LogFile) end)]
    end.

%% TODO: make this report file errors
event_logger(LogFile) ->
    case file:open(LogFile, [write]) of
	{ok, FD} ->
	    receive
		{start, Reference} ->
		    event_logger_loop(Reference, FD)
	    end;
	Error ->
	    exit(Error)
    end.

event_logger_loop(Reference, FD) ->
    receive
	{status, _Id, _Info}=Msg ->
	    io:fwrite(FD, "~w.\n", [Msg]),
	    event_logger_loop(Reference, FD);
	{stop, Reference, _ReplyTo} ->
	    %% no need to reply, just exit
	    file:close(FD),
	    exit(normal)
    end.

%% TODO: make a proper logger for asynchronous execution with submit/3

devnull() ->
    receive _ -> devnull() end.
