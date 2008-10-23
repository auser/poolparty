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
%% $Id$ 
%%
%% @private (for now)
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @doc Erlang file monitoring service

%% The behaviour of this service is inspired by the open source FAM
%% daemon [http://oss.sgi.com/projects/fam/].

-module(file_monitor).

-export([start/0, start/1, start/2, stop/0, stop/1, monitor_file/2,
	 monitor_file/3, monitor_dir/2, monitor_dir/3, demonitor/1,
	 demonitor/2]).

-export([main/1]).    %% private

-include_lib("kernel/include/file.hrl").

-define(POLL_TIME, 5000). % default; change with option poll_time
-define(SERVER, file_monitor).

%% NOTE: paths should be absolute, but this is not checked

%% We don't change the paths, e.g. from relative to absolute, but we
%% make sure that the path is a flat string and return it to the caller.

monitor_file(Path, Pid) ->
    monitor_file(?SERVER, Path, Pid).

monitor_file(Server, Path, Pid) ->
    monitor(Server, file, filename:flatten(Path), Pid).

monitor_dir(Path, Pid) ->
    monitor_dir(?SERVER, Path, Pid).

monitor_dir(Server, Path, Pid) ->
    monitor(Server, dir, filename:flatten(Path), Pid).

monitor(Server, Type, Path, Pid) when is_pid(Pid) ->
    {ok, Ref} = command(Server, {monitor, {Type, Path}, Pid}),
    {ok, Path, Ref}.

demonitor(Ref) ->
    demonitor(?SERVER, Ref).

demonitor(Server, Ref) ->
    ok = command(Server, {demonitor, Ref}).


command(Server, Cmd) ->
    ServerPid = ensure_started(Server),
    ServerPid ! {command, self(), Cmd},
    receive
	{ServerPid, Result} -> Result
    end.


stop() ->
    stop(?SERVER).

stop(Server) ->
    Server ! stop,
    ok.
	    
ensure_started(Name) when is_atom(Name) ->
    start(Name, []);
ensure_started(Pid) when is_pid(Pid) ->
    Pid.

start() ->
    start([]).

start(Options) ->
    start(?SERVER, Options).

start(Name, Options) ->
    case whereis(Name) of
	undefined ->
	    Parent = self(),
	    Pid = spawn(fun () -> server_init(Name, Parent, Options) end),
	    receive
		{Pid, ok} -> Pid;
		{Pid, error} -> throw(no_server)
	    end;
	Pid -> Pid
    end.

-record(state, {name, time, dirs, files, clients, refs}).

server_init(undefined = Name, Parent, Options) ->
    %% anonymous server
    server_init_1(Name, Parent, Options);
server_init(Name, Parent, Options) ->
    case catch register(Name, self()) of
	true ->
	    server_init_1(Name, Parent, Options);
	_ ->
	    Parent ! {self(), error},
	    exit(failed)
    end.

server_init_1(Name, Parent, Options) ->
    Parent ! {self(), ok},
    server(set_timer(init_state(Name, Options))).

init_state(Name, Options) ->
    Time = case proplists:get_value(poll_time, Options) of
	       N when is_integer(N), N >= 100 -> N;
	       _ -> ?POLL_TIME
	   end,
    #state{name = Name,
	   time = Time,
	   dirs = dict:new(),
	   files = dict:new(),
	   clients = dict:new(),
	   refs = dict:new()}.

server(St) -> ?MODULE:main(St).

%% @private
main(St) ->
    receive
	{command, From, {monitor, Object, Pid}} when is_pid(Pid) ->
	    {Ref, St1} = new_monitor(Object, Pid, St),
	    server_reply(From, {ok, Ref}),
	    server(add_client(Pid, St1));
	{command, From, {demonitor, Ref}} ->
	    server_reply(From, ok),
	    server(delete_monitor(Ref, St));
	stop ->
	    exit(normal);
	time ->
	    server(set_timer(poll(St)));
	{'DOWN', _Ref, process, Pid, _Info} ->
	    server(purge_pid(Pid, del_client(Pid, St)));
	_ ->
	    server(St)
    end.

server_reply(To, Msg) ->
    To ! {self(), Msg}.

set_timer(St) ->
    erlang:send_after(St#state.time, self(), time),
    St.


%% client monitoring (once a client, always a client - until death)

add_client(Pid, St) ->
    case dict:is_key(Pid, St#state.clients) of
	true ->
	    St;
	false ->
	    Ref = erlang:monitor(process, Pid),
	    St#state{clients = dict:store(Pid, Ref, St#state.clients)}
    end.

del_client(Pid, St) ->
    case dict:find(Pid, St#state.clients) of
	{ok, Ref} ->
	    erlang:demonitor(Ref, [flush]),
	    St#state{clients = dict:erase(Pid, St#state.clients)};
	error ->
	    St
    end.


-record(monitor, {pid, reference}).

-record(entry, {info = undefined, files = [], monitors = sets:new()}).

new_monitor(Object, Pid, St) ->
    Ref = make_ref(),
    Monitor = #monitor{pid = Pid, reference = Ref},
    new_monitor(Object, Monitor, Ref,
		St#state{refs = dict:store(Ref, {Pid, Object},
					   St#state.refs)}).

%% We must separate the namespaces for files and dirs, since we can't
%% trust the users to keep them distinct; there may be simultaneous file
%% and dir monitors for the same path.

new_monitor({file, Path}, Monitor, Ref, St) ->
    {Ref, St#state{files = add_monitor(Path, Monitor, file,
				       St#state.files)}};
new_monitor({dir, Path}, Monitor, Ref, St) ->
    {Ref, St#state{dirs = add_monitor(Path, Monitor, dir,
				      St#state.dirs)}}.

%% Adding a new monitor forces an immediate poll of the file, such that
%% previous monitors only see any real change, while the new monitor
%% either gets {exists, ...} or {error, ...}.

add_monitor(Path, Monitor, Type, Dict) ->
    Entry = case dict:find(Path, Dict) of
		{ok, OldEntry} -> poll_file(Path, OldEntry, Type);
		error -> new_entry(Path, Type)
	    end,
    event(#entry{}, dummy_entry(Entry, Monitor), Type, Path),
    NewEntry = Entry#entry{monitors =
			   sets:add_element(Monitor,
					    Entry#entry.monitors)},
    dict:store(Path, NewEntry, Dict).

dummy_entry(Entry, Monitor) ->
    Entry#entry{monitors = sets:add_element(Monitor, sets:new())}.

new_entry(Path, Type) ->
    refresh_entry(Path, #entry{monitors = sets:new()}, Type).

%% deleting a monitor by reference

delete_monitor(Ref, St) ->
    case dict:find(Ref, St#state.refs) of
	{ok, {_Pid, Object}} ->
	    St1 = St#state{refs = dict:erase(Ref, St#state.refs)},
	    delete_monitor_1(Ref, Object, St1);
	error ->
	    St
    end.

delete_monitor_1(Ref, {file, Path}, St) ->
    St#state{files = delete_monitor_2(Path, Ref, St#state.files)};
delete_monitor_1(Ref, {dir, Path}, St) ->
    St#state{dirs = delete_monitor_2(Path, Ref, St#state.dirs)}.

delete_monitor_2(Path, Ref, Dict) ->
    case dict:find(Path, Dict) of
	{ok, Entry} -> 
	    purge_empty_sets(
	      dict:store(Path, purge_monitor_ref(Ref, Entry), Dict));
	error ->
	    Dict
    end.

purge_monitor_ref(Ref, Entry) ->
    Entry#entry{monitors =
		sets:filter(fun (#monitor{reference = R})
				when R == Ref -> false;
				(_) -> true
			    end,
			    Entry#entry.monitors)}.

%% purging monitors belonging to dead clients

purge_pid(Pid, St) ->
    Files = dict:map(fun (_Path, Entry) ->
			     purge_monitor_pid(Pid, Entry)
		     end,
		     St#state.files),
    Dirs = dict:map(fun (_Path, Entry) ->
			    purge_monitor_pid(Pid, Entry)
		    end,
		    St#state.dirs),
    Refs = dict:filter(fun (_Ref, {P, _})
			   when P == Pid -> false;
			   (_, _) -> true
		       end,
		       St#state.refs),
    St#state{refs = Refs,
	     files = purge_empty_sets(Files),
	     dirs = purge_empty_sets(Dirs)}.

purge_monitor_pid(Pid, Entry) ->
    Entry#entry{monitors =
		sets:filter(fun (#monitor{pid = P})
				when P == Pid -> false;
				(_) -> true
			    end,
			    Entry#entry.monitors)}.

purge_empty_sets(Dict) ->
    dict:filter(fun (_Path, Entry) ->
			sets:size(Entry#entry.monitors) > 0
		end, Dict).


%% generating events upon state changes

%% Message formats:
%%   {exists, Path, Type, #file_info{}, Files}
%%   {changed, Path, Type, #file_info{}, Files}
%%   {error, Path, Type, Info}
%%
%% Type is dir or file. If Type is file, Files is always []. If Type is
%% dir, Files is a list of {added, FileName} and {deleted, FileName},
%% where FileName is relative to dir, without any path component.
%%
%% When a new monitor is installed for a path, an initial {exists,...}
%% or {error,...} message will be sent to the monitor owner.
%%
%% Subsequent events will be either {changed,...} or {error,...}.

event(#entry{info = Info}, #entry{info = Info}, _Type, _Path) ->
    %% no change in state
    ok;
event(#entry{info = undefined}, #entry{info = NewInfo}=Entry,
      Type, Path)
  when not is_atom(NewInfo) ->
    %% file or directory exists, for a fresh monitor
    Files = [{added, F} || F <- Entry#entry.files],
    cast({exists, Path, Type, NewInfo, Files}, Entry#entry.monitors);
event(_OldEntry, #entry{info = NewInfo}=Entry, Type, Path)
  when is_atom(NewInfo) ->
    %% file is not available
    cast({error, Path, Type, NewInfo}, Entry#entry.monitors);
event(_OldEntry, Entry, file, Path) ->
    %% a normal file has changed
    cast({changed, Path, file, Entry#entry.info, []},
	 Entry#entry.monitors);
event(#entry{info = OldInfo}, #entry{info = NewInfo}=Entry, dir, Path)
  when is_atom(OldInfo) ->
    %% a directory has suddenly become available
    Files = [{added, F} || F <- Entry#entry.files],
    cast({changed, Path, dir, NewInfo, Files}, Entry#entry.monitors);
event(OldEntry, #entry{info = NewInfo}=Entry, dir, Path) ->
    %% a directory has changed
    Files = diff_lists(Entry#entry.files, OldEntry#entry.files),
    cast({changed, Path, dir, NewInfo, Files}, Entry#entry.monitors).


poll(St) ->
    St#state{files = dict:map(fun (Path, Entry) ->
				      poll_file(Path, Entry, file)
			      end,
			      St#state.files),
	     dirs = dict:map(fun (Path, Entry) ->
				     poll_file(Path, Entry, dir)
			     end,
			     St#state.dirs)}.

poll_file(Path, Entry, Type) ->
    NewEntry = refresh_entry(Path, Entry, Type),
    event(Entry, NewEntry, Type, Path),
    NewEntry.

refresh_entry(Path, Entry, Type) ->
    Info = get_file_info(Path),
    Files = case Type of
		dir when not is_atom(Info) -> get_dir_files(Path);	    
		_ -> []
	    end,
    Entry#entry{info = Info, files = Files}.


%% We clear some fields of the file_info so that we only trigger on real
%% changes; see the //kernel/file.erl manual and file.hrl for details.

get_file_info(Path) ->
    case file:read_file_info(Path) of
	{ok, Info} ->
	    Info#file_info{access = undefined,
			   atime  = undefined};
	{error, Error} ->
	    Error  % posix error code as atom
    end.

%% Listing the members of a directory; note that it yields the empty
%% list if it fails - this is not the place for error detection.

get_dir_files(Path) ->
    case file:list_dir(Path) of
	{ok, Files} -> lists:sort(Files);
	{error, _} -> []
    end.

%% both lists must be sorted for this diff to work

diff_lists([F1 | Fs1], [F2 | _]=Fs2) when F1 < F2 ->
    [{added, F1} | diff_lists(Fs1, Fs2)];
diff_lists([F1 | _]=Fs1, [F2 | Fs2]) when F1 > F2 ->
    [{deleted, F2} | diff_lists(Fs1, Fs2)];
diff_lists([_ | Fs1], [_ | Fs2]) ->
    diff_lists(Fs1, Fs2);
diff_lists([F | Fs1], Fs2) ->
    [{added, F} | diff_lists(Fs1, Fs2)];
diff_lists(Fs1, [F | Fs2]) ->
    [{deleted, F} | diff_lists(Fs1, Fs2)];
diff_lists([], []) ->
    [].


%% Multicasting events to clients

cast(Msg, Monitors) ->
    sets:fold(fun (#monitor{pid = Pid, reference = Ref}, Msg) ->
		      %%erlang:display({file_monitor, Ref, Msg}),
		      Pid ! {file_monitor, Ref, Msg},
		      Msg  % note that this is a fold, not a map
	      end,
	      Msg, Monitors).
