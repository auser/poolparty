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
%% $Id: code_monitor.erl 238 2007-11-15 10:23:54Z mremond $ 
%%
%% @private (for now)
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @doc Erlang code monitoring service

-module(code_monitor).

-export([start/0, start/1, stop/0, stop/1, monitor/1, monitor/2,
	 demonitor/1, demonitor/2, install_codespy/1, wiretap/3]).

-export([main/1]).    %% private


-define(SERVER, code_monitor).

monitor(Pid) ->
    monitor(?SERVER, Pid).

monitor(Server, Pid) when is_pid(Pid) ->
    ensure_started(Server),
    Server ! {monitor, Pid},
    ok.

demonitor(Pid) ->
    demonitor(?SERVER, Pid).

demonitor(Server, Pid) when is_pid(Pid) ->
    ensure_started(Server),
    Server ! {demonitor, Pid},
    ok.

stop() ->
    stop(?SERVER).

stop(Server) ->
    Server ! stop,
    ok.
	    
ensure_started(Name) when is_atom(Name) ->
    start(Name);
ensure_started(Pid) when is_pid(Pid) ->
    Pid.

start() ->
    start(?SERVER).

start(Name) ->
    case whereis(Name) of
	undefined ->
	    Parent = self(),
	    Pid = spawn(fun () -> server_init(Name, Parent) end),
	    receive
		{Pid, ok} -> Pid;
		{Pid, error} -> throw(no_server)
	    end;
	Pid -> Pid
    end.

server_init(undefined = Name, Parent) ->
    %% anonymous server
    server_init_1(Name, Parent);
server_init(Name, Parent) ->
    case catch register(Name, self()) of
	true ->
	    server_init_1(Name, Parent);
	_ ->
	    init_failure(Parent)
    end.

server_init_1(Name, Parent) ->
    case install_codespy(self()) of
	{ok, _Spy} ->
	    Parent ! {self(), ok},
	    server(Name, sets:new());
	{error, _} ->
	    init_failure(Parent)
    end.

init_failure(Parent) ->
    Parent ! {self(), error},
    exit(failed).

server(Name, Listeners) ->
    ?MODULE:main({Name, Listeners}).

%% @private
main({Name, Listeners}) ->
    receive
	{code_server, {module, M}} ->
	    cast({loaded, M, erlang:now()}, Listeners),
	    server(Name, Listeners);
	{monitor, Pid} when is_pid(Pid) ->
	    server(Name, sets:add_element(Pid, Listeners));
	{demonitor, Pid} ->
	    server(Name, sets:del_element(Pid, Listeners));
	stop ->
	    exit(normal);
	_ ->
	    server(Name, Listeners)
    end.

cast(M, Listeners) ->
    sets:fold(fun (L, M) -> L ! M end, {code_monitor, M}, Listeners).


%% code server spy process using generic wiretap functionality

install_codespy(To) ->
    wiretap(code_server, To, fun code_spy/3).

code_spy({code_call,From,{load_file,_}=Req}, Server, To) ->
    handle_load(Req, From, Req, Server, To);
code_spy({code_call,From,{ensure_loaded,M}=Req}=Msg, Server, To) ->
    case erlang:module_loaded(M) of
	true -> Server ! Msg;
	false -> handle_load(Req, From, Req, Server, To)
    end;
code_spy({code_call,From,{load_abs,_,_}=Req}, Server, To) ->
    handle_load(Req, From, Req, Server, To);
code_spy({code_call,From,{load_binary,_,_,_}=Req}, Server, To) ->
    handle_load(Req, From, Req, Server, To);
code_spy({code_call,From,{load_native_partial,_,_}=Req}, Server, To) ->
    handle_load(Req, From, Req, Server, To);
code_spy({code_call,From,{load_native_sticky,_,_,_}=Req}, Server, To) ->
    handle_load(Req, From, Req, Server, To);
code_spy(Msg, Server, _To) ->
    Server ! Msg.

handle_load(Req, From, Req, Server, To) ->
    ReplyTo = spawn(fun () -> reply_handler(Server, From, To) end),
    Server ! {code_call, ReplyTo, Req}.

%% one-shot processes - receive, pass on and die
reply_handler(Server, Client, To) ->
    link(Server),
    receive
	{code_server, _Reply} = M ->
	    To ! Client ! M
    end.

%% basic wiretapping of registered processes (it should be possible to
%% have several wiretaps attached to the same registered name; they will
%% transparently form a chain, without knowing about each other)

wiretap(Name, To, F) when is_atom(Name), is_pid(To), is_function(F) ->
    Parent = self(),
    Pid = spawn(fun () -> wiretap_init(Name, To, F, Parent) end),
    receive
	{Pid, Result} -> Result
    end.

wiretap_init(Name, To, F, Parent) ->
    case whereis(Name) of
	undefined ->
	    Parent ! {self(), {error, undefined}},
	    exit(error);
	Pid ->
	    catch unregister(Name),
	    catch register(Name, self()),
	    Self = self(),
	    case whereis(Name) of
		Self ->
		    process_flag(trap_exit, true),
		    link(Pid),
		    link(To),
		    Parent ! {self(), {ok, self()}},
		    wiretap_loop(Name, To, Pid, F);
		_ ->
		    Parent ! {self(), {error, register_failed}},
		    exit(error)
	    end
    end.

wiretap_loop(Name, To, Pid, F) ->
    receive
	{'EXIT', Pid, _} ->
	    wiretap_dropped(Name, To, F);
	{'EXIT', To, _} ->
	    wiretap_exit(Name, Pid);
	Msg ->
	    F(Msg, Pid, To),
	    wiretap_loop(Name, To, Pid, F)
    end.

%% note that the registered name might get stolen from the spy process,
%% e.g., by another active wiretap

wiretap_exit(Name, Pid) ->
    %% the receiver died - restore things and go away invisibly
    unlink(Pid),
    Self = self(),
    %% sadly, this is not atomic...
    case whereis(Name) of
	Self ->
	    catch unregister(Name),
	    catch register(Name, Pid);
	_ -> ok
    end,
    exit(normal).

%% if the real server goes away, make sure to unregister, and keep watch
%% in order to restart the wiretap when the server comes up again

wiretap_dropped(Name, To, F) ->
    Self = self(),
    case whereis(Name) of
	Self -> (catch unregister(Name));
	_ -> ok
    end,
    wiretap_watch(Name, To, F).

wiretap_watch(Name, To, F) ->
    receive
	{'EXIT', To, _} ->
	    exit(normal)
    after 1000 ->
	case whereis(Name) of
	    Pid when is_pid(Pid) ->
		%% this process will terminate after starting the
		%% new wiretap (even it that call fails)
		wiretap(Name, To, F),
		exit(normal);
	    _ -> 
		wiretap_watch(Name, To, F)
	end
    end.
