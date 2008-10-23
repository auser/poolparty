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
%% $Id: eunit_serial.erl 249 2008-05-11 20:06:45Z rcarlsson $ 
%%
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Event serializing process which works as an adapter and
%% multiplexer for "supervisor" processes

-module(eunit_serial).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/1]).

%% Notes:
%% * Due to concurrency, there are no guarantees that we will receive
%% all status messages for the items within a group before we receive
%% the 'end' message of the group itself.
%% 
%% * A cancelling event may arrive at any time, and may concern items we
%% are not yet expecting (if tests are executed in parallel), or may
%% concern not only the current item but possibly a group ancestor of
%% the current item (as in the case of a group timeout).
%% 
%% * It is not possible to use selective receive to extract only those
%% cancelling messages that affect the current item and its parents;
%% basically, because we cannot have a dynamically computed prefix as a
%% pattern in a receive. Hence, we must extract each cancelling event as
%% it arrives and keep track of them separately.
%% 
%% * Before we wait for a new item, we must check whether it (and thus
%% also all its subitems, if any) is already cancelled.
%% 
%% * When a new cancelling event arrives, we must either store it for
%% future use, and/or cancel the current item and possibly one or more
%% of its parent groups.

-record(state, {listeners,
		cancelled = eunit_lib:trie_new(),
		messages = dict:new()}).

start(Pids) ->
    spawn(fun () -> serializer(Pids) end).

serializer(Pids) ->
    St = #state{listeners = sets:from_list(Pids),
		cancelled = eunit_lib:trie_new(),
		messages = dict:new()},
    item([], none, none, St),
    exit(normal).

item(Id, ParentId, N0, St0) ->
    case wait(Id, 'begin', ParentId, N0, St0) of
	{none, St1} ->
	    {true, St1};
	{{cancel, Done, undefined}, St1} ->
	    {Done, cast({status, Id, {cancel, undefined}}, St1)};
	{{cancel, Done, Msg}, St1} ->
	    {Done, cast(Msg, St1)};
	{{ok, Msg}, St1} ->
	    %%?debugVal({got_begin, Id, Msg}),
	    cast(Msg, St1),
	    St2 = case Msg of
		      {status, _, {progress, 'begin', group}} ->
			  items(Id, 0, St1);
		      _ -> St1
		  end,
	    case wait(Id, 'end', ParentId, N0, St2) of
		{{cancel, Done, undefined}, St3} ->
		    {Done, cast({status, Id, {cancel, undefined}}, St3)};
		{{cancel, Done, Msg1}, St3} ->
		    {Done, cast(Msg1, St3)};
		{{ok, Msg1}, St3} ->
		    %%?debugVal({got_end, Id, Msg1}),
		    {false, cast(Msg1, St3)}
	    end
    end.

items(ParentId, N0, St) ->
    N = N0 + 1,
    case item(ParentId ++ [N], ParentId, N0, St) of
	{false, St1} ->
	    items(ParentId, N, St1);
	{true, St1} ->
	    St1
    end.

cast(M, St) ->
    sets:fold(fun (L, M) -> L ! M end, M, St#state.listeners),
    St.

wait(Id, Type, ParentId, N0, St) ->
    %%?debugVal({wait, Id, Type}),
    case check_cancelled(Id, St) of
	no ->
	    case recall(Id, St) of
		undefined ->
		    wait_1(Id, Type, ParentId, N0, St);
		Msg ->
		    {{ok, Msg}, forget(Id, St)}
	    end;
	Why ->
	    %%?debugVal({cancelled, Why, Id, ParentId}),
	    Done = (Why =:= prefix),
	    {{cancel, Done, recall(Id, St)}, forget(Id, St)}
    end.

wait_1(Id, Type, ParentId, N0, St) ->
    receive
	{status, Id, {progress, Type, _}}=Msg ->
	    %%?debugVal({Type, ParentId, Id}),
	    {{ok, Msg}, St};
	{status,ParentId,{progress,'end',{N0,_,_}}}=Msg ->
	    %%?debugVal({end_group, ParentId, Id}),
	    {none, remember(ParentId, Msg, St)};
	{status, SomeId, {cancel, _Cause}}=Msg ->
	    %%?debugVal({got_cancel, SomeId, ParentId, Id}),
	    St1 = set_cancelled(SomeId, Msg, St),
	    wait(Id, Type, ParentId, N0, St1)
    end.

set_cancelled(Id, Msg, St0) ->
    St = remember(Id, Msg, St0),
    St#state{cancelled = eunit_lib:trie_store(Id, St0#state.cancelled)}.

check_cancelled(Id, St) ->
    eunit_lib:trie_match(Id, St#state.cancelled).

remember(Id, Msg, St) ->
    St#state{messages = dict:store(Id, Msg, St#state.messages)}.

forget(Id, St) ->
    %% this is just to enable garbage collection of old messages
    St#state{messages = dict:store(Id, undefined, St#state.messages)}.

recall(Id, St) ->
    case dict:find(Id, St#state.messages) of
	{ok, Msg} -> Msg;
	error -> undefined
    end.
