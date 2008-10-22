%%%***************************************
%%% 
%%% PoolParty node-server
%%% Author: Ari Lerner <ari.the.lerner@gmail.com>
%%% 
%%% Description: This server runs on the poolparty nodes
%%% 
%%%***************************************

% The name of our module
-module (pm_node).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

% We are using the gen_server behaviour
-behaviour (gen_server).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Client function definitions
-export ([start_link/0, stop/0]).
-export ([get_load_for_type/1]).

% Client Function API Calls

% Get the load for the type sent...
get_load_for_type(Type) ->
	String = string:concat(". /etc/profile && server-get-load -m ",Type),
	{os:cmd(String)}.

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

% Gen server callbacks
% Sends the response and state back
init([]) -> 
	process_flag(trap_exit, true),
	io:format("~p starting ~n", [?MODULE]),
	{ok, 0}.

% Handle synchronous messages
% Signature:
%  handle_call(_Request, _From, State) ->
% 	{reply, ignored, State} 
% 
% Gets the load on the server
handle_call({load, Type}, From, State) ->
	spawn(?MODULE, get_load_for_type, [From, Type]),
	{noreply, State};
handle_call(_Request, _From, State) -> % The catch-all
	{reply, ignored, State}.

% Handle asynchronous messages
handle_cast(stop, State) ->
	{stop, normal, State};	
handle_cast(_Msg, State) ->
	{noreply, State}.

% Handle other messages	
handle_info(_Info, State) ->
	io:format("Info message received from: ~p~n", [_Info]),
	{noreply, State}.

% Exit
terminate(_Reason, State) ->
	io:format("[~p] Server is stopping...~n", [?MODULE]),
	{ok, State}.

% If the code changes
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.