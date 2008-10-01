%%%***************************************
%%% 
%%% PoolParty node-server
%%% Author: Ari Lerner <ari.the.lerner@gmail.com>
%%% 
%%% Description: This server runs on the poolparty nodes
%%% 
%%%***************************************

% The name of our module
-module (eb_server).
% We are using the gen_server behaviour
-behaviour (gen_server).

% API methods
-export ([start_link/0]).

% gen_server callbacks
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Define the structure of the tuple state
-record (state, {}).
-define (SERVER, ?MODULE).

% API Methods
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
