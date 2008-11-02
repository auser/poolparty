%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for chat.

-module (cloudpanel_web).
-author('Ari Lerner <ari.lerner@citrusbyte.com>').

-export([start/1, stop/0, loop/2]).

-define(TIMEOUT, 20000).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

room(Users) ->
    receive
        {From, subscribe} ->
            From ! subscribed,
            room([From | Users]);
        {From, unsubscribe} ->
            From ! unsubscribed,
            room(Users -- [From]);
        {From, post, Message} ->
            From ! posted,
            lists:foreach(fun(User) ->
                    % broadcast the message
                    User ! Message
                end, Users),
            room([]);
        _Any ->
            room(Users)
    end.

get_the_room() ->
    % does the room exists?
    Pid = whereis(theroom),
    if
        is_pid(Pid) ->
            % yup
            Pid;
        true ->
            % create it
            NewPid = spawn(fun() ->
                room([])
            end),
            register(theroom, NewPid),
            NewPid
    end.

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "chat" ->
                    Room = get_the_room(),
                    Room ! {self(), subscribe},
                    receive
                        subscribed ->
                            % subscription is ok
                            % now wait for a message
                            receive
                                Message ->
                                    {Type, Message} = {ok, Message}
                            after ?TIMEOUT ->
                                % we waited too long
                                {Type, Message} = {error, <<"timeout">>}
                            end
                    after 1000 ->
                        % subscription failed on time
                        {Type, Message} = {error, <<"timeout">>}
                    end,

                    case Type of
                        error ->
                            % we need to unsubscribe from the room
                            % because it failed somewhere
                            Room ! {self(), unsubscribe},
                            receive
                                unsubscribed ->
                                    % unsubscribed
                                    ok
                            after 1000 ->
                                % do not wait too long
                                ok
                            end;
                        ok ->
                            % everything went fine
                            ok
                    end,
                    
                    % send back the JSON message
                    Req:ok({"text/javascript", mochijson2:encode({
                            struct, [
                                {Type, Message}
                            ]
                        })
                    });
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                "chat" ->
                    Data = Req:parse_post(),
                    Room = get_the_room(),
                    % post
                    Room ! {self(), post, list_to_binary(proplists:get_value("message", Data))},
                    receive
                        posted ->
                            % posted
                            Body = {ok, <<"posted">>}
                    after 1000 ->
                        % something went wrong
                        Body = {error, <<"timeout">>}
                    end,
                    
                    % send back the JSON message
                    Req:ok({"text/javascript", mochijson2:encode({
                            struct, [
                                Body
                            ]
                        })
                    });
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.