-module (pm_strings).
-include_lib("../include/defines.hrl").
-compile(export_all).

string_join(Items, Sep) ->
    lists:flatten(lists:reverse(string_join1(Items, Sep, []))).

string_join1([Head | []], _Sep, Acc) ->
    [Head | Acc];
string_join1([Head | Tail], Sep, Acc) ->
    string_join1(Tail, Sep, [Sep, Head | Acc]).