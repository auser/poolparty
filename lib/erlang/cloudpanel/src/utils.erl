-module (utils).
-compile(export_all).


subst(Template, Values) when is_list(Values) ->
    list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
