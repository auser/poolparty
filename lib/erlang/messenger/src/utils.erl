-module (utils).
-compile(export_all).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Turn a list from
% [{"0.66"}, {"0.32"}, []] -> [0.66, 0.32]
convert_responses_to_int_list(L) ->
	Sum = lists:foldr( fun(Int, Sum) -> Int + Sum end, 0, [erlang:list_to_float(F) || {F} <- L] ),
	average_for_list(Sum, L).

% Private
% Get the average of the list
average_for_list(Num, L) ->
	case length(L) of
		0 ->
			0;
		_ ->
			Num / length(L)
	end.

% Tests
