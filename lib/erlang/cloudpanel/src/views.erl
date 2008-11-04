-module (views).
-compile(export_all).

wrap_page(Name) ->
	AssetMatch = "(.*)([css|js])+",
	case regexp:first_match(Name, AssetMatch) of
		{match,_,_} ->			
			erlang:binary_to_list(read_asset(Name));
		nomatch ->
			wrap(read_page(Name));
		{_, _} ->
			io:format("Error: ~p~n", [Name]),
			"error"
	end.

json(Content) ->
	mochijson2:encode(Content).

wrap(Content) ->
	Header = read_partial("header"),
	Menu = read_partial("menu"),
	Footer = read_partial("footer"),
	Out = erlang:list_to_binary([Header, Menu, protect(Content), Footer]),
	erlang:binary_to_list(Out).

read_page(Name) ->
	{_, Cont} = file:read_file("priv/www/pages/"++Name++".html"),
	protect(Cont).
read_partial(Name) ->
	{_, Cont} = file:read_file("priv/www/partials/"++Name++".html"),
	protect(Cont).
read_asset(Name) ->
	{_, Cont} = file:read_file("priv/www/"++Name),
	protect(Cont).

protect(Content) ->
	case Content of
		enoent ->
			read_partial("error");
		[] ->
			read_partial("error");
		_ ->
			Content
	end.