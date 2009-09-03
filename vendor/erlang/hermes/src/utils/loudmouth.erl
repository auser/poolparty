-module (loudmouth).

-export ([banner/2]).

%%--------------------------------------------------------------------
%% Function: banner (Lines) -> ok
%% Description: 
%% Header = "List"
%% Lines = [{"node", "1"},{"port", "8991"}]
%%--------------------------------------------------------------------
banner(Header, Lines) ->
  DescrLen = lists:max([length(K) || {K, _V} <- Lines]),
  Arr = lists:map(fun (T) -> 
      case T of        
        {K, V}  -> 
          lists:flatten(io_lib:fwrite("~-" ++ integer_to_list(DescrLen) ++ "s: ~p", [K, V]));
        E       -> E
      end
    end, Lines),
  
  LongestLine = lists:max([length(K) || K <- Arr]),
  HeaderLen = length(Header),  
  StripLen = round((LongestLine - (HeaderLen + 2))/2),
  
  Strips = string:copies("-", StripLen),
	io:format("~s ~s ~s~n", [Strips, Header, Strips]),
	
  lists:map(fun(Line) -> io:format("~s~n", [Line]) end, Arr),
  
  BannerStripes = string:copies("-", LongestLine),
	io:format("~s~n", [BannerStripes]),
  io:nl().  