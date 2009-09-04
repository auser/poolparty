-module (home).
-export ([get/1, post/2, put/2, delete/2]).

get([]) -> {"hello", <<"world">>};
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.

put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete(_Path, _Data) -> {"error", <<"unhandled">>}.