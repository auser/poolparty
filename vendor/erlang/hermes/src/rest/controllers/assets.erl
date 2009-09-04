-module (assets).
-export ([get/1]).

-define (WONDERLAND_DIR, "web/wonderland/assets/").

get(["css", Path]) -> {"text/css", read_css(Path)};
get(["js", Path]) -> {"application/javascript", read_js(Path)};
get(["images", Path]) -> {"application/binary", read_image(Path)};
get(["templates", Path]) -> {"application/binary", read_template(Path)};
get(Path) -> {"text/html", "unknown assets path: " ++ Path}.

read_css(Name) ->
  {ok, Contents} = file:read_file(?WONDERLAND_DIR ++ "css/" ++ Name),
  Contents.

read_js(Name) ->
  {ok, Contents} = file:read_file(?WONDERLAND_DIR ++ "js/" ++ Name),
  Contents.

read_image(Name) ->
  {ok, Contents} = file:read_file(?WONDERLAND_DIR ++ "images/" ++ Name),
  Contents.
  
read_template(Name) ->
  {ok, Contents} = file:read_file(?WONDERLAND_DIR ++ "templates/" ++ Name),
  Contents.