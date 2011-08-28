-module(test_web).

-export([out/1, process/4]).

-export([start/0]).

start() ->
  mnesia:start(),
  mnesia_auth:init_db(),
  mnesia_auth:create("test", "test").

out(A) -> gen_http:enter(?MODULE, A).

% hijack all requests to /auth/* and let the test_auth mod handle them.
% that part should be pretty generic once I figure out some kind of
% template engine thingy
process(Who, Method, ["auth" | More], Req) ->
  test_auth:process(Who, Method, More, Req);

% Who is either unknown or a UserId (list)
process(Who, Method, Path, Req) ->
  Body = [
    {'div', [{class, "page-header"}], 
      {h1, [], [atom_to_list(Method), ": /", string:join(Path, "/")]}
    },
    {h2, [], "Hello World"},
    {p, [], "See test_web:process/4"}
  ],

  test_html:ehtml_layout(Body).


