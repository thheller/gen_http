-module(gen_http).

-export([
    enter/2,
    get_current_user/0,
    get_current_login/0,
    get_current_user_info/0,
    get_session_value/1,
    get_session_value/2,
    set_session_value/2,
    set_current_user/2,
    reset_session/0
  ]).

-define(SESSION_KEY, gen_http_session).
-define(SESSION_DICT, gen_http_session_dict).
-define(SESSION_CHANGED, gen_http_session_changed).

-include("yaws_api.hrl").

-record(session, {user = unknown, login = "", data = []}).

enter(Module, A) ->
  Req = A#arg.req,
  {abs_path, Path} = Req#http_request.path,

  ConvertedPath = string:tokens(A#arg.appmoddata, "/"),

  #session{user = CurrentUser} = CurrentSession = get_session(A),

  set_session(CurrentSession),

  try
    Result = Module:process(CurrentUser, Req#http_request.method, ConvertedPath, A),

    CookieHeader = case get(?SESSION_CHANGED) of
      undefined ->
        [];
      true ->
        make_session_cookie(A)
    end,
    [CookieHeader, Result]

  after
    cleanup()
  end.


get_session(A) ->
  case yaws_api:find_cookie_val("session", A) of
    [] ->
      #session{};

    Cookie ->
      case decode_cookie_value(Cookie, cookie_secret(A)) of
        invalid ->
          #session{};

        Session ->
          Session
      end
  end.

set_session(#session{data = SessionData} = CurrentSession) ->
  put(?SESSION_KEY, CurrentSession),
  put(?SESSION_DICT, dict:from_list(SessionData)).


cleanup() ->
  erase(?SESSION_KEY),
  erase(?SESSION_DICT),
  erase(?SESSION_CHANGED).

reset_session() ->
  % I could delete cookie, or just set it to an empty one?
  % making it empty is easier for now
  set_current_user(unknown, ""),
  put(?SESSION_DICT, dict:new()),
  session_changed().

session_changed() ->
  put(?SESSION_CHANGED, true).

get_current_user() ->
  #session{user = User} = get(?SESSION_KEY),
  User.

get_current_login() ->
  #session{login = Login} = get(?SESSION_KEY),
  Login.

get_current_user_info() ->
  case get(?SESSION_KEY) of
    #session{user = unknown} ->
      unknown;
    #session{user = UserId, login = Login} ->
      {logged_in, UserId, Login}
  end.

set_current_user(UserId, Login) ->
  Session = get(?SESSION_KEY),
  NewSession = Session#session{user = UserId, login = Login},
  put(?SESSION_KEY, NewSession),
  session_changed().

set_session_value(Key, Value) when is_atom(Key) ->
  Dict = get(?SESSION_DICT),
  NewDict = dict:store(Key, Value, Dict),
  put(?SESSION_DICT, NewDict),
  session_changed().

get_session_value(Key) ->
  Dict = get(?SESSION_DICT),
  dict:find(Key, Dict).

get_session_value(Key, Default) ->
  case get_session_value(Key) of
    error ->
      {ok, Default};

    Other ->
      Other
  end.

make_session_cookie(A) ->
  Session = get(?SESSION_KEY),
  Dict = get(?SESSION_DICT),
  UpdatedSession = Session#session{data = dict:to_list(Dict)},
  make_signed_cookie(A, UpdatedSession).

% signed cookies cannot be changed by the user, so we can trust them
% MUST HAVE A GOOD "secret"
make_signed_cookie(A, Term) ->
  CookieName = atom_to_list(element(1, Term)),
  CookieValue = encode_cookie_value(Term, cookie_secret(A)),
  {header, {set_cookie, [CookieName, "=", CookieValue, "; HttpOnly; Path=/;"]}}. % should add "; secure;"

cookie_secret(A) -> 
  "blubb".

encode_cookie_value(Record, Secret) ->
  Bin = term_to_binary(Record),
  Base64 = base64:encode(Bin),
  Mac = crypto:sha_mac(Secret, Base64),
  iolist_to_binary([string_mac(Mac), ".", Base64]).

decode_cookie_value(List, Secret) when is_list(List) ->
  decode_cookie_value(iolist_to_binary(List), Secret);

decode_cookie_value(<<Str:27/binary, ".", Base64/binary>>, Secret) ->
  Test = crypto:sha_mac(Secret, Base64),
  safe_decode(binary_to_list(Str), string_mac(Test), Base64);

decode_cookie_value(_, _) -> invalid.

safe_decode(H1, H2, Base64) when H1 == H2 ->
  binary_to_term(base64:decode(Base64));

safe_decode(_H1, _H2, _Base64) ->
  invalid.

% using this cause its 27 bytes, instead of 40 (optimization)
string_mac(<<C:160/big-unsigned-integer>>) ->
  riak_core_util:integer_to_list(C, 62).

% http://www.enchantedage.com/hex-format-hash-for-md5-sha1-sha256-and-sha512
hexstring(<<C:160/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~40.16.0b", [C])).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test() ->
  TestSecret = "blubb",
  Input = {hello, world, [], 1},
  Encoded = encode_cookie_value(Input, TestSecret),
  Decoded = decode_cookie_value(Encoded, TestSecret),
  invalid = decode_cookie_value(Encoded, "other secret"),
  ?assertEqual(Input, Decoded).

session_test() ->
  Session = #session{user = blubb, login = bla, data = [{hello, "world"}]},
  set_session(Session),

  Hello = get_session_value(hello),
  ?assertEqual({ok, "world"}, Hello),

  Hello2 = get_session_value(hello, "default"),
  ?assertEqual({ok, "world"}, Hello2),

  undefined = get(?SESSION_CHANGED),
  set_session_value(hello, "blubb"),
  true = get(?SESSION_CHANGED),

  Hello3 = get_session_value(hello),
  ?assertEqual({ok, "blubb"}, Hello3),

  Missing = get_session_value(missing),
  ?assertEqual(error, Missing),

  Default = get_session_value(missing, "default"),
  ?assertEqual({ok, "default"}, Default),

  cleanup().
  
-endif.

