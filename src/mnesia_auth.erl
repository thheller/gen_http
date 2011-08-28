-module(mnesia_auth).


-export([init_db/0, create/3, create/2, login/2, update_password/3]).

-record(user, {id, login, password, created_at}).

-include_lib("eunit/include/eunit.hrl").

init_db() ->
  mnesia:create_table(users, [
      {type, set},
      {index, [login]},
      {record_name, user},
      {attributes, record_info(fields, user)}
    ]).

generate_id() ->
  riak_core_util:unique_id_62().

create(_Login, Password, Password2) when Password /= Password2 ->
  password_mismatch;

create(Login, Password, _) ->
  create(Login, Password).

create(Login, Password) ->
  {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:index_read(users, Login, login) of
          [] ->
            {ok, Salt} = bcrypt:gen_salt(),
            {ok, PasswordHash} = bcrypt:hashpw(Password, Salt),

            User = #user{
              id = generate_id(),
              login = Login,
              password = PasswordHash,
              created_at = riak_core_util:moment()
            },
            ok = mnesia:write(users, User, write),
            {ok, User#user.id};

          _Taken ->
            login_taken

        end
    end),

  Result.

login(Login, Password) ->
  {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:index_read(users, Login, login) of
          [] ->
            not_found;

          [User] ->
            check_password(User, Password)
        end
    end),
  Result.

update_password(UserId, OldPassword, NewPassword) ->
  {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:read(users, UserId, read) of
          [] ->
            not_found;

          [User] ->
            case check_password(User, OldPassword) of
              {ok, _} ->
                {ok, Salt} = bcrypt:gen_salt(),
                {ok, PasswordHash} = bcrypt:hashpw(NewPassword, Salt),

                UpdatedUser = User#user{password = PasswordHash},
                ok = mnesia:write(users, UpdatedUser, write),
                ok;

              invalid_password ->
                invalid_password
            end
        end
    end),
  Result.

check_password(#user{id = UserId, password = HashedPw} = _User, InputPw) ->
  case bcrypt:hashpw(InputPw, HashedPw) of 
    {ok, HashedPw} ->
      {ok, UserId};

    _ ->
      invalid_password
  end.


-ifdef(TEST).

start_mnesia() ->
  crypto:start(),
  bcrypt:start(),
  mnesia:start(),

  case init_db() of 
    {atomic, ok} ->
      ok;

    {aborted, {already_exists, users}} ->
      ok
  end.

mnesia_test() ->
  start_mnesia(),

  {ok, UserId} = create("Hello", "World"),
  {ok, UserId} = login("Hello", "World"),

  not_found = login("World", "hello"),

  invalid_password = login("Hello", "test"),
  invalid_password = update_password(UserId, "blubb", "test"),

  login_taken = create("Hello", "test"),

  ok = update_password(UserId, "World", "blubb"),

  invalid_password = login("Hello", "World"),
  {ok, UserId} = login("Hello", "blubb"),

  password_mismatch = create("test", "test", "nomatch"),
  {ok, _} = create("test", "test", "test").


-endif.
