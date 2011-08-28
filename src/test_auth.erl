-module(test_auth).

-export([process/4]).

-define(LOG, fun(Term) -> io:format("~p:~p - ~p.~n", [?MODULE, ?LINE, Term]) end).

login_field_spec() ->
  {login, string, [presence, {min_length, 3}], []}.

password_field_spec(Name) ->
  {Name, string, [presence, {min_length, 6}], []}.

login_form_spec() ->
  [ login_field_spec(), password_field_spec(password) ].

register_form_spec() ->
  [
    login_field_spec(),
    password_field_spec(password),
    password_field_spec(password_confirmation)
  ].

process(unknown, 'GET', ["login"], Req) ->
  test_html:ehtml_login_page([]);

% already logged in, no point in displaying login page (or force logout?)
process(UserId, 'GET', ["login"], Req) ->
  {redirect_local, "/"};

process(_, 'POST', ["login"], Req) ->
  case gen_forms:validate(login_form_spec(), Req) of
    {ok, Form} ->
      Login = gen_forms:get_value(Form, login),
      Password = gen_forms:get_value(Form, password),

      case mnesia_auth:login(Login, Password) of
        {ok, UserId} ->
          gen_http:set_current_user(UserId, Login),
          {redirect_local, "/"};

        not_found ->
          test_html:ehtml_login_page([{login, {errors, Login, ["does not exist"]}}, {password, {ok, Password}}]);

        invalid_password ->
          test_html:ehtml_login_page([{login, {errors, Login, ["failed"]}}])

      end;

    {errors, Messages} ->
      test_html:ehtml_login_page(Messages)

  end;

process(Who, _, ["register"], _Req) when Who =/= unknown ->
  {ehtml, "already registered"};

process(unknown, 'GET', ["register"], Req) ->
  test_html:ehtml_register_page([]);

process(unknown, 'POST', ["register"], Req) ->
  case gen_forms:validate(register_form_spec(), Req) of
    {ok, Form} ->
      Login = gen_forms:get_value(Form, login),
      Password = gen_forms:get_value(Form, password),
      Password2 = gen_forms:get_value(Form, password_confirmation),

      case mnesia_auth:create(Login, Password, Password2) of
        {ok, UserId} ->
          gen_http:set_current_user(UserId, Login),
          {redirect_local, "/"};

        login_taken ->
          test_html:ehtml_register_page([
              {login, {errors, Login, ["is already taken"]}},
              {password, {ok, Password}},
              {password_confirmation, {ok, Password2}}
            ]);
          
        password_mismatch ->
          test_html:ehtml_register_page([
              {login, {ok, Login}},
              {password, {errors, "", ["did not match"]}}
            ])

      end;

    {errors, Messages} ->
      test_html:ehtml_register_page(Messages)

  end;

process(unknown, _, ["logout"], Req) ->
  {redirect_local, "/"};

process(_, _, ["logout"], Req) ->
  gen_http:reset_session(),
  {redirect_local, "/"};

process(_Who, _Method, _Path, _Req) ->
  {redirect_local, "/"}.

