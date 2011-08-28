-module(test_html).

-compile(export_all).

ehtml_layout(Body) ->
  {ehtml, [
      "<!DOCTYPE html>",
      {html, [], [
          {head, [], [
              {link, [{rel, "stylesheet"}, {href, "/css/bootstrap-1.1.0.min.css"}]},
              {link, [{rel, "stylesheet"}, {href, "/css/app.css"}]}
            ]},
          {body, [], [
              {'div', [{class, "topbar"}], [
                  {'div', [{class, "fill"}], [
                      {'div', [{class, "container"}], [
                          {h3, [], {a, [{href, "/"}], "Erlang Test"}},
                          {ul, [], [
                              {li, [], {a, [{href, "/"}], "Overview"}}
                            ]},
                          login_menu()
                        ]}
                    ]}
                ]},
              {'div', [{class, "main container"}], Body}
            ]}
        ]}
    ]}.

login_menu() ->
  {ul, [{class, "nav secondary-nav"}], login_menu_items(gen_http:get_current_user_info()) }.

login_menu_items(unknown) ->
  [
    {li, [{class, "menu"}], {a, [{href, "/auth/login"}], "Login"}},
    {li, [{class, "menu"}], {a, [{href, "/auth/register"}], "Register"}}
  ];

login_menu_items({logged_in, _UserId, Login}) ->
  [
    {li, [{class, "menu"}], {a, [{href, "/profile"}], ["Logged in as: ", Login]}},
    {li, [{class, "menu"}], {a, [{href, "/auth/logout"}], ["Log out"]}}
  ].

add_error_class(Class, {ok, _Value}) -> Class;
add_error_class(Class, []) -> Class;
add_error_class(Class, _) -> [Class, " error"].

add_value(Attr, {ok, Value}) -> [{value, Value} | Attr];
add_value(Attr, {errors, Value, _Messages}) -> [{value, Value} | Attr];
add_value(Attr, []) -> Attr.

error_messages({ok, _Value}) -> [];
error_messages([]) -> [];
error_messages({errors, _Value, Messages}) ->
  {span, [{class, "help-inline"}], string:join(Messages, ", ")}.

ehtml_input(Type, Field, Label, Post) ->
  FieldName = atom_to_list(Field),
  FormField = gen_forms:get_field(Post, Field),
  {'div', [{class, add_error_class("clearfix", FormField)}], [
      {label, [{for, FieldName}], Label},
      {'div', [{class, "input"}], [
          {input, add_value([{id, FieldName}, {class, "xlarge"}, {size, 30}, {name, FieldName}, {type, Type}], FormField)},
          error_messages(FormField)
      ]}
  ]}.

ehtml_login_page(Messages) -> ehtml_layout(ehtml_login_form(Messages)).
ehtml_login_form(Post) ->
  LoginField = ehtml_input("text", login, "Login", Post),
  PasswordField = ehtml_input("password", password, "Password", Post),

  Form = {form, [{method, "POST"}, {action, "/auth/login"}], [
      {fieldset, [], [
          {legend, [], "Welcome back, please sign in."},
          LoginField,
          PasswordField,

          {'div', [{class, "actions"}], [
              {button, [{type, "submit"}, {class, "btn primary"}], "Login"}
          ]}
      ]}
    ]},

  {section, [{id, "form"}], [
      {'div', [{class, "page-header"}], [
          {h1, [], "Login"}
      ]},
      ehtml_row(Form)
  ]}.

ehtml_register_page(Messages) -> ehtml_layout(ehtml_register_form(Messages)).
ehtml_register_form(Post) ->
  LoginField = ehtml_input("text", login, "Login", Post),
  PasswordField = ehtml_input("password", password, "Password", Post),
  Password2Field = ehtml_input("password", password_confirmation, "Confirm Password", Post),

  Form = {form, [{method, "POST"}, {action, "/auth/register"}], [
      {fieldset, [], [
          LoginField,
          PasswordField,
          Password2Field,

          {'div', [{class, "actions"}], [
              {button, [{type, "submit"}, {class, "btn primary"}], "Register"}
          ]}
      ]}
    ]},

  {section, [{id, "form"}], [
      {'div', [{class, "page-header"}], [
          {h1, [], "Register"}
      ]},
      ehtml_row(Form)
  ]}.


ehtml_row(Body) ->
  {'div', [{class, "row"}], Body}.
