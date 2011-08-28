-module(gen_forms).

-include_lib("eunit/include/eunit.hrl").
-include("yaws_api.hrl").

-export([validate/2, get_value/2, get_field/2]).

get_field(Result, Field) ->
  case lists:keysearch(Field, 1, Result) of
    {value, {Field, Value}} ->
      Value;

    _ ->
      []
  end.

get_value(Result, Field) ->
  proplists:get_value(Field, Result, undefined).

validate(Spec, Input) when is_record(Input, arg) ->
  validate(Spec, yaws_api:parse_post(Input));

validate(Spec, Input) ->
  validate(Spec, Input, [], []).

validate([], _Input, Output, Errors) ->
  case lists:any(fun({_, {errors, _, _}}) -> true; (_) -> false end, Errors) of
    true ->
      {errors, Errors};

    false ->
      {ok, Output}
  end;

validate([FieldSpec | MoreSpec], Input, Output, Errors) ->
  case validate_field(FieldSpec, Input) of
    {ok, Name, Value} ->
      validate(MoreSpec, Input, [{Name, Value} | Output], [{Name, {ok, Value}} | Errors]);

    {invalid, Name, Value, Message} ->
      validate(MoreSpec, Input, Output, [{Name, {errors, Value, Message}} | Errors])
  end.

validate_field({Name, Type, Validations, Options}, Input) ->
  Value = get_field_value(Name, Input),

  case convert_value(Type, Value) of
    {ok, ConvertedValue} ->

      case validate_field_value(ConvertedValue, Validations) of
        ok ->
          {ok, Name, ConvertedValue};

        {invalid, Messages} ->
          {invalid, Name, Value, Messages}
      end;

    invalid ->
      {invalid, Name, Value, ["is invalid"]}

  end.

get_field_value(Name, Input) ->
  string:strip(proplists:get_value(atom_to_list(Name), Input, "")).

convert_value(string, Value) ->
  {ok, Value};

convert_value(int, Value) ->
  case (catch list_to_integer(Value)) of
    {'EXIT', _} ->
      invalid;

    Int ->
      {ok, Int}

  end;

convert_value(Type, Value) ->
  throw({unknown_type, Type}).

validate_field_value(Value, Validations) ->
  Results = [ apply_validation(Value, Validation) || Validation <- Validations ],

  case proplists:get_all_values(error, Results) of
    [] ->
      ok;

    Errors ->
      {invalid, Errors}

  end.


apply_validation(empty, presence) -> {error, "can't be empty"};
apply_validation("", presence) -> {error, "can't be empty"};
apply_validation(Value, {min_length, Len}) when length(Value) < Len -> {error, "is too short"};
apply_validation(_Value, {min_length, _Len}) -> ok;
apply_validation(_Value, presence) -> ok.



-ifdef(TEST).

example_spec() ->
  [
    {login, string, [presence], []},
    {password, string, [], [presence]}
  ].

example_post() ->
  [{"login", "hello"}, {"password", "omg"}].

example_result() ->
  {errors, [
      {field, {ok, value}},
      {field, {errors, value, messages}}
    ]}.

validate_presence_test() ->
  Form = [{field, string, [presence], []}],

  Errors = validate(Form, [{"field", ""}]),
  ?assertEqual(Errors, {errors, [{field, {errors, [], ["can't be empty"]}}]}),

  Errors2 = validate(Form, [{"field", "   "}]),
  ?assertEqual(Errors2, {errors, [{field, {errors, [], ["can't be empty"]}}]}),

  Ok = validate(Form, [{"field", "test"}]),
  ?assertEqual(Ok, {ok, [{field, "test"}]}).

validate_int_test() ->
  Form = [{field, int, [], []}],
  Result = validate(Form, [{"field", "123"}]),
  ?assertEqual({ok, [{field, 123}]}, Result),

  BadResult = validate(Form, [{"field", "as123"}]),
  ?assertEqual({errors, [{field, {errors, "as123", ["is invalid"]}}]}, BadResult).

get_field_test() ->
  Result = [{login, {errors, "", ["can't be empty"]}}],
  Field = get_field(Result, login),
  ?assertEqual({errors, [], ["can't be empty"]}, Field).

get_value_test() ->
  Result = [{login, "hi"}],
  Value = get_value(Result, login),
  ?assertEqual("hi", Value).

min_length_test() ->
  Form = [{field, string, [{min_length, 3}], []}],

  Result = validate(Form, [{"field", "a"}]),
  ?assertEqual({errors, [{field, {errors, "a", ["is too short"]}}]}, Result).

-endif.
