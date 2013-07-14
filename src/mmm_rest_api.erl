-module(mmm_rest_api).

-include("mmm_rest_api_records.hrl").

-export([get_servers/3]).

get_servers(User, Pass, Url) ->
    Method = get,
    UserB = asciify(User),
    PassB = asciify(Pass),
    Delim = ":",
    AuthToken = UserB ++ Delim ++ PassB,
    Base64 = base64:encode_to_string(AuthToken),
    AuthKey = "Authorization",
    AuthType = "Basic ",
    AuthValue = AuthType ++ Base64,
    Headers = [{AuthKey, AuthValue}],
    Request = {Url, Headers},
    Response = httpc:request(Method, Request, [], []),

    Json = case Response of
        {ok, {{_, 200, _}, Bdy}} -> mochijson2:decode(Bdy);
        {ok, {{_, 200, _}, _, Bdy}} -> mochijson2:decode(Bdy);
        _ -> []
    end,

    lists:map(fun(E) -> json_parse_server(E) end, Json).

json_parse_server({struct, Srv}) ->
    Port = proplists:get_value(<<"port">>, Srv),
    Status = proplists:get_value(<<"type">>, Srv),
    Players = proplists:get_value(<<"players">>, Srv),
    #server{
        port = Port,
        status = binary_to_list(Status),
        players = lists:map(fun(E) -> binary_to_list(E) end, Players),
        empty = length(Players) == 0
    }.

asciify(Str) -> binary_to_list(unicode:characters_to_binary(Str, utf8, latin1)).
