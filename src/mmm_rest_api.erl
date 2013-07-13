-module(mmm_rest_api).

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

    case Response of
        {ok, {_, Json}} -> mochijson2:decode(Json);
        {ok, {_, _, Json}} -> mochijson2:decode(Json);
        _ -> null
    end.

asciify(Str) -> binary_to_list(unicode:characters_to_binary(Str, utf8, latin1)).
